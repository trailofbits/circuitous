/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/BaseLifter.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <remill/BC/Compat/CallSite.h>
#include <remill/BC/Util.h>
#include <remill/BC/Optimizer.h>

#include "Flatten.hpp"
#include <circuitous/Fuzz/InstructionFuzzer.hpp>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <glog/logging.h>
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
#pragma clang diagnostic pop

namespace circ {

  struct InstrinsicHandler {
    using functions_t = std::vector<llvm::Function *>;
    using intrinsic_id = llvm::Intrinsic::ID;
    using intrinsic_map = std::map<intrinsic_id, std::vector<llvm::CallInst *>>;

    intrinsic_map Fetch(const functions_t &fns) {
      intrinsic_map out;
      std::vector<llvm::Instruction *> calls;
      for (auto fn : fns) {
        for (auto &bb : *fn) {
          for (auto &inst : bb) {
            // NOTE(lukas): I opted to go inst by inst to avoid accidentaly
            //              modifying semantic functions we do not use.
            auto cs = remill::compat::llvm::CallSite(&inst);
            if (!cs.isCall()) {
              continue;
            }
            auto id = cs.getCalledFunction()->getIntrinsicID();
            if (id == llvm::Intrinsic::usub_sat ||
                id == llvm::Intrinsic::fshr ||
                id == llvm::Intrinsic::fshl) {
              out[id].push_back(llvm::dyn_cast<llvm::CallInst>(cs.getInstruction()));
            }
          }
        }
      }
      return out;
    }

    void Lower(const std::vector<llvm::CallInst *> &calls) {
      if (calls.empty()) {
        return;
      }
      auto dl = calls[0]->getModule()->getDataLayout();
      llvm::IntrinsicLowering lower{dl};
      for (auto c : calls) {
        lower.LowerIntrinsicCall(c);
      }
    }

    void Lower(const functions_t &fns) {
      auto calls = Fetch(fns);
      HandleUsubSat(calls[llvm::Intrinsic::usub_sat]);
      HandleFSH_(calls[llvm::Intrinsic::fshr], [](auto &ir, auto a, auto b) {
        return ir.CreateLShr(a, b);
      }, [](auto &ir, auto from, auto size) {
        // Least significant bytes
        return irops::make< irops::ExtractRaw >(ir, from, 0, size);
      });
      HandleFSH_(calls[llvm::Intrinsic::fshl], [](auto &ir, auto a, auto b) {
        return ir.CreateShl(a, b);
      }, [](auto &ir, auto from, auto size){
        // Most significant bytes
        return irops::make< irops::ExtractRaw >(ir, from, size, size);
      });
    }

    llvm::Value *LowerFSH_(llvm::CallInst *call, auto make_shift, auto make_extract) {
      llvm::IRBuilder<> ir(call);
      auto &dl = call->getModule()->getDataLayout();

      std::vector<llvm::Value *> args { call->getArgOperand(1u), call->getArgOperand(0u) };

      // iN out = fshr(iN x, iN y, iZ z)
      // i(N * 2) x'y' = concat(x, y)
      auto full = irops::make< irops::Concat >(ir, args);

      auto size = static_cast<uint32_t>(
          dl.getTypeSizeInBits(call->getArgOperand(0u)->getType()));
      auto shift_size = static_cast<uint32_t>(
          dl.getTypeSizeInBits(call->getArgOperand(2u)->getType()));
      // z' = z % N
      auto shift_c = ir.CreateURem(call->getArgOperand(2u), ir.getIntN(shift_size, size));
      // shifted' = x'y' >> z'
      auto shifted = make_shift(ir, full, ir.CreateZExt(shift_c, full->getType()));
      // out' = extract.N.N*2(shifted')
      return make_extract(ir, shifted, size);
    }

    void HandleFSH_(const std::vector<llvm::CallInst *> &calls,
                    auto make_shift, auto make_extract)
    {
      for (auto call : calls) {
        auto nc = LowerFSH_(call, make_shift, make_extract);
        call->replaceAllUsesWith(nc);
        call->eraseFromParent();
      }
    }

    void HandleUsubSat(const std::vector<llvm::CallInst *> &calls) {
      for (auto inst : calls) {
        llvm::IRBuilder<> ir(inst);
        auto call = llvm::cast<llvm::CallInst>(inst);
        auto a = call->getOperand(0);
        auto b = call->getOperand(1);
        auto size = static_cast<uint32_t>(inst->getType()->getPrimitiveSizeInBits());
        // sub = a - b
        auto sub = ir.CreateSub(a, b);
        // select  = (a < b) ? 0 : a - b;
        auto flag = ir.CreateICmpULT(a, b);
        auto zero = ir.getIntN(size, 0);
        auto select = ir.CreateSelect(flag, zero, sub);
        // replace & erase
        call->replaceAllUsesWith(select);
        call->eraseFromParent();
      }
    }
  };

  void OptimizeSilently(const remill::Arch *arch, llvm::Module *module,
                        const std::vector<llvm::Function *> &fns) {
    // `remill::OptimizeModule` calls some transformation that pollute
    // output with some info, so we need to mute it.
    auto saved_threshold = module->getContext().getDiagnosticsHotnessThreshold();
    module->getContext().setDiagnosticsHotnessThreshold(1);
    remill::OptimizeModule(arch, module, fns);
    // Set the logging back to the original values.
    module->getContext().setDiagnosticsHotnessThreshold(saved_threshold);

    // Handle intrinsic which can be lowered && we do not have intrinsic for
    // them.
    InstrinsicHandler().Lower(fns);

    // Verify we did not broke anything
    remill::VerifyModule(module);
  }


  // Flatten all control flow into pure data-flow inside of a function.
  void flatten_cfg(llvm::Function *func, const remill::IntrinsicTable &intrinsics) {
    Flattener(func, intrinsics.error).Run();
  }

  void fuzz_operands(const remill::Arch::ArchPtr &arch, InstSelections &insts) {
    for (auto &inst : insts) {
      for (auto i = 0U; i < inst.instructions.size(); ++i) {
        LOG(INFO) << "Searching for immediate operands regions in:";
        LOG(INFO) << inst.instructions[i].Serialize();
        inst.shadows[i] = InstructionFuzzer{arch, inst.instructions[i]}.FuzzOps();
      }
    }
  }

  bool IsDecodePositionIndependent(const remill::Arch::ArchPtr &arch,
                                   const remill::Instruction &inst) {
    remill::Instruction copy;
    if (!arch->DecodeInstruction(inst.pc + 32, inst.bytes, copy) ||
        inst.operands.size() != copy.operands.size()) {
      return false;
    }

    for (auto i = 0u; i < inst.operands.size(); ++i) {
      if (inst.operands[i].Serialize() != copy.operands[i].Serialize()) {
        return false;
      }
    }

    return true;
  }

} // namespace circ
