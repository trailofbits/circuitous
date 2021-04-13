/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/BaseLifter.hpp>

#include <remill/BC/Util.h>
#include <remill/BC/Optimizer.h>

#include "Flatten.hpp"
#include "InstructionFuzzer.hpp"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <glog/logging.h>
#include <llvm/IR/CallSite.h>
#include <llvm/Support/raw_os_ostream.h>
#pragma clang diagnostic pop

namespace circuitous {

  struct InstrinsicHandler {
    using functions_t = std::vector<llvm::Function *>;

    void HandleUsubSat(const functions_t &fns) {
      // TOOD(lukas): This most likely wants its own class
      // We want to lower some intrinsics that llvm optimizations introduced
      // `usub.sat` is a result of InstCombining.
      std::vector<llvm::Instruction *> calls;
      for (auto fn : fns) {
        for (auto &bb : *fn) {
          for (auto &inst : bb) {
            // NOTE(lukas): I opted to go inst by inst to avoid accidentaly
            //              modifying semantic functions we do not use.
            if (auto cs = llvm::CallSite(&inst)) {
              if (cs.isCall() &&
                  cs.getCalledFunction()->getIntrinsicID() == llvm::Intrinsic::usub_sat)
                  calls.push_back(cs.getInstruction());
            }
          }
        }
      }
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
    InstrinsicHandler().HandleUsubSat(fns);

    // Verify we did not broke anything
    remill::VerifyModule(module);
  }


  // Flatten all control flow into pure data-flow inside of a function.
  void FlattenControlFlow(llvm::Function *func, const remill::IntrinsicTable &intrinsics) {
    Flattener(func, intrinsics.error).Run();
  }

  void IdentifyImms(const remill::Arch::ArchPtr &arch, InstSelections &insts) {
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

} // namespace circuitous