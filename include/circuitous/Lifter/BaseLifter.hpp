/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <cstdint>
#include <deque>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <circuitous/Lifter/Context.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/IntrinsicTable.h>
#include <remill/BC/Lifter.h>
#include <remill/BC/Optimizer.h>
#include <remill/BC/Util.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <glog/logging.h>
#include <llvm/IR/Module.h>
#pragma clang diagnostic pop

namespace circuitous {

  void OptimizeSilently(
    const remill::Arch *arch, llvm::Module *module,
    const std::vector<llvm::Function *> &fns);

  // Flatten all control flow into pure data-flow inside of a function.
  void FlattenControlFlow(llvm::Function *func, const remill::IntrinsicTable &intrinsics);
  void IdentifyImms(const remill::Arch::ArchPtr &arch, InstSelections &insts);

  // Check that the decoding of a particular instruction results in position-
  // independent operands. It's possible that some operands have PC-relative
  // operands and have pre-calculated those values.
  bool IsDecodePositionIndependent(const remill::Arch::ArchPtr &arch,
                                   const remill::Instruction &inst);

  // Lifter responsible for producing the ISELs and lifting the instructions
  // using the `ILifter` (which is required to be stateless w.r.t to inst lifts).
  template<typename ILifter>
  struct BaseLifter {
    using arch_ptr_t = Ctx::arch_ptr_t;
    CtxRef ctx;

    // The maximum number of bits needed to hold any instruction that can be
    // verified by this ciruit.
    uint64_t encoded_inst_size{0};
    bool reduce_imms = false;

    BaseLifter(CtxRef ctx_) : ctx(ctx_), reduce_imms(true) {}

    // Apply a callback `cb` to every instruction in the buffer `buff`.
    template <typename CB>
    void ForEachInstructionInBuffer(const arch_ptr_t &arch, llvm::StringRef buff, CB &&cb) {
      const auto max_inst_size = arch->MaxInstructionSize();

      remill::Instruction inst;
      for (size_t i = 0u, max_i = buff.size(); i < max_i; inst.Reset()) {
        auto next_i = std::min<std::size_t>(max_i, i + max_inst_size);
        std::string_view bytes(&(buff.data()[i]), next_i - i);

        if (!arch->DecodeInstruction(0, bytes, inst) || !inst.IsValid()) {
          LOG(ERROR) << "Unable to decode instruction at byte offset " << i;
          ++i;
        } else {
          i += inst.bytes.size();
          cb(std::move(inst));
        }
      }
    }

    InstSelections Run(llvm::StringRef buff) {
      auto isels = DecodeInstructions(buff);
      IdentifyImms(isels);
      LiftInstructions(isels);
      return isels;
    }

    // Get the semantic name for an instruction encoding. We attach on the size of
    // the instruction in bytes as on x86, the iforms from XED don't guarantee us
    // the same size of bits.
    static std::string IselName(const remill::Instruction &inst) {
      CHECK_GE(15, inst.bytes.size());
      return inst.function + ("123456789abcdef"[inst.bytes.size()]);
    }

    static std::string LiftedName(const std::string &bytes) {
      std::stringstream ss;
      ss << "inst_" << bytes;
      return ss.str();
    }

    static void DisableOpts(llvm::Function *func) {
      func->removeFnAttr(llvm::Attribute::InlineHint);
      func->removeFnAttr(llvm::Attribute::AlwaysInline);
      func->setLinkage(llvm::GlobalValue::ExternalLinkage);
      func->addFnAttr(llvm::Attribute::NoInline);
    }

    static void DisableOpts(const std::vector<llvm::Function *> &fns) {
      for (auto fn : fns) {
        DisableOpts(fn);
      }
    }

    static void EnableOpts(llvm::Function *func) {
      func->removeFnAttr(llvm::Attribute::InlineHint);
      func->removeFnAttr(llvm::Attribute::AlwaysInline);
      func->setLinkage(llvm::GlobalValue::ExternalLinkage);
      func->addFnAttr(llvm::Attribute::NoInline);
    }

    static void EnableOpts(const std::vector<llvm::Function *> &fns) {
      for (auto fn : fns) {
        EnableOpts(fn);
      }
    }

    // Decode all instructions in `buff` using `arch`. Group the instructions in
    // terms of a general semantic category/class.
    InstSelections DecodeInstructions(llvm::StringRef buff) {
      std::vector<InstructionSelection> grouped_insts;
      std::set<std::string> inst_bytes;
      std::unordered_map<std::string, size_t> isel_index;

      // Abort if `inst.bytes` are filled with `0`s.
      auto assert_non_zero = [](auto &inst) {
        for (auto b : inst.bytes) {
          if (b) {
            return;
          }
        }
        LOG(FATAL)
            << "Instructions whose machine code representation is all zeroes are "
            << "not permitted as they would invalidate the XOR-based checking of "
            << "encode verification checks";
      };

      ForEachInstructionInBuffer(ctx._arch, buff, [&](remill::Instruction inst) {
        // Check that bytes of the instruction are not all `0`s, we cannot deal
        // with that yet.
        assert_non_zero(inst);

        // It's likely that some of Remill's decoders implicitly put position-
        // dependent operands into the operands list, so try to catch that, warn
        // about them, and skip them.
        if (!IsDecodePositionIndependent(ctx._arch, inst)) {
          LOG(ERROR) << "Skipping position-dependent instruction: "
                    << inst.Serialize();
          return;
        }

        // Keep track of the maximum size.
        encoded_inst_size = std::max<uint64_t>(encoded_inst_size, inst.bytes.size() * 8u);
        // Make sure the size of inst is not bigger than our assumption.
        CHECK_LE(encoded_inst_size, kMaxNumInstBits);

        // Group the unique decoded instructions in terms of their ISELs, i.e. the
        // general semantic category of those instructions.
        if (auto [_, inserted] = inst_bytes.insert(inst.bytes); inserted) {

          auto &iclass = [&]() -> InstructionSelection & {
            // Make the identifier specific to the `inst.function` and its size.
            const auto isel = IselName(inst);

            // Is it present already?
            if (auto [it, inserted] = isel_index.emplace(isel, grouped_insts.size());
                !inserted) {
              auto &iclass = grouped_insts[it->second];
              CHECK_EQ(inst.bytes.size(), iclass.instructions.back().bytes.size());
              return iclass;
            }
            // We need to create it -- `isel_index` was already update in the `if`,
            return grouped_insts.emplace_back();
          }();

          iclass.PartialAdd(std::move(inst),
                            InstructionSelection::RemillBytesToEncoding(inst.bytes));
        }
      });

      return grouped_insts;
    }

      // Decode all instructions in `buff` using `arch`.
    void LiftInstructions(std::vector<InstructionSelection> &isels) {
      remill::IntrinsicTable intrinsics(ctx.module());
      std::vector<llvm::Function *> inst_funcs;

      for (auto &group : isels) {
        for (auto i = 0ull; i < group.instructions.size(); ++i) {
          auto &inst = group.instructions[i];

          auto func = remill::DeclareLiftedFunction(ctx.module(), LiftedName(inst.bytes));
          group.lifted_fns[i] = func;

          remill::CloneBlockFunctionInto(func);
          auto block = &func->getEntryBlock();

          ILifter lifter(ctx.arch(), intrinsics);
          lifter.SupplyShadow(&group.shadows[i]);
          auto status = lifter.LiftIntoBlock(inst, block, false);
          if (status == remill::LiftStatus::kLiftedInstruction) {
              llvm::ReturnInst::Create(*ctx.llvm_ctx(), remill::LoadMemoryPointer(block), block);
              inst_funcs.push_back(func);
              continue;
          }

          if (status == remill::LiftStatus::kLiftedUnsupportedInstruction) {
            LOG(ERROR) << "Missing semantics for instruction: "
                      << inst.Serialize();
          }
          if (status == remill::LiftStatus::kLiftedInvalidInstruction) {
              LOG(ERROR) << "Invalid instruction: " << inst.Serialize();
          }
          func->eraseFromParent();
        }
      }

      DisableOpts(inst_funcs);
      OptimizeSilently(ctx.arch(), ctx.module(), inst_funcs);

      std::vector<llvm::Function *> reopt_funcs;
      for (auto func : inst_funcs) {
        if (func->size() == 1) {
          continue;  // Pure data-flow; doesn't need to be re-optimized.
        }

        reopt_funcs.push_back(func);
        FlattenControlFlow(func, intrinsics);
      }

      // TOOD(lukas): Inline into the loop once the `OptimizeSilently` does
      //              only function verification (now does module).
      OptimizeSilently(ctx.arch(), ctx.module(), reopt_funcs);

      // We're done; make the instruction functions more amenable for inlining
      // and elimination.
      EnableOpts(inst_funcs);
    }

    void IdentifyImms(InstSelections &insts) {
      circuitous::IdentifyImms(ctx._arch, insts);
    }

  };
}