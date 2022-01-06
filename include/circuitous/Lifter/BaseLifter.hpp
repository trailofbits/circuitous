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
#include <circuitous/Util/LLVMUtil.hpp>

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

namespace circ {

  void optimize_silently(
    const remill::Arch *arch, llvm::Module *module,
    const std::vector<llvm::Function *> &fns);

  // Flatten all control flow into pure data-flow inside of a function.
  void flatten_cfg(llvm::Function *func, const remill::IntrinsicTable &intrinsics);
  void fuzz_operands(const remill::Arch::ArchPtr &arch, InstSelections &insts);

  // Check that the decoding of a particular instruction results in position-
  // independent operands. It's possible that some operands have PC-relative
  // operands and have pre-calculated those values.
  bool IsDecodePositionIndependent(const remill::Arch::ArchPtr &arch,
                                   const remill::Instruction &inst);

  // Get the semantic name for an instruction encoding. We attach on the size of
  // the instruction in bytes as on x86, the iforms from XED don't guarantee us
  // the same size of bits.
  static std::string isel_name(const remill::Instruction &inst) {
    CHECK(15 >= inst.bytes.size());
    return inst.function + ("123456789abcdef"[inst.bytes.size()]);
  }

  struct has_ctx_ref {
    CtxRef ctx;
    has_ctx_ref(CtxRef ctx_) : ctx(ctx_) {}
  };

  struct Decoder : has_ctx_ref {
    using maybe_inst_t = std::optional< remill::Instruction >;
    using has_ctx_ref::has_ctx_ref;
    using self_ref = Decoder &;

    std::vector< InstructionSelection > grouped_insts;
    std::set< std::string > inst_bytes;
    std::unordered_map< std::string, size_t > isel_index;

    // Try to decode bytes in buff using arch from `ctx_ref`.
    auto decode(std::string_view buff) -> std::tuple< maybe_inst_t, std::string_view >
    {
      // Nothing to do anymore.
      if (buff.empty())
        return std::make_tuple( std::nullopt, buff );

      remill::Instruction inst;
      if (!ctx.arch()->DecodeInstruction(0, buff, inst) || !inst.IsValid()) {
        // Failed, move one byte.
        return decode(buff.substr(1));
      }
      // Success, return inst with shorter buffer.
      return std::make_tuple( std::make_optional(std::move(inst)),
                              buff.substr(inst.bytes.size()) );
    }

    // Recursively try to decode everything present, call `process` for each decoded inst.
    auto decode_all_(std::string_view buff) {
      while (!buff.empty())
      {

        auto [inst, rest] = decode(buff);
        if (inst)
          process(std::move(*inst));
        buff = rest;
      }
    }

    self_ref decode_all(llvm::StringRef buff) {
      std::string_view coerced(buff.data(), buff.size());
      decode_all_(coerced);
      return *this;
    }

    void process(remill::Instruction inst) {
      if (!IsDecodePositionIndependent(ctx._arch, inst)) {
        log_error() << "Skipping position-dependent instruction: " << inst.Serialize();
        return;
      }

      // Make sure the size of inst is not bigger than our assumption.
      CHECK(inst.bytes.size() * 8u <= kMaxNumInstBits);

      // Group the unique decoded instructions in terms of their ISELs, i.e. the
      // general semantic category of those instructions.
      if (auto [_, inserted] = inst_bytes.insert(inst.bytes); inserted) {
        auto &iclass = [&]() -> InstructionSelection & {

          // Is it present already?
          if (auto [it, inserted] = isel_index.emplace(isel_name(inst), grouped_insts.size());
              !inserted) {
            auto &iclass = grouped_insts[it->second];
            CHECK(inst.bytes.size() == iclass.instructions.back().bytes.size());
            return iclass;
          }
          // We need to create it -- `isel_index` was already update in the `if`,
          return grouped_insts.emplace_back();
        }();

        iclass.PartialAdd(std::move(inst),
                          InstructionSelection::RemillBytesToEncoding(inst.bytes));
      }
    }

    // Take all decoded instructions.
    auto take() { return std::move( grouped_insts ); }
  };


  template< typename Impl >
  struct ILifter : has_ctx_ref {
    using has_ctx_ref::has_ctx_ref;

    void after_lift_opts(std::vector< llvm::Function * > &inst_funcs) {
      disable_opts(inst_funcs);

      remill::VerifyModule(ctx.module());
      optimize_silently(ctx.arch(), ctx.module(), inst_funcs);

      std::vector<llvm::Function *> reopt_funcs;
      for (auto func : inst_funcs) {
        if (func->size() == 1) {
          continue;  // Pure data-flow; doesn't need to be re-optimized.
        }

        reopt_funcs.push_back(func);

        remill::IntrinsicTable intrinsics(ctx.module());
        flatten_cfg(func, intrinsics);
      }
      check_unsupported_intrinscis(inst_funcs);
      // TOOD(lukas): Inline into the loop once the `optimize_silently` does
      //              only function verification (now does module).
      optimize_silently(ctx.arch(), ctx.module(), reopt_funcs);

      // We're done; make the instruction functions more amenable for inlining
      // and elimination.
      enable_opts(inst_funcs);
    }

    static std::string lifted_name(const std::string &bytes) {
      std::stringstream ss;
      ss << "inst_";
      for (std::size_t i = 0; i < bytes.size(); ++i) {
        ss << std::hex << static_cast<uint32_t>(bytes[i]);
      }
      return ss.str();
    }

    void check_unsupported_intrinscis(const std::vector< llvm::Function * > &fns)
    {
      bool found = false;
      // For reporting errors
      std::stringstream out;

      auto fn_name = [](llvm::Function *fn) -> std::string {
        if (!fn || !fn->hasName())
          return "(nullptr)";
        return fn->getName().str();
      };

      auto is_allowed = [&](llvm::Function *fn) {
        // Indirect call is not an intrinsics and all functions should have names
        // at this point.
        if (!fn || !fn->hasName())
          return false;

        static const std::unordered_set< std::string > allowed = {
          "__remill_atomic_begin",
          "__remill_atomic_end",
        };

        auto fn_name = fn->getName();
        // TODO(lukas): Pull this from whatever class is responsible for lowering.
        //              May be tricky since it may be a user of this class.
        if (allowed.count(fn_name.str()))
          return true;
        // Cannot handle atomics right now
        if (fn_name.contains("atomic"))
          return false;
        // Cannot handle any form of floats
        if (fn_name.contains("float"))
          return false;
        if (fn_name.contains("__remill_sync_hyper_call"))
          return false;
        // If something was missed, lifter will most likely crash and intrinsic can
        // be retroactively added here.
        return true;
      };

      auto check = [&](llvm::Function *fn) {
        out << fn->getName().str() << std::endl;
        for (auto &bb : *fn)
          for (auto &inst : bb)
            if (auto call = llvm::dyn_cast< llvm::CallInst >(&inst))
              if (!is_allowed(call->getCalledFunction())) {
                found = true;
                out << "\t" << fn_name(call->getCalledFunction()) << std::endl;
              }
      };

      for (auto fn : fns)
        check(fn);
      CHECK(!found) << out.str();
    }

    bool was_lifted_correctly(auto status, const remill::Instruction &inst) {
      if (status == remill::LiftStatus::kLiftedInstruction)
        return true;

      if (status == remill::LiftStatus::kLiftedUnsupportedInstruction)
        log_error() << "Missing semantics for instruction: " << inst.Serialize();
      else if (status == remill::LiftStatus::kLiftedInvalidInstruction)
        log_error() << "Invalid instruction: " << inst.Serialize();
      else
        UNREACHABLE() << "Instruction lifter ended with unexpected error: " << inst.Serialize();

      return false;
    }

    void lift(std::vector<InstructionSelection> &isels) {
      std::vector<llvm::Function *> inst_funcs;

      auto unique_id = 0;
      for (auto &group : isels) {
        for (auto i = 0ull; i < group.instructions.size(); ++i) {
          auto &inst = group.instructions[i];
          auto name = lifted_name(inst.bytes) + std::to_string(++unique_id);
          CHECK(!ctx.module()->getFunction(name));

          auto func = remill::DeclareLiftedFunction(ctx.module(), name);
          group.lifted_fns[i] = func;

          log_dbg() << inst.Serialize();
          log_dbg() << group.shadows[i].to_string();
          remill::CloneBlockFunctionInto(func);
          auto block = &func->getEntryBlock();

          Impl lifter(ctx.arch(), ctx.module());
          lifter.SupplyShadow(&group.shadows[i]);
          CHECK(func->size() == 1);
          auto status = lifter.LiftIntoBlock(inst, block, false);

          if (!was_lifted_correctly(status, inst)) {
            func->eraseFromParent();
            continue;
          }

          llvm::ReturnInst::Create(*ctx.llvm_ctx(), remill::LoadMemoryPointer(block), block);
          inst_funcs.push_back(func);
        }
      }
      check_unsupported_intrinscis(inst_funcs);
      after_lift_opts(inst_funcs);
    }
  };

  // Lifter responsible for producing the ISELs and lifting the instructions
  // using the `ILifter` (which is required to be stateless w.r.t to inst lifts).
  template< typename ILifter_ >
  struct BaseLifter : has_ctx_ref {
    using arch_ptr_t = Ctx::arch_ptr_t;
    using has_ctx_ref::has_ctx_ref;


    InstSelections Run(llvm::StringRef buff) {
      auto isels = decode(buff);
      log_info() << "Decoding done.";
      fuzz(isels);
      log_info() << "Fuzzing done.";
      lift(isels);
      log_info() << "Lifting done.";
      return isels;
    }

    void fuzz(InstSelections &insts) { circ::fuzz_operands(ctx._arch, insts); }

    // Decode all instructions in `buff` using `arch`. Group the instructions in
    // terms of a general semantic category/class.
    InstSelections decode(llvm::StringRef buff) { return Decoder(ctx).decode_all(buff).take(); }

    auto lift(auto &isels) { return ILifter< ILifter_ >(ctx).lift(isels); }
  };
} // namespace circ
