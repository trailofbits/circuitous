/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <glog/logging.h>
#include <llvm/IR/CFG.h>
#pragma clang diagnostic pop

#include <sstream>
#include <circuitous/Lifter/Context.hpp>

namespace circ {

  // TODO(lukas): Rename.
  // For each `Source` intrinsic collects all used `Sink` intrinsics.
  template< typename Source, typename Sink >
  struct ContextCollector {
    std::map< llvm::Value *, std::unordered_set< llvm::CallInst * > > data;
    llvm::Function *fn;


    ContextCollector(llvm::Function *fn_) : fn(fn_) {}

    void try_insert(llvm::CallInst *ctx, llvm::Instruction *current) {
      if (auto call = llvm::dyn_cast< llvm::CallInst >(current);
          call && Sink::IsIntrinsic(call->getCalledFunction())) {
        data[ctx].insert(call);
      }
    }

    void run(llvm::CallInst *ctx, llvm::Instruction *current) {
      try_insert(ctx, current);
      for (auto op : current->operand_values()) {
        if (auto as_inst = llvm::dyn_cast< llvm::Instruction >(op)) {
          run(ctx, as_inst);
        }
      }
    }

    auto get() {
      std::map< llvm::Value *, std::vector< llvm::CallInst * > > out;
      for (auto &[ctx, entries] : data) {
        out[ctx] = std::vector< llvm::CallInst * >(entries.begin(), entries.end());
      }
      return out;
    }

    auto run() {
      std::vector< llvm::CallInst * > sources;
      auto collect = [&](auto inst) { sources.push_back(inst); };
      intrinsics::VerifyInst::ForAllIn(fn, collect);

      for (auto source : sources) {
        run(source, source);
      }
      return get();
    }
  };

}  // namespace circ
