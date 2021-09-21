/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/Util/LLVMUtil.hpp>
#include <circuitous/Lifter/DependencyVisitor.hpp>

#include <deque>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>

namespace circ::_dbg {

  struct CtxPrinter {
    using op_ctxs_t = CtxGatherer::op_ctxs_t;

    uint64_t ctx_count = 0;
    std::unordered_map< uint64_t, llvm::Value * > ctx_ids;
    llvm::Function *fn = nullptr;
    op_ctxs_t op_ctxs;
    std::stringstream ss;

    CtxPrinter(llvm::Function *fn_) : fn(fn_)
    {
      op_ctxs = CtxGatherer().run(fn);

      auto count_ctxs = [&](auto ctx) { ctx_ids[ctx_count] = ctx; ++ctx_count; };
      irops::VerifyInst::for_all_in(fn, count_ctxs);
      fmt();
    }


    void fmt() {
      CHECK(std::distance(fn->begin(), fn->end()) == 1);
      for (auto &bb : *fn)
        for (auto &inst : bb)
        {
          fmt(&inst);
        }
    }

    uint64_t prefix_size() {
      return 2ul + ctx_count + 2ul;
    }

    void generate_prefix() {
      ss << std::string(prefix_size(), ' ');
    }

    void generate_prefix(llvm::Instruction *inst) {
      if (inst->getNumOperands() == 0) {
        ss << "L ";
      } else {
        ss << "  ";
      }
      auto &ctxs = op_ctxs[inst];
      for (std::size_t i = 0; i < ctx_count; ++i) {
        if (ctxs.count(ctx_ids[i])) {
          ss << i;
        } else {
          ss << " ";
        }
      }
      ss << "  ";
    }

    void fmt(llvm::Instruction *inst) {
      generate_prefix(inst);
      ss << dbg_dump(inst);
      ss << std::endl;
    }

    std::string get() { return ss.str(); }

  };

}// namespace circ::print
