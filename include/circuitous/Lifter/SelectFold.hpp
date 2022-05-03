/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Intrinsics.hpp>
#include <circuitous/Lifter/DependencyVisitor.hpp>
#include <circuitous/Support/Check.hpp>


#include <remill/BC/Util.h>

#include <set>
#include <vector>
#include <unordered_map>

namespace circ {

  struct SelectFolder {

    using blueprint_t = llvm::CallInst *;
    using ctx_to_selects_t = std::map<llvm::Value *, std::vector<llvm::CallInst *>>;

    ctx_to_selects_t ctx_to_selects;
    llvm::Function *fn;

    template<typename T>
    using val_to_T = std::unordered_map<llvm::Value *, T>;
    using used_blueprints_t = val_to_T<std::unordered_map<blueprint_t, uint64_t>>;

    used_blueprints_t used_blueprints;
    val_to_T<blueprint_t> select_to_blueprint;

    std::vector<blueprint_t> blueprints;

    // [ Hint, SelectN ]
    using node_t = std::tuple<llvm::CallInst *, llvm::CallInst *>;
    std::unordered_map<llvm::CallInst *, std::vector<node_t>> generated;

    SelectFolder(ctx_to_selects_t selects_, llvm::Function *fn_)
        : ctx_to_selects(std::move(selects_)), fn(fn_) {}

    void gather_blueprints() {
      for (auto &[_, selects] : ctx_to_selects) {
        for (auto select : selects) {
          // If call is not complete we leave it for later when we collected
          // all complete blueprints.
          if (!irops::Instance< irops::Select >(select).is_complete()) {
            continue;
          }

          bool found = false;
          for (std::size_t i = 0; i < blueprints.size() && !found; ++i) {
            if (irops::Instance< irops::Select >::are_compatible(select, blueprints[i])) {
              // They are compatible but current has more operands -> is more general
              if (select->arg_size() > blueprints[i]->arg_size()) {
                blueprints[i] = select;
              }
              found = true;
            }
          }

          if (!found) {
            blueprints.push_back(select);
          }
        }
      }
    }

    std::unordered_map<blueprint_t, uint64_t> assign_blueprints() {
      auto assign = [&](auto select) -> llvm::CallInst * {
        for (auto blueprint : blueprints) {
          if (irops::Instance< irops::Select >::is_compatible_with(select, blueprint)) {
            return blueprint;
          }
        }
        // This can be triggered since in the gather step we skipped all
        // selects that are incomplete.
        blueprints.push_back(select);
        return select;
      };

      std::unordered_map<blueprint_t, uint64_t> out;
      for (auto b : blueprints) {
        out[b] = 0;
      }

      for (auto &[ctx, selects] : ctx_to_selects) {
        std::unordered_map<blueprint_t, uint64_t> stats;
        for (auto select : selects) {
          auto blueprint = assign(select);
          select_to_blueprint[select] = blueprint;
          auto [count, _] = stats.try_emplace(blueprint, 0);
          ++count->second;
        }

        for (auto [blueprint, count] : stats) {
          out[blueprint] = std::max(out[blueprint], count);
        }
      }
      return out;
    }

    auto inst_distance(llvm::Instruction *a, llvm::Instruction *b) {
      check(a->getParent() == b->getParent());
      using bb_it = llvm::BasicBlock::iterator;
      return std::distance(bb_it(a), bb_it(b));
    }

    // Sometimes we need to move the select call further into the function
    // because domination issues - i.e. we try to emit it right after last
    // of its operands is defined.
    template<typename C, typename ...Ts>
    auto make_blueprint(C &args, Ts &&... ts) {
      auto fst = &*fn->begin()->begin();

      auto get_ip = [&]() {
        llvm::Instruction *out = fst;
        for (auto i = 1u; i < args.size(); ++i) {
          if (auto inst = llvm::dyn_cast<llvm::Instruction>(args[i])) {
            if (inst_distance(fst, out) < inst_distance(fst, inst)) {
              out = inst;
            }
          }
        }
        return out;
      };

      auto get_ir = [&](auto ip_) {
        if (!ip_) {
          return llvm::IRBuilder<>(&*fn->begin()->begin());
        }
        return llvm::IRBuilder<>(ip_->getNextNonDebugInstruction());
      };

      auto ir = get_ir(get_ip());
      args[0] = irops::make_leaf< irops::Advice >(ir, args[0]->getType());
      return irops::make< irops::Select >(ir, args, std::forward<Ts>(ts)...);
    }

    void generate_blueprints(std::unordered_map<blueprint_t, uint64_t> stats) {
      auto generate = [&](auto blueprint, auto count) {
        llvm::IRBuilder<> ir(&*fn->begin()->begin());

        std::vector<llvm::Value *> args{ blueprint->arg_begin(), blueprint->arg_end() };
        auto [bitsize, type] = irops::Select::parse_args(blueprint->getCalledFunction());
        for (std::size_t i = 0; i < count; ++i) {
          // Replace the selector with hint
          auto select = make_blueprint(args, bitsize, type);
          generated[blueprint].emplace_back(llvm::dyn_cast<llvm::CallInst>(args[0]), select);
        }
      };


      for (auto [blueprint, count] : stats) {
        generate(blueprint, count);
      }
    }

    void use_blueprints() {
      auto coerce_selector = [](auto &ir, auto original, auto node) -> llvm::Value * {
        auto selector = irops::Instance< irops::Select >(original).selector();
        auto [obits, _] = irops::Select::parse_args(original->getCalledFunction());
        auto [nbits, _1] = irops::Select::parse_args(node->getCalledFunction());
        if (obits == nbits) {
          return selector;
        }
        if (obits == 3 && nbits == 4) {
          return irops::make< irops::Concat >(ir, {ir.getInt1(0), selector});
        }
        uint32_t dbits_size = static_cast< uint32_t >(nbits - obits);
        return irops::make< irops::Concat >(ir, {ir.getIntN(dbits_size, 0), selector});

        unreachable() << "Cannot coerce selector:\n" << dbg_dump(selector)
                      << "\nof select:\n"
                      << dbg_dump(original)
                      << "\nWhere blueprint is:\n"
                      << dbg_dump(node);
      };

      for (auto &[ctx_, selects] : ctx_to_selects) {
        auto ctx = llvm::dyn_cast<llvm::CallInst>(ctx_);
        check(ctx) << remill::LLVMThingToString(ctx_);

        std::vector<llvm::Value *> ctx_args;
        std::unordered_map<blueprint_t, std::size_t> idxs;

        for (std::size_t i = 0; i < selects.size(); ++i) {
          if (selects[i]->hasNUses(0)) {
            selects[i]->eraseFromParent();
            continue;
          }
          // Get next blueprint to use
          auto blueprint = select_to_blueprint[selects[i]];
          auto [idx, _] = idxs.try_emplace(blueprint, 0);
          auto [hint, node] = generated[blueprint][idx->second++];

          llvm::IRBuilder<> ir(selects[i]);
          // Get selector that may be enlarged (to use N bit blueprint with M select
          // where N > M)
          auto selector = coerce_selector(ir, selects[i], node);
          selects[i]->replaceAllUsesWith(node);
          ctx_args.push_back(irops::make< irops::AdviceConstraint >(ir, {selector, hint}));
          selects[i]->eraseFromParent();
        }

        // Now we need to update the verify_inst itself, as there are new hint checks
        extend(ctx, ctx_args);
      }
    }

    void run() {
      gather_blueprints();
      generate_blueprints(assign_blueprints());
      use_blueprints();
    }
  };


  struct ACUnfolder {
    using op_ctxs_t = CtxGatherer::op_ctxs_t;

    llvm::Function *fn = nullptr;
    op_ctxs_t op_ctxs;

    ACUnfolder(llvm::Function *fn_) : fn(fn_), op_ctxs(CtxGatherer().run(fn_)) {}

    void run(llvm::CallInst *call) {
      auto ac = irops::Instance< irops::AdviceConstraint >(call);

      auto have_compatible_ctxs = [&](auto &use) {
        auto user = use.getUser();
        if (llvm::isa< llvm::CallInst >(user))
          return false;
        auto as_inst = llvm::dyn_cast< llvm::Instruction >(user);
        check(as_inst);
        return op_ctxs[as_inst] == op_ctxs[call];
      };

      ac.dynamic()->replaceUsesWithIf(ac.advice(), have_compatible_ctxs);
    }

    void run() {
      auto exec = [&](auto ac) { this->run(ac); };
      irops::AdviceConstraint::for_all_in(fn, exec);
    }
  };


  static inline void unfold_advice_constraints(llvm::Function *fn) {
    ACUnfolder(fn).run();
  }
} // namespace circuitous
