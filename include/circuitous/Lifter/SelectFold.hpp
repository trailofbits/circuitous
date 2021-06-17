/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Intrinsics.hpp>

#include <remill/BC/Util.h>

#include <unordered_map>
#include <vector>

namespace circuitous {

  struct SelectFolder : private intrinsics::Select {
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
          // If call is not complete we leave it for later when we called
          // all complete blueprints.
          if (!is_complete(select)) {
            continue;
          }

          bool found = false;
          for (std::size_t i = 0; i < blueprints.size() && !found; ++i) {
            if (are_compatible(select, blueprints[i])) {
              // They are compatible but current has more operands -> is more general
              if (select->getNumArgOperands() > blueprints[i]->getNumArgOperands()) {
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
          if (are_compatible(select, blueprint)) {
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

    void generate_blueprints(std::unordered_map<blueprint_t, uint64_t> stats) {
      auto generate = [&](auto blueprint, auto count) {
        llvm::IRBuilder<> ir(&*fn->begin()->begin());

        std::vector<llvm::Value *> args{ blueprint->arg_begin(), blueprint->arg_end() };
        auto [bitsize, type] = intrinsics::Select::ParseArgs(blueprint->getCalledFunction());
        for (std::size_t i = 0; i < count; ++i) {
          // Replace the selector with hint
          args[0] = intrinsics::make_hint(ir, args[0]->getType());
          auto select = intrinsics::make_select(ir, args, bitsize, type);
          generated[blueprint].emplace_back(llvm::dyn_cast<llvm::CallInst>(args[0]), select);
        }
      };


      for (auto [blueprint, count] : stats) {
        generate(blueprint, count);
      }
    }

    void use_blueprints() {
      auto coerce_selector = [](auto &ir, auto original, auto node) -> llvm::Value * {
        auto selector = intrinsics::Select::selector(original);
        auto [obits, _] = intrinsics::Select::ParseArgs(original->getCalledFunction());
        auto [nbits, _1] = intrinsics::Select::ParseArgs(node->getCalledFunction());
        if (obits == nbits) {
          return selector;
        }
        if (obits == 3 && nbits == 4) {
          return intrinsics::make_concat(ir, {ir.getInt1(0), selector});
        }
        LOG(FATAL) << "Cannot coerce selector";
      };

      for (auto &[ctx_, selects] : ctx_to_selects) {
        auto ctx = llvm::dyn_cast<llvm::CallInst>(ctx_);
        CHECK(ctx);

        std::vector<llvm::Value *> ctx_args{ctx->arg_begin(), ctx->arg_end()};
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
          ctx_args.push_back(intrinsics::make_hintcheck(ir, {selector, hint}));
          selects[i]->eraseFromParent();
        }

        // Now we need to update the verify_inst itself, as there are new hint checks
        llvm::IRBuilder<> ir(ctx);
        auto n_ctx = intrinsics::make_verify(ir, ctx_args);
        ctx->replaceAllUsesWith(n_ctx);
        ctx->eraseFromParent();
      }
    }

    void run() {
      gather_blueprints();
      generate_blueprints(assign_blueprints());
      use_blueprints();
    }
  };
} // namespace cirucituous