/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

 #pragma once

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <gflags/gflags.h>
#include <glog/logging.h>
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/IR/CallSite.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Codegen/IntrinsicLowering.h>
#pragma clang diagnostic pop

namespace circuitous {

struct Flattener {

  llvm::Function *func;
  // Function that generates errors
  llvm::Function *error;

  Flattener(llvm::Function *func_, llvm::Function *error_) :
    func(func_), error(error_)
  {}

  void Run() {
    auto module = func->getParent();
    auto &context = func->getContext();
    llvm::IRBuilder<> ctx_b(context);
    auto true_value = ctx_b.getTrue();
    auto false_value = ctx_b.getFalse();

    const auto entry_block = &(func->getEntryBlock());
    llvm::ReversePostOrderTraversal<llvm::BasicBlock *> it(entry_block);

    std::unordered_map<llvm::BasicBlock *, llvm::Value *> reaching_cond;
    std::unordered_map<llvm::BasicBlock *,
                      std::unordered_map<llvm::BasicBlock *, llvm::Value *>>
        pred_conds;

    const auto new_block =
        llvm::BasicBlock::Create(context, "", func, entry_block);

    std::vector<llvm::Instruction *> insts;
    std::vector<llvm::Instruction *> to_remove;
    std::vector<llvm::BasicBlock *> orig_blocks;
    std::vector<std::pair<llvm::ReturnInst *, llvm::Value *>> ret_vals;


    // TODO(lukas): Refactor this entire thing into a separate class
    std::map<llvm::SwitchInst *, std::map<llvm::BasicBlock *, llvm::Value *>> switch_conds;

    auto init_switch = [ & ](auto inst, auto &ir) {
      auto val = inst->getCondition();
      auto default_dst = inst->getDefaultDest();
      llvm::Value *default_cond = ir.getTrue();

      auto &conditions = switch_conds[inst];
      for (auto x : inst->cases()) {
        auto target_val = x.getCaseValue();
        auto dst = x.getCaseSuccessor();
        auto cond = ir.CreateICmpEQ(target_val, val);
        conditions[dst] = cond;
        default_cond = ir.CreateAnd(default_cond, ir.CreateNot(cond));
      }
      if (default_dst) {
        conditions[default_dst] = default_cond;
      }
    };

    auto get_switch = [ & ](auto inst, auto &ir) {
      if (!switch_conds.count(inst)) {
        init_switch(inst, ir);
      }
      return switch_conds[inst];
    };

    for (llvm::BasicBlock *block : it) {
      orig_blocks.push_back(block);
    }

    for (auto block : orig_blocks) {

      // The entry block is guaranteed to be reachable.
      if (block->hasNPredecessors(0)) {
        reaching_cond.emplace(block, true_value);
        pred_conds[block].emplace(nullptr, true_value);
        pred_conds[block].emplace(block, true_value);

      // Figure out the reaching conditions for this block, and express them as
      // data flow (i.e. instructions).
      } else {
        llvm::IRBuilder<> ir(new_block);
        llvm::Value *cond = false_value;

        for (auto pred_block : llvm::predecessors(block)) {
          const auto pred_cond = reaching_cond[pred_block];
          LOG_IF(FATAL, !pred_cond)
              << "Cycle in control-flow graphs are not handled";

          const auto pred_br =
              llvm::dyn_cast<llvm::BranchInst>(pred_block->getTerminator());

          const auto pred_switch =
              llvm::dyn_cast<llvm::SwitchInst>(pred_block->getTerminator());

          // Figure out the reaching condition for `block` coming through
          // `pred`.
          llvm::Value *edge_cond = pred_cond;
          if (pred_br && pred_br->isConditional()) {
            const auto true_succ = pred_br->getSuccessor(0);
            const auto false_succ = pred_br->getSuccessor(1);

            if (true_succ != false_succ) {
              edge_cond = pred_br->getCondition();
              if (true_succ == block) {
                edge_cond = ir.CreateAnd(pred_cond, edge_cond);
              } else {
                edge_cond = ir.CreateAnd(pred_cond, ir.CreateNot(edge_cond));
              }
            } else {
              edge_cond = pred_cond;
            }

          } else if (pred_switch) {
            edge_cond = get_switch(pred_switch, ir)[block];
            CHECK(edge_cond);
            edge_cond = ir.CreateAnd(pred_cond, edge_cond);
          }

          pred_conds[block].emplace(pred_block, edge_cond);
          cond = ir.CreateXor(cond, edge_cond);
        }

        reaching_cond.emplace(block, cond);
      }

      insts.clear();
      for (llvm::Instruction &inst : *block) {
        insts.push_back(&inst);
      }

      for (auto inst : insts) {
        if (auto phi = llvm::dyn_cast<llvm::PHINode>(inst)) {
          llvm::IRBuilder<> ir(new_block);

          const auto num_preds = phi->getNumIncomingValues();
          CHECK_LT(0, num_preds);

          llvm::Value *sel_val = nullptr;

          if (1 == num_preds) {
            sel_val = phi->getIncomingValue(0);

          // Turn it into a SelectInst.
          } else if (2 == num_preds) {
            auto pred_block = phi->getIncomingBlock(0);
            auto val_cond = pred_conds[block][pred_block];
            LOG_IF(FATAL, !val_cond) << "Missing reaching condition for value";
            CHECK_NE(val_cond, true_value);
            auto true_val = phi->getIncomingValue(0);
            auto false_val = phi->getIncomingValue(1);
            sel_val = ir.CreateSelect(val_cond, true_val, false_val);

          // Turn it into a tower of SelectInsts.
          } else {
            sel_val = llvm::Constant::getNullValue(phi->getType());
            for (auto i = 0u; i < num_preds; ++i) {
              auto pred_block = phi->getIncomingBlock(i);
              auto pred_val = phi->getIncomingValue(i);
              auto val_cond = pred_conds[block][pred_block];
              LOG_IF(FATAL, !val_cond) << "Missing reaching condition for value";
              CHECK_NE(val_cond, true_value);
              sel_val = ir.CreateSelect(val_cond, pred_val, sel_val);
            }
          }

          phi->replaceAllUsesWith(sel_val);
          to_remove.push_back(inst);

        } else if (auto ret = llvm::dyn_cast<llvm::ReturnInst>(inst)) {
          ret_vals.emplace_back(ret, reaching_cond[block]);
        } else if (auto store = llvm::dyn_cast<llvm::StoreInst>(inst)) {
          // Consider following:
          //   if (x) return;
          //   SF <- 0
          // We need to wrap SF <- 0 as:
          //   SF <- select(path_condition, 0, Load(SF))
          // Otherwise the SF would always be `0` no matter the `x`
          auto ptr_op = store->getPointerOperand();
          auto gep = llvm::dyn_cast<llvm::GetElementPtrInst>(ptr_op);
          auto state = remill::NthArgument(func, remill::kStatePointerArgNum);
          if (gep && gep->getPointerOperand() == state) {
            llvm::IRBuilder<> ir(new_block);
            auto original = ir.CreateLoad(gep);
            auto guarded = ir.CreateSelect(reaching_cond[block], store->getOperand(0), original);
            ir.CreateStore(guarded, gep);
          }
          to_remove.push_back(inst);
        } else if (!inst->isTerminator()) {
          inst->removeFromParent();
          new_block->getInstList().insert(new_block->end(), inst);

        } else {
          to_remove.push_back(inst);
        }
      }
    }
    // Add a final return value to the data flow function.
    CHECK(!ret_vals.empty());
    if (1 == ret_vals.size()) {
      ret_vals[0].first->removeFromParent();
      new_block->getInstList().insert(new_block->end(), ret_vals[0].first);
      ret_vals.clear();

    // Create a tower of selects, where the default value is a call to
    // `__remill_error`, which will signal downstream translation to
    // the IR to produce set the error bit.
    } else {
      llvm::IRBuilder<> ir(new_block);

      llvm::Value *args[remill::kNumBlockArgs];
      for (auto i = 0u; i < remill::kNumBlockArgs; ++i) {
        args[i] = llvm::UndefValue::get(remill::NthArgument(func, i)->getType());
      }

      llvm::Value *sel_val = ir.CreateCall(error, args);
      for (auto [ret_inst, reaching_cond] : ret_vals) {
        CHECK_NE(reaching_cond, true_value);
        llvm::Value *ret_val = ret_inst->getReturnValue();
        sel_val = ir.CreateSelect(reaching_cond, ret_val, sel_val);
        ret_inst->eraseFromParent();
      }

      ir.CreateRet(sel_val);
    }

    for (auto inst : to_remove) {
      inst->eraseFromParent();
    }

    for (auto block : orig_blocks) {
      block->eraseFromParent();
    }
    remill::VerifyModule(module);
  }
}; // struct Flattener


} // namespace circuitous