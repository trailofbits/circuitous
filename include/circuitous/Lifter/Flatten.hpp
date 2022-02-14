/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Intrinsics.hpp>
#include <circuitous/Util/Logging.hpp>
#include <circuitous/Support/Check.hpp>
#include <circuitous/Support/Log.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Verifier.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ {

struct Flattener {
  llvm::Function *func;
  // Function that generates errors
  llvm::Function *error;
  llvm::BasicBlock *new_block = nullptr;

  using instructions_t = std::vector<llvm::Instruction *>;
  template<typename T>
  using bb_map = std::unordered_map<llvm::BasicBlock *, T>;

  bb_map<llvm::Value *> reaching_cond;
  bb_map<bb_map<llvm::Value *>> pred_conds;
  std::unordered_map<llvm::SwitchInst *, bb_map<llvm::Value *>> switch_conds;

  std::vector<llvm::BasicBlock *> orig_blocks;

  Flattener(llvm::Function *func_, llvm::Function *error_) :
    func(func_), error(error_)
  {}

  void FlattenSwitch(llvm::SwitchInst *inst, llvm::IRBuilder<> &ir) {
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
  }

  auto &SwitchTarget(llvm::SwitchInst *inst, llvm::IRBuilder<> &ir) {
    if (!switch_conds.count(inst)) {
      FlattenSwitch(inst, ir);
    }
    return switch_conds[inst];
  }

  // Compute condition `pred_block -> block` and insert new instructions if needed
  // by `ir` (which is inserting into `new_block`).
  llvm::Value *GetCondition(llvm::BasicBlock *block, llvm::BasicBlock *pred_block,
                            llvm::IRBuilder<> &ir) {
    const auto pred_cond = reaching_cond[pred_block];
    check(pred_cond) << "Cycle in control-flow graphs are not handled";

    // Figure out the reaching condition for `block` coming through
    // `pred`.
    auto inst = pred_block->getTerminator();
    if (auto pred_br = llvm::dyn_cast<llvm::BranchInst>(inst)) {
      // There is no condition, just inherit
      if (!pred_br->isConditional()) {
        return pred_cond;
      }

      auto true_succ = pred_br->getSuccessor(0);
      auto false_succ = pred_br->getSuccessor(1);

      // TODO(lukas): Probably just return `pred_cond`
      check(true_succ != false_succ)
        << "TODO: Not sure how to handle when true_succ == false_succ";
      auto edge_cond = pred_br->getCondition();
      if (true_succ == block) {
        return ir.CreateAnd(pred_cond, edge_cond);
      }
      return ir.CreateAnd(pred_cond, ir.CreateNot(edge_cond));
    }
    if (auto pred_switch = llvm::dyn_cast<llvm::SwitchInst>(inst)) {
      auto edge_cond = SwitchTarget(pred_switch, ir)[block];
      check(edge_cond);
      return ir.CreateAnd(pred_cond, edge_cond);
    }
    return pred_cond;
  }

  void CreatePathCondition(llvm::BasicBlock *block) {
    // The entry block is guaranteed to be reachable.
    llvm::IRBuilder<> ir(new_block);
    if (block->hasNPredecessors(0)) {
      reaching_cond.emplace(block, ir.getTrue());
      pred_conds[block].emplace(nullptr, ir.getTrue());
      pred_conds[block].emplace(block, ir.getTrue());
      return;
    }
    // Figure out the reaching conditions for this block, and express them as
    // data flow (i.e. instructions).

    llvm::Value *cond = ir.getFalse();

    for (auto pred_block : llvm::predecessors(block)) {
      auto edge_cond = GetCondition(block, pred_block, ir);
      pred_conds[block].emplace(pred_block, edge_cond);
      cond = ir.CreateXor(cond, edge_cond);
    }
    reaching_cond.emplace(block, cond);
  }

  void HandleBBs() {
    check(new_block);

    // We need to keep account of them as they are processed last
    std::unordered_map<llvm::ReturnInst *, llvm::Value *> ret_vals;
    for (auto block : orig_blocks) {
      CreatePathCondition(block);

      // We are going to modify the block, we need to copy the instructions before
      // hand - iterating the block while modifying it is a **terrible** idea.
      std::vector<llvm::Instruction *> insts;
      for (llvm::Instruction &inst : *block) {
        insts.push_back(&inst);
      }

      for (auto inst : insts) {
        if (auto phi = llvm::dyn_cast<llvm::PHINode>(inst)) {
          llvm::IRBuilder<> ir(new_block);

          const auto num_preds = phi->getNumIncomingValues();
          check(0 < num_preds);

          llvm::Value *sel_val = nullptr;

          if (1 == num_preds) {
            sel_val = phi->getIncomingValue(0);

          // Turn it into a SelectInst.
          } else if (2 == num_preds) {
            auto pred_block = phi->getIncomingBlock(0);
            auto val_cond = pred_conds[block][pred_block];
            check(val_cond) << "Missing reaching condition for value";
            check(val_cond != ir.getTrue());
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
              check(val_cond) << "Missing reaching condition for value";
              check(val_cond != ir.getTrue());
              sel_val = ir.CreateSelect(val_cond, pred_val, sel_val);
            }
          }

          phi->replaceAllUsesWith(sel_val);

        } else if (auto call = llvm::dyn_cast<llvm::CallInst>(inst)) {
          auto callee = call->getCalledFunction();
          if (callee->getName() == "__remill_error") {
            llvm::IRBuilder<> ir(new_block);
            irops::make< irops::Error >(ir, reaching_cond[block]);
            call->replaceAllUsesWith(call->getArgOperand(2u));
            continue;
          }
          inst->removeFromParent();
          new_block->getInstList().insert(new_block->end(), inst);
        } else if (auto ret = llvm::dyn_cast<llvm::ReturnInst>(inst)) {
          ret_vals[ret] = reaching_cond[block];
        } else if (auto store = llvm::dyn_cast<llvm::StoreInst>(inst)) {
          // Consider following:
          //   if (x) return;
          //   SF <- 0
          // We need to wrap SF <- 0 as:
          //   SF <- select(path_condition, 0, Load(SF))
          // Otherwise the SF would always be `0` no matter the `x`
          llvm::IRBuilder<> ir(new_block);
          auto ptr_op = store->getPointerOperand();
          auto original = ir.CreateLoad(ptr_op);
          auto guarded = ir.CreateSelect(reaching_cond[block], store->getOperand(0), original);
          ir.CreateStore(guarded, ptr_op);
        } else if (!inst->isTerminator()) {
          inst->removeFromParent();
          new_block->getInstList().insert(new_block->end(), inst);

        } else {
          // Here only Terminator instructions such as `br` or `switch` should fall through,
          // and we have already taken care of those.
          check(llvm::isa<llvm::BranchInst>(inst) || llvm::isa<llvm::SwitchInst>(inst));
        }
      }
    }
    HandleReturns(ret_vals);
  }

  void HandleReturns(const std::unordered_map<llvm::ReturnInst *, llvm::Value *> &ret_vals) {
    check(!ret_vals.empty());
    if (1 == ret_vals.size()) {
      auto &[ret, _] = *ret_vals.begin();
      ret->removeFromParent();
      new_block->getInstList().insert(new_block->end(), ret);
      return;
    }
    // Create a tower of selects, where the default value is a call to
    // `__remill_error`, which will signal downstream translation to
    // the IR to produce set the error bit.
    llvm::IRBuilder<> ir(new_block);

    llvm::Value *args[remill::kNumBlockArgs];
    for (auto i = 0u; i < remill::kNumBlockArgs; ++i) {
      args[i] = llvm::UndefValue::get(remill::NthArgument(func, i)->getType());
    }

    llvm::Value *sel_val = ir.CreateCall(error, args);
    for (auto [ret_inst, reaching_cond] : ret_vals) {
      check(reaching_cond != ir.getTrue());
      llvm::Value *ret_val = ret_inst->getReturnValue();
      sel_val = ir.CreateSelect(reaching_cond, ret_val, sel_val);
      ret_inst->eraseFromParent();
    }

    ir.CreateRet(sel_val);
  }

  void Run() {
    auto &context = func->getContext();

    auto entry_block = &(func->getEntryBlock());
    // Store these so we have them in nice iterable shape before we add a new block
    for (llvm::BasicBlock *block : llvm::ReversePostOrderTraversal(entry_block)) {
      orig_blocks.push_back(block);
    }

    new_block = llvm::BasicBlock::Create(context, "", func, entry_block);
    HandleBBs();

    // We no longer need any of the original blocks
    for (auto block : orig_blocks) {
      block->eraseFromParent();
    }
    verify_function(*func);
  }
}; // struct Flattener


} // namespace circ
