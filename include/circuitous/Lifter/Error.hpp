/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Intrinsics.hpp>
#include <circuitous/Util/LLVMUtil.hpp>

#include <tuple>
#include <vector>

namespace circ::err {

  static inline bool is_div(llvm::BinaryOperator *op) {
    auto op_code = op->getOpcode();
    using BO = llvm::BinaryOperator;
    return op_code == BO::SDiv || op_code == BO::UDiv;
  }

  // Returns `[explicit_errors, implicit_erros]`
  template<typename R = llvm::iterator_range<llvm::BasicBlock::iterator>>
  static inline auto collect(R range)
  {
    auto explicit_errs = intrinsics::collect<intrinsics::Error>(range);
    std::vector<llvm::Value *> implicit_errs;
    for (auto &inst : range) {
      if (auto bin_op = llvm::dyn_cast<llvm::BinaryOperator>(&inst)) {
        if (is_div(bin_op)) {
          implicit_errs.push_back(bin_op);
        }
      }
    }
    return std::make_tuple(std::move(explicit_errs), std::move(implicit_errs));
  }

  static inline auto collect(llvm::Value *from, llvm::Value *to) {
    CHECK(llvm::isa<llvm::Instruction>(from) && llvm::isa<llvm::Instruction>(to));

    auto as_it = [](auto what) {
      return llvm::BasicBlock::iterator{ llvm::cast<llvm::Instruction>(what) };
    };
    return collect({as_it(from), as_it(to)});
  }


  llvm::Value *handle_div(llvm::BinaryOperator *div) {
    auto divisor = div->getOperand(1u);
    llvm::IRBuilder<> ir(div);
    auto zero = llvm::ConstantInt::get(divisor->getType(), 0u);
    auto is_zero = ir.CreateICmpEQ(zero, divisor);
    return ir.CreateSelect(is_zero, ir.getTrue(), ir.getFalse());
  }

  static inline auto handle_implicit(
      llvm::IRBuilder<> &ir, const std::vector<llvm::Value *> &implicit_errs)
  {
    std::vector<llvm::Value *> out;
    for (auto err_i : implicit_errs) {
      auto bin_op = llvm::dyn_cast< llvm::BinaryOperator >(err_i);
      if (bin_op && is_div(bin_op)) {
        out.push_back(handle_div(bin_op));
      }
    }
    return out;
  }

  static inline llvm::Value *synthesise_current(
      llvm::IRBuilder<> &ir, llvm::Value *from, llvm::Value *to)
  {
    const auto &[explicit_errs, implicit_errs] = collect(from, to);

    // Check if this instruction is faultable
    if (explicit_errs.empty() && implicit_errs.empty()) {
      return nullptr;
    }

    auto args = intrinsics::Error::unwrap(explicit_errs);

    auto args_ = handle_implicit(ir, implicit_errs);
    args.insert(args.end(), args_.begin(), args_.end());
    return intrinsics::make_or(ir, args);
  }

} // namespace circ::err