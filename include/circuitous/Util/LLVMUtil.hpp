/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

// TODO(lukas): Fill llvm headers
#include <fstream>
#include <optional>
#include <string>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/raw_os_ostream.h>
#pragma clang diagnostic pop

namespace circuitous {
  static inline std::string LLVMName(llvm::Value *val,
                                     const std::string &def = "(name not set)") {
    if (!val->hasName()) {
      return def;
    }
    return val->getName().str();
  }

  template<typename T>
  void AddMetadata(llvm::Instruction *inst, const std::string &kind, T value) {
    CHECK(!inst->getMetadata(kind));
    auto &ctx = inst->getContext();
    llvm::IRBuilder<> ir(ctx);
    auto node = llvm::MDNode::get(
      ctx, {llvm::MDString::get(ctx, std::to_string(value))});
    inst->setMetadata(kind, node);
  }

  // If you feel like you need to return different type just extend it.
  static inline std::optional<uint64_t> GetMetadata(llvm::Instruction *inst,
                                                    const std::string &kind) {
    auto node = inst->getMetadata(kind);
    if (!node) {
      return {};
    }
    CHECK(node->getNumOperands() == 1);
    auto op = llvm::dyn_cast<llvm::MDString>(node->getOperand(0));
    return std::strtoull(op->getString().data(), nullptr, 10);
  }

  static inline void DumpFns(
      const std::string &filename, const std::vector<llvm::Function *> &fns) {
    std::ofstream file(filename);
    llvm::raw_os_ostream out(file);
    for (auto fn : fns) {
      fn->print(out);
    }
    out.flush();
  }

  template<typename C>
  auto make_and(llvm::IRBuilder<> &ir, const C &vals) {
    llvm::Value *acc = ir.getTrue();
    for (auto val : vals) {
      acc = ir.CreateAnd(acc, val);
    }
    return acc;
  }

} // namespace circuitous
