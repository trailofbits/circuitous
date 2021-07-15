/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

// TODO(lukas): Fill llvm headers
#include <fstream>
#include <optional>
#include <string>

#include <remill/BC/Version.h>
#include <remill/BC/Util.h>
#include <remill/Arch/Arch.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/Transforms/Utils/Cloning.h>
#pragma clang diagnostic pop

namespace circ {

  static inline void EraseFn(llvm::Module *module, const std::string &fn_name) {
    if (auto fn = module->getFunction(fn_name)) {
      fn->eraseFromParent();
    }
  }

  static inline void EraseFns(llvm::Module *module, const std::vector<std::string> &fns) {
    for (auto &fn : fns) {
      EraseFn(module, fn);
    }
  }

  static inline std::string LLVMName(llvm::Value *val,
                                     const std::string &def = "(name not set)") {
    if (!val->hasName()) {
      return def;
    }
    return val->getName().str();
  }

  template<typename T>
  void AddMetadata(llvm::Instruction *inst, const std::string &kind, T value) {
    CHECK(inst);
    CHECK(!inst->getMetadata(kind));
    auto &ctx = inst->getContext();
    llvm::IRBuilder<> ir(ctx);

    auto format = [](const auto &val_) {
      if constexpr (std::is_integral_v< T >) {
        return std::to_string(val_);
      } else {
        return val_;
      }
    };

    auto node = llvm::MDNode::get(
      ctx, {llvm::MDString::get(ctx, format(value))});
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

  template<typename C>
  auto make_or(llvm::IRBuilder<> &ir, const C &vals) {
    llvm::Value *acc = ir.getFalse();
    for (auto val : vals) {
      acc = ir.CreateOr(acc, val);
    }
    return acc;
  }

  static inline bool is_undef(llvm::Value *val) {
    return llvm::UndefValue::get(val->getType()) == val;
  }

  static inline auto extend(llvm::CallInst *call, std::vector<llvm::Value *> args) {
    llvm::IRBuilder<> ir(call);
    args.insert(args.begin(), call->data_operands_begin(), call->data_operands_end());
    auto callee = call->getCalledFunction();

    auto new_call = ir.CreateCall(callee, args);
    new_call->copyMetadata(*call);
    call->replaceAllUsesWith(new_call);
    call->eraseFromParent();
    return new_call;
  }

  static inline auto reg_from_gep(llvm::GetElementPtrInst *gep, const auto &arch)
  -> const remill::Register *
  {
    auto dl = llvm::DataLayout(gep->getModule());
    auto [_, off] = remill::StripAndAccumulateConstantOffsets(dl, gep);
    return arch->RegisterAtStateOffset(static_cast<uint64_t>(off));
  }

  static inline auto coerce_reg(llvm::LoadInst *load, const remill::Register *reg)
  -> const remill::Register *
  {
    if (!reg) {
      return nullptr;
    }
    auto dl = llvm::DataLayout(load->getModule());
    auto size = dl.getTypeSizeInBits (load->getType());
    return reg->EnclosingRegisterOfSize(size / 8);
  }

  static inline auto make_range(llvm::Instruction *from, llvm::Instruction *to) {
    using bb_t = llvm::BasicBlock::iterator;
    return llvm::iterator_range< bb_t >(bb_t{from}, bb_t{to});
  }

  template<typename F>
  static inline auto inline_flattened(llvm::CallInst *call, F &&f) {
    auto begin = f(llvm::IRBuilder<>(call));
    using bb_it = llvm::BasicBlock::iterator;
    auto end = f(llvm::IRBuilder<>(call->getParent(), std::next(bb_it{call})));

    auto block = call->getParent();
    if (std::prev(block->end())->isTerminator()) {
      LOG(WARNING) << "Going to inline ill formed block (terminator missing).";
    }
    CHECK(call->getCalledFunction()->size() == 1);

    llvm::InlineFunctionInfo info;
  #if LLVM_VERSION_NUMBER < LLVM_VERSION(11, 0)
    llvm::InlineFunction(call, info);
  #else
    llvm::InlineFunction(*call, info);
  #endif

    CHECK(begin->getParent() == end->getParent());
    return std::make_tuple(begin, end);
  }

} // namespace circ
