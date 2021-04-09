/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <cstdint>
#include <deque>
#include <map>
#include <optional>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <circuitous/Lifter/Shadows.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#pragma clang diagnostic pop

namespace circuitous::shadowinst {

  using values_t = std::vector<llvm::Value *>;
  using annotated_values = std::unordered_map<std::string, values_t>;
  static inline annotated_values collect_annotated(llvm::Value *from, llvm::Value *to) {
    annotated_values out = {
      { "circuitous.dst.reg", {} },
      { "circuitous.verify_fn_args", {} },
    };

    using bb_it_t = llvm::BasicBlock::iterator;
    auto begin = bb_it_t{llvm::cast<llvm::Instruction>(from)};
    auto end = bb_it_t{llvm::cast<llvm::Instruction>(to)};
    for (auto it = begin; it != end; ++it) {
      auto &inst = *it;
      if (auto dst_alloca = GetMetadata(&inst, "circuitous.dst.reg")) {
        out["circuitous.dst.reg"].push_back(&inst);
      }
      if (auto verify_arg = GetMetadata(&inst, "circuitous.verify_fn_args")) {
        out["circuitous.verify_fn_args"].push_back(&inst);
      }
    }
    return out;
  }

  struct SelectMaker {
    llvm::Value *head = nullptr;
    std::vector<llvm::Value *> conditions;
    llvm::IRBuilder<> &ir;

    SelectMaker(llvm::IRBuilder<> &ir_) : ir(ir_) {}

    llvm::Value *initial_head(llvm::Value *val) {
      return llvm::UndefValue::get(val->getType());
    }

    bool is_ptr(llvm::Value *val) const { return llvm::isa<llvm::PointerType>(val->getType()); }
    bool is_ival(llvm::Value *val) const { return llvm::isa<llvm::IntegerType>(val->getType()); }

    llvm::Value *coerce(llvm::Value *val) {
      CHECK(is_ptr(head) == is_ptr(val));
      CHECK(is_ival(head) == is_ival(val));

      if (is_ival(head)) {
        return ir.CreateSExtOrTrunc(val, head->getType());
      }
      if (is_ptr(head)) {
        return ir.CreateBitCast(val, head->getType());
      }
      LOG(FATAL) << "Unreachable";
    }

    llvm::Value *chain(llvm::Value *condition, llvm::Value *on_true) {
      if (!head) {
        head = initial_head(on_true);
      }
      conditions.push_back(condition);
      head = ir.CreateSelect(condition, coerce(on_true), head);
      return head;
    }

    template<typename T=llvm::Value>
    llvm::Value *get() { return llvm::cast< T >(head); }
  };

  static inline auto make_APInt(
    const std::vector<bool> &bits, std::size_t from, std::size_t size)
  {
    CHECK(bits.size() >= from + size) << bits.size() << " >= " << from + size;

    std::string span;
    for (uint64_t i = 0; i < size; ++i) {
      span += (bits[from + i]) ? '1' : '0';
    }
    auto size_ = static_cast<uint32_t>(size);
    return llvm::APInt(size_, span, 2);
  }

  // Emit instructions that will encode the `reg_name`
  static inline auto decoder_conditions(
    const Reg &s_reg, const Reg::reg_t &reg_name, llvm::IRBuilder<> &ir)
  {
    CHECK(s_reg.translation_map.count(reg_name));

    std::vector<llvm::Value *> translation_checks;
    for (auto &mats : s_reg.translation_map.find(reg_name)->second) {
      std::vector<llvm::Value *> reg_fragment_checks;
      std::size_t current = 0;

      for (auto &[from, size] : s_reg.regions) {
        auto extract = intrinsics::make_extract(ir, from, size);
        auto key = ir.getInt(make_APInt(mats, current, size));

        reg_fragment_checks.push_back(ir.CreateICmpEQ(extract, key));
        current += size;
      }

      translation_checks.push_back(make_and(ir, reg_fragment_checks));
    }

    return translation_checks;
  }

  template<typename Getter>
  auto make_decoder_selects(const Reg &s_reg, llvm::IRBuilder<> &ir, Getter &&get_reg) {
    SelectMaker selects{ir};
    for (auto &[reg, all_mats] : s_reg.translation_map) {
      auto conditions = decoder_conditions(s_reg, reg, ir);
      auto condition = intrinsics::make_xor(ir, conditions);
      selects.chain(condition, get_reg(reg, ir));
    }
    auto xor_all = intrinsics::make_xor(ir, selects.conditions);
    return std::make_tuple(xor_all, selects.get());
  }

} // namespace circuitous::shadowinst