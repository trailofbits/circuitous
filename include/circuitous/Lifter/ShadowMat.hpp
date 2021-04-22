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
    llvm::Value *get() { return llvm::cast_or_null< T >(head); }
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

  // Emit instructions that will check the encoding of the register `reg_name`
  // First we check each entry of the `translation_map`
  //
  // fragment1 = extract.X1.X2()
  // fragment2 = extract.Y1.Y2()
  // ...
  //
  // Then we need to check the concated value against what's passed in
  // `translation_map`
  //
  // fullN = concat(fragmentN, fragmentN-1, ..., fragment1)
  // final_check = and fullN translation_map[REG]
  //
  // This can also be achieved without concats only with `and`s, but this should
  // produced a slightly nicer bitcode.
  static inline auto decoder_conditions(
    const Reg &s_reg, const Reg::reg_t &reg_name, llvm::IRBuilder<> &ir)
  {
    CHECK(s_reg.translation_map.count(reg_name));

    std::vector<llvm::Value *> translation_checks;
    for (auto &mats : s_reg.translation_map.find(reg_name)->second) {
      std::vector<llvm::Value *> input_fragments;
      std::size_t current = 0;

      for (auto &[from, size] : s_reg.regions) {
        auto extract = intrinsics::make_extract(ir, from, size);
        input_fragments.push_back(extract);
        current += size;
      }
      // We need to match the order of the entry in `translation_map`
      std::reverse(input_fragments.begin(), input_fragments.end());
      auto full_input = intrinsics::make_concat(ir, input_fragments);
      auto expected_value = ir.getInt(make_APInt(mats, 0, current));
      translation_checks.push_back(ir.CreateICmpEQ(full_input, expected_value));
    }

    return translation_checks;
  }

  template<typename Getter>
  auto make_decoder_selects(const Reg &s_reg, llvm::IRBuilder<> &ir, Getter &&get_reg) {
    auto merge_conditions = [&](auto &all) -> llvm::Value * {
      if (all.size() == 1) {
        return all[0];
      }
      return intrinsics::make_xor(ir, all);
    };

    SelectMaker selects{ir};
    for (auto &[reg, all_mats] : s_reg.translation_map) {
      auto conditions = decoder_conditions(s_reg, reg, ir);

      // We do not need to emit `xor` if there there would
      // be only one argument to it.
      auto condition = merge_conditions(conditions);
      selects.chain(condition, get_reg(reg, ir));
    }
    auto xor_all = intrinsics::make_xor(ir, selects.conditions);
    return std::make_tuple(xor_all, selects.get());
  }

} // namespace circuitous::shadowinst