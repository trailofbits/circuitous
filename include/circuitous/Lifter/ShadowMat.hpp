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

#include <circuitous/Support/Check.hpp>

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Lifter/Shadows.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#pragma clang diagnostic pop

namespace circ::shadowinst {

  using values_t = std::vector<llvm::Value *>;
  using annotated_values = std::unordered_map<std::string, values_t>;
  static inline annotated_values collect_annotated(llvm::Value *from, llvm::Value *to) {
    annotated_values out = {
      { Names::meta::dst_reg, {} },
      { Names::meta::verify_args, {} },
    };

    using bb_it_t = llvm::BasicBlock::iterator;
    auto begin = bb_it_t{llvm::cast<llvm::Instruction>(from)};
    auto end = bb_it_t{llvm::cast<llvm::Instruction>(to)};
    for (auto it = begin; it != end; ++it) {
      auto &inst = *it;
      if (auto dst_alloca = GetMetadata(&inst, Names::meta::dst_reg)) {
        out[Names::meta::dst_reg].push_back(&inst);
      }
      if (auto verify_arg = GetMetadata(&inst, Names::meta::verify_args)) {
        out[Names::meta::verify_args].push_back(&inst);
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
      UNREACHABLE() << "Unreachable";
    }

    llvm::Value *chain(llvm::Value *condition, llvm::Value *on_true) {
      if (!head) {
        head = initial_head(on_true);
      }
      conditions.push_back(condition);
      head = ir.CreateSelect(condition, coerce(on_true), head);
      return head;
    }

    llvm::Value *chain(const std::map< llvm::Value *, llvm::Value * > &vals) {
      for (const auto &[c, v] : vals)
        chain(c, v);
      return get();
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

  static inline auto region_selector(llvm::IRBuilder<> &ir, const Reg &s_reg) {
    std::vector<llvm::Value *> input_fragments;

    for (auto &[from, size] : s_reg.regions) {
      auto extract = irops::make_leaf< irops::Extract >(ir, from, size);
      input_fragments.push_back(extract);
    }
    // We need to match the order of the entry in `translation_map`
    std::reverse(input_fragments.begin(), input_fragments.end());
    return irops::make< irops::Concat >(ir, input_fragments);
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

      if (s_reg.regions.empty()) {
        translation_checks.push_back(ir.getTrue());
        continue;
      }


      for (auto &[from, size] : s_reg.regions) {
        auto extract =
          irops::make< irops::Extract >(ir, std::vector< llvm::Value * >{}, from, size);
        input_fragments.push_back(extract);
        current += size;
      }
      CHECK(!input_fragments.empty());
      // We need to match the order of the entry in `translation_map`
      std::reverse(input_fragments.begin(), input_fragments.end());
      auto full_input = irops::make< irops::Concat >(ir, input_fragments);
      auto expected_value = ir.getInt(make_APInt(mats, 0, current));
      translation_checks.push_back(ir.CreateICmpEQ(full_input, expected_value));
    }
    return translation_checks;
  }

  static inline auto decoder_conditions(auto arch, const Reg &s_reg, llvm::IRBuilder<> &ir)
  {
    // Insert elements from vector `from` into vector `into`.
    // Usefull if `from` is rvalue, since then `begin(), end()` cannot be easily
    // retrieved at the same time.
    auto append = [](auto &into, const auto &from) {
      into.insert(into.end(), from.begin(), from.end());
    };

    std::map< Reg::reg_t, std::vector< llvm::Value * > > out;
    for (const auto &[reg, _] : s_reg.translation_map) {
      append(out[enclosing_reg(arch, reg)->name], decoder_conditions(s_reg, reg, ir));
    }
    return out;
  }

  template<typename Getter>
  auto make_intrinsics_decoder(const Reg &s_reg, llvm::IRBuilder<> &ir, Getter &get_reg) {
    auto entries = s_reg.translation_entries_count();
    auto bits = s_reg.region_bitsize();
    CHECK(entries <= (1 << bits))
        << "Translation entries count do not correspond to regions size "
        << entries << " > " << (1 << bits);

    std::vector<llvm::Value *> select_args((1 << bits) + 1, nullptr);
    select_args[0] = region_selector(ir, s_reg);

    llvm::Type *type = nullptr;
    for (auto &[str, reg] : s_reg.translation_bytes_map()) {
      auto idx = llvm::APInt{static_cast<uint32_t>(bits), str, 2}.getLimitedValue();
      CHECK(select_args.size() > idx + 1);
      select_args[idx + 1] = get_reg(ir, reg);
      if (!type) {
        type = select_args[idx + 1]->getType();
      }
      CHECK(type == select_args[idx + 1]->getType());
    }

    CHECK(type);
    std::vector<llvm::Value *> holes;
    for (std::size_t idx = 1; idx < select_args.size(); ++idx) {
      if (select_args[idx]) {
        continue;
      }
      select_args[idx] = llvm::UndefValue::get(type);
      auto key = ir.getIntN(static_cast<uint32_t>(bits), idx - 1);
      holes.push_back(ir.CreateICmpEQ(select_args[0], key));
    }

    // We cannot accept is a hole was selected
    auto cond = [&]() -> llvm::Value * {
      if (holes.empty()) {
        return nullptr;
      }
      return ir.CreateNot(make_or(ir, holes));
    }();

    // We do not need to do any extra checking if we decoded something because
    // the select is "saturated" -- each possible return is a valid register
    CHECK(select_args.size() > 1);
    auto select = irops::make< irops::Select >(ir, select_args);
    return std::make_tuple(cond, select);
  }

  // Return `i1` that is set to true if `s_reg` was decoded to `reg`.
  static inline auto make_explicit_decode(
      llvm::IRBuilder<> &ir, const Reg &s_reg, const std::string &reg)
  {
    auto selector = region_selector(ir, s_reg);

    auto it = s_reg.translation_map.find(reg);
    CHECK(it != s_reg.translation_map.end());

    llvm::Value *acc = ir.getFalse();
    for (auto &mat : it->second) {
      auto as_constant = ir.getInt(make_APInt(mat, 0, mat.size()));
      acc = ir.CreateOr(ir.CreateICmpEQ(selector, as_constant), acc);
    }
    return acc;
  }

} // namespace circ::shadowinst
