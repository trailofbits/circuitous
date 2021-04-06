/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <remill/Arch/Arch.h>
#include <remill/Arch/Instruction.h>

#include <circuitous/Lifter/Shadows.hpp>

#include <bitset>
#include <optional>
#include <string>
#include <vector>
#include <map>
#include <unordered_map>

namespace circuitous {

namespace permutate {

  using permutations_t = std::vector<std::optional<remill::Instruction>>;

  static inline permutations_t Flip(
    const remill::Instruction &rinst, const remill::Arch::ArchPtr &arch)
  {
    permutations_t out;
    out.resize(rinst.bytes.size() * 8);
    for (std::size_t i = 0; i < rinst.bytes.size(); ++i) {
      for (int j = 7; j >= 0; --j) {
        auto index = 8 * i + 7 - static_cast<uint64_t>(j);

        std::string flipped = rinst.bytes;
        auto byte = static_cast<uint8_t>(flipped[i]);
        uint8_t mask = 1;
        flipped[i] = static_cast<char>(byte ^ (mask << j));

        remill::Instruction tmp;
        if (arch->DecodeInstruction(0, flipped, tmp)) {
          out[index] = std::move(tmp);
        }
      }
    }
    return out;
  }

  struct OperandRef {
    using value_type = remill::Operand;
    using reference = remill::Operand &;
    using pointer = remill::Operand *;

    remill::Operand *op = nullptr;
    std::size_t idx = 0;

    pointer operator->() { return op; }
  };


  // Everything equals to everything
  struct TrueBase {
    using OpType = remill::Operand::Type;
    using operands_t = std::vector<const remill::Operand *>;

    static auto identity_imm() { return [](auto &, auto &) { return true; }; }
    static auto identity_reg() { return [](auto &, auto &) { return true; }; }

    static bool full_compare(const remill::Operand &lhs, const remill::Operand &rhs) {
      return lhs.Serialize() == rhs.Serialize();
    }

    static bool Depends(const operands_t &) { return true; }
    // TODO(lukas): Dependency verifier
  };

  template<typename Next>
  struct UnitCompares : Next {
    using OpType = typename Next::OpType;

    static auto identity_imm() {
      return [](auto &self, auto &flipped) {
        // TODO(lukas): Check in remill that is_signed is always properly
        //              (and consistently) set.
        // This may seem counterintuitive, but two immediates are equal
        // even if their values are not equal -- actaully that is the whole
        // point.
        return flipped.type == remill::Operand::kTypeImmediate
              && self.size == flipped.size;
      };
    }

    static auto identity_reg() {
      return [](auto &self, auto &flipped) {
        return flipped.type == OpType::kTypeRegister &&
              self.size == flipped.size;
      };
    }
  };

  template<typename Next>
  struct EqDependency : Next {
    using OpType = typename Next::OpType;

    static bool Depends(const std::vector<const remill::Operand *> &ops) {
      for (auto op : ops) {
        CHECK(op->type == OpType::kTypeRegister)
          << "Cannot check EqDependency on non-register operands.";
      }
      auto compare = [](auto a, auto b) {
        return a.name == b.name && a.size == b.size;
      };

      auto &fst = *(ops.begin());
      for (auto op : ops) {
        if (!compare(fst->reg, op->reg)) {
          return false;
        }
      }
      return true;
    }
  };

  template<typename Next>
  struct DependencyComparator : Next {
    using Item_t = std::map<std::size_t, const remill::Operand *>;
    using cri = const remill::Instruction &;

    template<typename Fn>
    static std::tuple<bool, bool> CheckStructure(
      cri original, cri permutation, const Item_t &items, Fn &&on_self)
    {
      if (original.function != permutation.function) {
        return { false, false };
      }

      if (original.operands.size() != permutation.operands.size()) {
        return { false, false };
      }

      bool exact_check = true;
      bool on_self_check = true;
      for (auto [i, op] : items) {
        exact_check &= Next::full_compare(*op, permutation.operands[i]);
        on_self_check &= on_self(*op, permutation.operands[i]);
      }

      if (!on_self_check) {
        return { false, exact_check };
      }

      // Check if there is the assumed relationship.
      // Often, it can be the same as the `exact_check`, but not always.
      std::vector<const remill::Operand *> raw_items;
      for (auto [_, op] : items) raw_items.push_back(op);
      if (!Next::Depends(raw_items)) {
        return { false, exact_check };
      }

      for (std::size_t i = 0U; i < permutation.operands.size(); ++i) {
        // Was already checked
        if (items.count(i)) {
          continue;
        }

        if (!Next::full_compare(original.operands[i], permutation.operands[i])) {
          return { false, exact_check };
        }
      }
      return { true, exact_check };
    }
  };

  template<typename Next>
  struct Dispatch : Next {
    using OpType = typename Next::OpType;
    using Item_t = typename Next::Item_t;

    static bool Compare(const remill::Instruction &original,
                        const remill::Instruction &permutation,
                        const Item_t &items)
    {
      CHECK(items.size() >= 1) << "Cannot compare " << items.size() << " items.";
      OpType type = items.begin()->second->type;
      for (auto [_, op] : items) {
        CHECK (op->type == type) << "Cannot compare as group operands of different types!";
      }
      switch(type) {
        case OpType::kTypeImmediate: return Check(original, permutation, items, Next::identity_imm());
        case OpType::kTypeRegister : return Check(original, permutation, items, Next::identity_reg());
        default                    : return false;
      }
    }

    using cri = const remill::Instruction &;
    using citem_ref = const Item_t &;

    template<typename Fn>
    static bool Check(cri original, cri permutation, citem_ref op, Fn &&on_self) {
      auto [structural, exact] = Next::CheckStructure(original, permutation, op, on_self);
      return structural && !exact;
    }
  };

  using Comparator = Dispatch<DependencyComparator<UnitCompares<TrueBase>>>;
  using HuskComparator = Dispatch<DependencyComparator<EqDependency<UnitCompares<TrueBase>>>>;
} // namespace permutate


struct InstructionFuzzer {
  // Declare return type
  using imm_meta_t = std::map<uint64_t, uint64_t>;
  using imm_meta_list_t = std::map<remill::Operand *, imm_meta_t>;

  // Import some remill types we will often neeed
  using Instruction = remill::Instruction;
  using Operand = remill::Operand;
  using Kind = remill::Operand::Address::Kind;
  using OpType = remill::Operand::Type;
  using Arch_ref = remill::Arch::ArchPtr;

  const remill::Arch::ArchPtr &arch;
  const remill::Instruction &rinst;

  permutate::permutations_t permutations;

  InstructionFuzzer(const Arch_ref &arch_, const remill::Instruction &rinst_)
    : arch(arch_), rinst(rinst_), permutations(permutate::Flip(rinst, arch))
  {}

  // TODO(lukas): Move this to remill.
  std::string ToString(remill::Operand::Type type) {
    switch(type) {
      case remill::Operand::kTypeInvalid : return "kTypeInvalid";
      case remill::Operand::kTypeRegister : return "kTypeRegister";
      case remill::Operand::kTypeShiftRegister : return "kTypeShiftRegister";
      case remill::Operand::kTypeImmediate : return "kTypeImmediate";
      case remill::Operand::kTypeAddress : return "kTypeAddress";
    }
  }

  // TODO(lukas): Move this to remill.
  std::string ToString(remill::Operand::Address::Kind &kind) {
    using ROAK = remill::Operand::Address::Kind;
    switch(kind) {
      case ROAK::kInvalid : return "kInvalid";
      case ROAK::kMemoryWrite : return "kMemoryWrite";
      case ROAK::kMemoryRead : return "kMemoryRead";
      case ROAK::kAddressCalculation : return "kAddressCalculation";
      case ROAK::kControlFlowTarget : return "kControlFlowTarget";
    }
  }

  auto FuzzOps() {
    shadowinst::Instruction shadow_inst;
    std::vector<std::vector<bool>> op_bits =
      { rinst.operands.size(), std::vector<bool>(rinst.bytes.size() * 8, false) };

    for (std::size_t i = 0; i < permutations.size(); ++i) {
      for (std::size_t op_i = 0; op_i < rinst.operands.size(); ++op_i) {
        if (!permutations[i]) {
          continue;
        }
        using C = permutate::Comparator;
        op_bits[op_i][i] = C::Compare(rinst,
                                      *permutations[i],
                                      { { op_i, &rinst.operands[op_i] } });
      }
    }

    for (std::size_t i = 0; i < rinst.operands.size(); ++i) {
      auto &r_op = rinst.operands[i];
      switch(r_op.type) {
        case OpType::kTypeRegister : {
          std::reverse(op_bits[i].begin(), op_bits[i].end());
          shadow_inst.Add<shadowinst::Reg>(op_bits[i]);
          auto &s_op = shadow_inst.operands.back();
          PopulateTranslationTable(*s_op.reg, i);
          break;
        }
        case OpType::kTypeImmediate : {
          std::reverse(op_bits[i].begin(), op_bits[i].end());
          shadow_inst.Add<shadowinst::Immediate>(op_bits[i]);
          break;
        }
        default:
          shadow_inst.operands.emplace_back();
          break;
      }
    }

    ResolveHusks(shadow_inst);
    LOG(INFO) << shadow_inst.to_string();
    return shadow_inst;
  }

  void PopulateTranslationTable(
    remill::Instruction &rinst, shadowinst::Reg &shadow_reg,
    std::size_t op_idx)
  {
    LOG(INFO) << "\t\t\t" << shadow_reg.region_bitsize();
    // TODO(lukas): This is 100% arbitrary check, I am curious
    //              if it fires (and how often)
    CHECK(shadow_reg.region_bitsize() <= 6);

    //auto idxs = shadow_reg.region_idxs();
    std::vector<uint64_t> idxs;
    for (auto &[from, size] : shadow_reg.regions) {
      auto afrom = rinst.bytes.size() * 8 - from - size;
      for (uint64_t i = 0; i < size; ++i) {
        idxs.push_back(afrom);
        ++afrom;
        LOG(INFO) << "Adjusted idx: " << afrom;
      }
    }
    auto yield = [&](auto bytes) {
      remill::Instruction tmp;
      if (!arch->DecodeInstruction(0, bytes, tmp)) {
        LOG(WARNING) << "Was not able to decode guaranteed permutation";
        return;
      }
      if (tmp.operands.size() != rinst.operands.size()) {
        LOG(WARNING) << "Permutation has different number of operands.";
        LOG(WARNING) << tmp.function;
        LOG(WARNING) << rinst.function;
        return;
      }
      auto &reg = tmp.operands[op_idx];

      std::vector<bool> out;
      std::stringstream ss;

      for (auto idx : idxs) {
        auto byte = static_cast<uint8_t>(bytes[idx / 8]);
        out.push_back((byte >> (7 - (idx % 8))) & 1u);
        ss << out.back();
      }
      LOG(INFO) << shadowinst::to_binary(bytes);
      LOG(INFO) << shadowinst::to_hex(bytes);
      LOG(INFO) << "\t\t" << reg.reg.name << " " << ss.str();
      shadow_reg.translation_map[reg.reg.name].insert(std::move(out));
    };
    std::string copied_bytes = rinst.bytes;
    _Permutate(copied_bytes, idxs, 0, yield);
  }

  template<typename Yield>
  void _Permutate(std::string &bytes, const std::vector<uint64_t> &idxs,
                  std::size_t current, Yield &yield) {
    // I am done flipping, we look at the value now
    if (current == idxs.size()) {
      yield(bytes);
      return;
    }

    // We chose not to flip first
    _Permutate(bytes, idxs, current + 1, yield);
    auto byte = static_cast<uint8_t>(bytes[idxs[current] / 8 ]);
    uint8_t mask = 1;
    bytes[idxs[current] / 8] = static_cast< char >(byte ^ (mask << (7 - idxs[current] % 8)));
    _Permutate(bytes, idxs, current + 1, yield);
    // We are taking bytes as reference we need to flip back as we backtrack!
    byte = static_cast<uint8_t>(bytes[idxs[current] / 8 ]);
    bytes[idxs[current] / 8] = static_cast< char >(byte ^ (mask << (7 - idxs[current] % 8)));
  }

  void DecodeRegions(Instruction *inst) {
    LOG(FATAL) << "InstBitParser cannot use decoder yet";
  }
};


} // namespace circuitous