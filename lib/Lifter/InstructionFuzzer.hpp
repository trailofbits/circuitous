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

  struct Comparator {
    using OpType = remill::Operand::Type;

    static bool Compare(const remill::Instruction &original,
                        const remill::Instruction &permutation,
                        const remill::Operand &op)
    {
      switch(op.type) {
        case OpType::kTypeImmediate: return Immediate(original, permutation, op);
        case OpType::kTypeRegister : return Register (original, permutation, op);
        default                    : return false;
      }
    }

    using cri = const remill::Instruction &;
    using cro = const remill::Operand &;

    static bool Immediate(cri original, cri permutation, cro op) {
      auto compare_self = [](auto &self, auto &flipped) {
        // TODO(lukas): Check in remill that is_signed is always properly
        //              (and consistently) set.
        // This may seem counterintuitive, but two immediates are equal
        // even if their values are not equal -- actaully that is the whole
        // point.
        return flipped.type == remill::Operand::kTypeImmediate
              && self.size == flipped.size;
      };
      return Check(original, permutation, op, compare_self);
    }

    static bool Register(cri original, cri permutation, cro op) {
      auto compare_self = [](auto &self, auto &flipped) {
        return flipped.type == OpType::kTypeRegister &&
              self.size == flipped.size;
      };
      return Check(original, permutation, op, compare_self);
    }

    template<typename Fn>
    static bool Check(cri original, cri permutation, cro op, Fn &&on_self) {
      auto [structural, exact] = CheckStructure(original, permutation, op, on_self);
      return structural && !exact;
    }



    // Custom comparator of two instructions
    // Two instructions `original` and `flipped` are equal if following holds:
    //  * their operands with exception of `skip` are equal
    //  * on_self returns true for the `skip` and its flipped counterpart
    // Return value consists of two result
    // { are instruction equivalent?, is the `skip` identical to `flipped`? }
    template<typename Fn>
    static std::tuple<bool, bool> CheckStructure(
      cri original, cri permutation, cro skip, Fn &&on_self)
    {
      if (original.function != permutation.function) {
        return { false, false };
      }

      if (original.operands.size() != permutation.operands.size()) {
        return { false, false };
      }

      auto compare = [](auto &lhs, auto &rhs) {
        return lhs.Serialize() == rhs.Serialize();
      };

      auto structural_check = true;
      auto exact_check = true;
      for (auto i = 0U; i < permutation.operands.size(); ++i) {
        if (&original.operands[i] == &skip) {
          exact_check = compare(skip, permutation.operands[i]);
          if (!on_self(skip, permutation.operands[i])) {
            return { false, exact_check };
          }
          continue;
        }

        if (!compare(original.operands[i], permutation.operands[i])) {
          structural_check &= false;
        }
      }
      return {structural_check, exact_check};
    }
  };
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

  const remill::Arch::ArchPtr &arch;

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

  auto FuzzOps(remill::Instruction &rinst) {
    shadowinst::Instruction shadow_inst;
    std::vector<std::vector<bool>> op_bits =
      { 3, std::vector<bool>(rinst.bytes.size() * 8, false) };

    auto permutations = permutate::Flip(rinst, arch);
    for (std::size_t i = 0; i < permutations.size(); ++i) {
      for (std::size_t op_i = 0; op_i < rinst.operands.size(); ++op_i) {
        LOG(INFO) << op_i;
        if (!permutations[i]) {
          continue;
        }
        using C = permutate::Comparator;
        op_bits[op_i][i] = C::Compare(rinst, *permutations[i], rinst.operands[op_i]);
      }
    }

    for (std::size_t i = 0; i < rinst.operands.size(); ++i) {
      auto &r_op = rinst.operands[i];
      switch(r_op.type) {
        case OpType::kTypeRegister:
          std::reverse(op_bits[i].begin(), op_bits[i].end());
          shadow_inst.Add<shadowinst::Reg>(op_bits[i]);
          PopulateTranslationTable(rinst, *(shadow_inst.operands.back().reg), i);
          break;
        case OpType::kTypeImmediate:
          std::reverse(op_bits[i].begin(), op_bits[i].end());
          shadow_inst.Add<shadowinst::Immediate>(op_bits[i]);
          break;
        default:
          break;
      }
    }
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