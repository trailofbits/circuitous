/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <remill/Arch/Instruction.h>

#include <string>
#include <vector>
#include <map>
#include <unordered_map>

namespace circuitous {

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

  static bool CompareOps(const remill::Operand &lhs, const remill::Operand &rhs) {
    return lhs.Serialize() == rhs.Serialize();
  }

  bool HasSomeImm(Instruction *inst) {
    for (auto &op : inst->operands) {
      if (op.type == remill::Operand::kTypeImmediate) {
        return true;
      }
    }
    return false;
  }

  // Custom comparator of two instructions
  // Two instructions `original` and `flipped` are equal if following holds:
  //  * their operands with exception of `skip` are equal
  //  * on_self returns true for the `skip` anf its flipped counterpart
  template<typename Fn>
  bool CheckFlip(Instruction &original, Instruction &flipped,
                 Operand &skip, Fn &&on_self) {
      if (original.operands.size() != flipped.operands.size()) {
        return false;
      }

      for (auto i = 0U; i < flipped.operands.size(); ++i) {
        if (&original.operands[i] == &skip) {
          if (!on_self(skip, flipped.operands[i])) {
            return false;
          }
          continue;
        }

        if (!CompareOps(original.operands[i], flipped.operands[i])) {
          return false;
        }
      }
      return true;
  }

  template<typename Fn>
  std::vector<bool> Permutate(Instruction &rinst, Operand &op, Fn &&on_self) {
    std::vector<bool> bits(rinst.bytes.size() * 8, 0);
    for (auto i = 0U; i < rinst.bytes.size(); ++i) {
      for (auto j = 0U; j < 8; ++j) {
        remill::Instruction tmp;
        std::string flipped_bit = rinst.bytes;
        auto byte = static_cast<uint8_t>(flipped_bit[ i ]);
        uint8_t mask = 1;
        flipped_bit[i] = static_cast< char >(byte ^ (mask << j));
        if (!arch->DecodeInstruction(0, flipped_bit, tmp)) {
          bits[i * 8 + j] = 0;
          continue;
        }
        bits[i * 8 + j] = CheckFlip(rinst, tmp, op, on_self);
      }
    }
    return bits;
  }

  std::tuple<uint64_t, uint64_t> Adjust(Instruction &rinst,
                                        uint64_t from, uint64_t size) {
    auto rinst_bit_size = rinst.bytes.size() * 8;
    return {rinst_bit_size - from - size, size};
  }

  imm_meta_t ProcessResult(Instruction &rinst, Operand &op, std::vector<bool> bits) {
    imm_meta_t regions;
    for (auto i = 0U; i < bits.size(); ++i) {
      if (!bits[i]) {
        continue;
      }
      uint64_t offset = i;
      uint32_t count = 0;
      for(; i < bits.size() && bits[i]; ++i) {
        ++count;
      }
      // NOTE(lukas): We need to flip this a bit since the ordering of instruction
      //              in the circuit itself will be reversed of that in
      //              `remill::Instruction::bytes`.
      const auto &[from, size] = Adjust(rinst, offset, count);
      regions.emplace(from, size);
    }
    return regions;
  }

  imm_meta_t PermutateImm(Instruction &rinst, Operand &op) {
    if (op.type != OpType::kTypeImmediate) {
      return {};
    }
    auto compare_self = [](auto &self, auto &flipped) {
      return flipped.type == remill::Operand::kTypeImmediate &&
             !(self.imm.val == flipped.imm.val &&
               self.imm.is_signed != flipped.imm.is_signed);
    };
    return ProcessResult(rinst, op, Permutate(rinst, op, compare_self));
  }

  imm_meta_list_t FuzzOps(remill::Instruction &rinst) {
    imm_meta_list_t imms;

    for (auto &op : rinst.operands) {
      LOG(INFO) << ToString(op.type);
      if (op.type == remill::Operand::kTypeAddress) {
        LOG(INFO) << ToString(op.addr.kind);
      }
      if (op.type == remill::Operand::kTypeImmediate) {
        imms.emplace(&op, PermutateImm(rinst, op));
      }
    }

    // TODO(lukas): Remove as it serves only for debug purposes.
    for (auto &[op, regions] : imms) {
      LOG(INFO) << &op << " -> " << op->Serialize();
      for (auto &[from, size] : regions ) {
        LOG(INFO) << "\t[ " << from << ", " << from + size << " ] " << size;
      }
    }
    return imms;
  }


  void DecodeRegions(Instruction *inst) {
    LOG(FATAL) << "InstBitParser cannot use decoder yet";
  }
};


} // namespace circuitous