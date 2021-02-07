/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

 #pragma once

#include <remill/BC/Lifter.h>

#include <cstdint>
#include <map>
#include <utility>
#include <vector>

namespace circuitous {

struct HideImms {
  // [from, size], regions cannot overlap!
  using imm_regions_t = std::map<uint64_t, uint64_t>;
  std::unordered_map<remill::Instruction *, imm_regions_t> regions = {};

  void AddImmRegions(remill::Instruction *ptr, imm_regions_t value) {
    // TODO(lukas): Not sure how we want to deal with this yet.
    if (regions.count(ptr)) {
      LOG(FATAL) << "Inserting new imm region map for present instruction";
    }
    regions[ptr] = std::move(value);
  }
};

struct InstructionLifter : remill::InstructionLifter, HideImms {
  using parent = remill::InstructionLifter;
  using parent::parent;

  using parent::LiftIntoBlock;

  auto LiftIntoBlock(remill::Instruction &inst, llvm::BasicBlock *block,
                     bool is_delayed, const imm_regions_t &imm_regions) {
      this->AddImmRegions(&inst, imm_regions);
      return this->parent::LiftIntoBlock(inst, block, is_delayed);
  }

  llvm::Value *LiftImmediateOperand(remill::Instruction &inst, llvm::BasicBlock *bb,
                                    llvm::Argument *arg,
                                    remill::Operand &arch_op) override {
    // We run this to run some extra checks, but we do not care about result.
    return this->parent::LiftImmediateOperand(inst, bb, arg, arch_op);
  }
};

} // namespace circuitous