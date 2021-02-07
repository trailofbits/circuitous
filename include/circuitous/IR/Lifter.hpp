/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

 #pragma once

#include <remill/BC/Lifter.h>

#include <circuitous/Util/LLVMUtil.hpp>

#include <cstdint>
#include <map>
#include <utility>
#include <vector>

namespace circuitous {

struct HideImms {
  // [from, size], regions cannot overlap!
  using imm_regions_t = std::map<uint64_t, uint64_t>;
  using op_imm_regions_t = std::map<remill::Operand *, imm_regions_t>;
  std::unordered_map<remill::Instruction *, op_imm_regions_t> regions = {};

  void AddImmRegions(remill::Instruction *ptr, op_imm_regions_t value) {
    // TODO(lukas): Not sure how we want to deal with this yet.
    if (regions.count(ptr)) {
      LOG(FATAL) << "Inserting new imm region map for present instruction";
    }
    regions[ptr] = std::move(value);
  }

  static std::string GetterName(uint64_t from, uint64_t size) {
    std::stringstream ss;
    ss << "__circuitous.get_imm." << from << "." << size;
    return ss.str();
  }

  using functions_t = std::vector<llvm::Function *>;
  // TODO(lukas): Technically We probably do not need the `remill::Instruction *`
  //              since every instruction has unique Operands; therefore having
  //              just remill::Operand * as keys should be enough.
  functions_t ImmGetters(llvm::Module *module, remill::Instruction *inst,
                         remill::Operand *op) {
    functions_t out;
    for (auto &[from, to] : regions[inst][op]) {
      out.push_back(ImmGetter(module, from, to));
    }
    return out;
  }

  llvm::Function *CreateGetter(llvm::Module *module, uint64_t from, uint64_t size) {
    LLVMTypes tys(module);
    auto fn_t = tys.FnTy(tys.Void(), {});
    auto callee = module->getOrInsertFunction(GetterName(from, size), fn_t);
    return llvm::cast<llvm::Function>(callee.getCallee());
  }

  llvm::Function *ImmGetter(llvm::Module *module, uint64_t from, uint64_t size) {
    auto name = GetterName(from, size);
    // Function is already present, therefore just return it;
    if (auto fn = module->getFunction(name)) {
      return fn;
    }
    return CreateGetter(module, from, size);
  }
};

struct InstructionLifter : remill::InstructionLifter, HideImms {
  using parent = remill::InstructionLifter;
  using parent::parent;

  using parent::LiftIntoBlock;

  auto LiftIntoBlock(remill::Instruction &inst, llvm::BasicBlock *block,
                     bool is_delayed, const op_imm_regions_t &imm_regions) {
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