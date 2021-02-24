/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

 #pragma once

#include <circuitous/Util/LLVMUtil.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <remill/Arch/Instruction.h>
#include <remill/BC/Lifter.h>
#include <remill/BC/Util.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#pragma clang diagnostic pop

#include <cstdint>
#include <map>
#include <utility>
#include <vector>

namespace circuitous {

struct ImmAsIntrinsics : public intrinsics::Extract {
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

  using functions_t = std::vector<llvm::Function *>;
  // TODO(lukas): Technically We probably do not need the `remill::Instruction *`
  //              since every instruction has unique Operands; therefore having
  //              just remill::Operand * as keys should be enough.
  functions_t GetImmediates(llvm::Module *module, remill::Instruction *inst,
                            remill::Operand *op) {
    functions_t out;
    for (auto &[from, to] : regions[inst][op]) {
      out.push_back(CreateFn(module, from, to));
    }
    return out;
  }
};

struct InstructionLifter : remill::InstructionLifter, ImmAsIntrinsics {
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
    llvm::IRBuilder<> ir(bb);
    auto module = bb->getModule();

    auto constant_imm = this->parent::LiftImmediateOperand(inst, bb, arg, arch_op);
    auto imm_getters = GetImmediates(module, &inst, &arch_op);

    if (imm_getters.size() == 0) {
      return constant_imm;
    }
    CHECK(imm_getters.size() == 1);
    LOG(INFO) << "Would call: " << LLVMName(*imm_getters.begin());


    auto inst_fn = *(imm_getters.begin());

    auto hidden_imm = ir.CreateCall(inst_fn->getFunctionType(), inst_fn);
    if (hidden_imm->getType() != constant_imm->getType())
    {
      LOG(INFO) << remill::LLVMThingToString(hidden_imm->getType())
                << " is not what a constant type would be: "
                << remill::LLVMThingToString(constant_imm->getType())
                << ". Coercion inserted";
      return ir.CreateSExtOrTrunc(hidden_imm, constant_imm->getType());
    }
    return hidden_imm;
  }
};

} // namespace circuitous