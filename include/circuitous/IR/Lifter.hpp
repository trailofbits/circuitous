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

struct ImmAsIntrinsics {
  // [from, size], regions cannot overlap!
  using imm_regions_t = std::map<uint64_t, uint64_t>;
  using op_imm_regions_t = std::map<remill::Operand *, imm_regions_t>;
  std::unordered_map<remill::Instruction *, op_imm_regions_t> regions = {};

  static constexpr const char *fn_prefix = "__circuitous.get_imm.";
  static constexpr char separator = '.';

  static std::string GetterName(uint64_t from, uint64_t size) {
    std::stringstream ss;
    ss << fn_prefix << from << separator << size;
    return ss.str();
  }

  static bool IsIntrinsic(llvm::Function *fn) {
    if (!fn->hasName() || !fn->isDeclaration()) {
      return false;
    }
    return fn->getName().startswith(fn_prefix);
  }

  using intrinsic_args_t = std::tuple<uint64_t, uint64_t>;
  static intrinsic_args_t ParseArgs(llvm::Function *fn) {
    CHECK(IsIntrinsic(fn))
      << "Cannot parse arguments of function: "
      << LLVMName(fn)
      << "that is not immediate intrinsic";
    llvm::StringRef name = fn->getName();
    name.consume_front(fn_prefix);
    const auto &[from, size] = name.split(separator);

    auto as_uint64_t = [](auto &str_ref) {
      uint64_t out;
      str_ref.getAsInteger(10, out);
      return out;
    };
    return {as_uint64_t(from), as_uint64_t(size)};
  }

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
    return this->parent::LiftImmediateOperand(inst, bb, arg, arch_op);
  }
};

} // namespace circuitous