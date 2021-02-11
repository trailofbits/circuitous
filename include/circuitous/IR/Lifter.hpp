/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

 #pragma once

#include <remill/Arch/Instruction.h>
#include <remill/BC/Lifter.h>
#include <remill/BC/Util.h>

#include <circuitous/Util/LLVMUtil.hpp>
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/IRBuilder.h>
#pragma clang diagnostic pop


#include <cstdint>
#include <map>
#include <utility>
#include <vector>

namespace circuitous {
namespace intrinsics {

template<typename Data>
struct Intrinsic_impl : Data {
  static bool IsIntrinsic(llvm::Function *fn) {
    if (!fn->hasName() || !fn->isDeclaration()) {
      return false;
    }
    return fn->getName().startswith(Data::fn_prefix);
  }
};

template<typename Data>
struct IntervalIntrinsic_impl : Intrinsic_impl<Data> {
  using parent = IntervalIntrinsic_impl<Data>;

  static std::string Name(uint64_t from, uint64_t size) {
    std::stringstream ss;
    ss << Data::fn_prefix << from << Data::separator << size;
    return ss.str();
  }

  static llvm::Function *CreateFn(llvm::Module *module, uint64_t from, uint64_t size) {
    llvm::IRBuilder<> ir(module->getContext());
    auto r_ty = ir.getIntNTy(static_cast<uint32_t>(size));
    auto fn_t = llvm::FunctionType::get(r_ty, {}, true);
    auto callee = module->getOrInsertFunction(Name(from, size), fn_t);
    return llvm::cast<llvm::Function>(callee.getCallee());
  }

  using intrinsic_args_t = std::tuple<uint64_t, uint64_t>;
  static intrinsic_args_t ParseArgs(llvm::Function *fn) {
    CHECK(parent::IsIntrinsic(fn))
      << "Cannot parse arguments of function: "
      << LLVMName(fn)
      << "that is not our intrinsic.";
    llvm::StringRef name = fn->getName();
    name.consume_front(Data::fn_prefix);
    const auto &[from, size] = name.split(Data::separator);

    auto as_uint64_t = [](auto &str_ref) {
      uint64_t out;
      str_ref.getAsInteger(10, out);
      return out;
    };
    return {as_uint64_t(from), as_uint64_t(size)};
  }
};

template<typename Data>
struct BitCompare_impl : Intrinsic_impl<Data> {
  using parent = Intrinsic_impl<Data>;

  static std::string Name(uint64_t size) {
    std::stringstream ss;
    ss << Data::fn_prefix << Data::separator << size;
    return ss.str();
  }

  static llvm::Function *CreateFn(llvm::Module *module, uint64_t size) {
    llvm::IRBuilder<> ir(module->getContext());
    auto r_ty = ir.getInt1Ty();
    auto arg_ty = ir.getIntNTy(static_cast<uint32_t>(size));
    auto fn_t = llvm::FunctionType::get(r_ty, {arg_ty, arg_ty}, false);
    auto callee = module->getOrInsertFunction(Name(size), fn_t);
    return llvm::cast<llvm::Function>(callee.getCallee());
  }

  using intrinsic_args_t = uint64_t;
  static intrinsic_args_t ParseArgs(llvm::Function *fn) {
    CHECK(parent::IsIntrinsic(fn))
      << "Cannot parse arguments of function: "
      << LLVMName(fn)
      << "that is not our intrinsic.";

    llvm::StringRef name = fn->getName();
    name.consume_front(Data::fn_prefix);
    name.consume_front(Data::separator);

    uint64_t out;
    name.getAsInteger(10, out);
    return out;
  }
};

struct ExtractData {
  static constexpr const char *fn_prefix = "__circuitous.extract";
  static constexpr const char separator = '.';
};

struct BitCompareData {
  static constexpr const char *fn_prefix = "__circuitous.bitcompare";
  static constexpr const char separator = '.';
};

using Extract = IntervalIntrinsic_impl<ExtractData>;
using BitCompare = BitCompare_impl<BitCompareData>;

} // namespace intrinsics

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
    auto module = bb->getModule();
    auto imm_getters = GetImmediates(module, &inst, &arch_op);
    CHECK(imm_getters.size() == 1);
    LOG(INFO) << "Would call: " << LLVMName(*imm_getters.begin());
    auto constant_imm = this->parent::LiftImmediateOperand(inst, bb, arg, arch_op);

    auto inst_fn = *(imm_getters.begin());
    llvm::IRBuilder<> ir(bb);
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