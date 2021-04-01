/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

 #pragma once

#include <circuitous/Util/LLVMUtil.hpp>
#include <circuitous/IR/Intrinsics.hpp>
#include <circuitous/Lifter/Shadows.hpp>

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
    // We already have some immediate regions associated with this instruction
    // - while adding the new ones can be a valid use case it is not atm.
    // Also it is not clear what the effect on the existing should be
    // union, replace, something else?
    if (regions.count(ptr)) {
      LOG(FATAL) << "Inserting new imm region map for present instruction";
    }
    regions[ptr] = std::move(value);
  }

  using functions_t = std::vector<llvm::Function *>;
  // NOTE(lukas): Technically We probably do not need the `remill::Instruction *`
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

struct WithShadow : public intrinsics::Extract {
  // TODO(lukas): We do not have it at the time of the construction (yet at least)
  //              so we cannot take it by `const &`.
  shadowinst::Instruction *shadow = nullptr;
  // TODO(lukas): Synchronizes the operands to be lifted with the shadow instruction.
  //              It is not the best solution, but it is not invasive with respect
  //              to the parent class.
  uint8_t current_op = 0;

  auto &CurrentShade() { return shadow->operands[current_op]; }

  auto ImmediateOperand(llvm::Module *module) {
    auto &current = CurrentShade();
    CHECK(current.immediate);
    CHECK(current.immediate->size() >= 1);

    std::vector<llvm::Function *> out;
    for (auto [from, to] : *current.immediate) {
      out.push_back(CreateFn(module, from, to));
    }
    return out;
  }

  auto RegisterOperand(llvm::Module *module) {
    auto &current = CurrentShade();
    CHECK(current.reg);
    CHECK(current.reg->regions.size() == 1);
  }
};

struct InstructionLifter : remill::InstructionLifter, ImmAsIntrinsics, WithShadow {
  using parent = remill::InstructionLifter;
  using parent::parent;

  using parent::LiftIntoBlock;


  void SupplyShadow(shadowinst::Instruction *shadow_) {
    CHECK(!shadow) << "Shadow is already set, possible error";
    shadow = shadow_;
  }

  auto LiftIntoBlock(remill::Instruction &inst, llvm::BasicBlock *block,
                     bool is_delayed, const op_imm_regions_t &imm_regions) {
      this->AddImmRegions(&inst, imm_regions);
      auto lift_status = this->parent::LiftIntoBlock(inst, block, is_delayed);

      // If the instruction was not lifted correctly we do not wanna do anything
      // in the block.
      if (lift_status != remill::kLiftedInstruction) {
        return lift_status;
      }

      // We need to actually store the new value of instruction pointer
      // into the corresponding register.
      // NOTE(lukas): This should technically be responsiblity of remill::InstructionLifter
      //              but it is not happening for some reason.
      llvm::IRBuilder ir(block);
      auto state_ptr = remill::NthArgument(block->getParent(), remill::kStatePointerArgNum);
      auto pc_ref = LoadRegAddress(block, state_ptr, remill::kPCVariableName);
      auto next_pc_ref = LoadRegAddress(block, state_ptr, remill::kNextPCVariableName);
      ir.CreateStore(ir.CreateLoad(next_pc_ref), pc_ref);
      return lift_status;
  }

  llvm::Function *ChooseImm(remill::Operand &arch_op, const functions_t &funcs) {
    CHECK(funcs.size());
    if (funcs.size() == 1) {
      return funcs[0];
    }
    llvm::Function *canditate = nullptr;
    // TODO(lukas): From all candidates we need to choose one - currently
    //              there is no mechanism how to compare the candidates themselves
    //              if more than one passes the criteria we cannot choose.
    //              Ideally we would want to have some ranking, but currently I have
    //              no idea how such thing could look like.
    auto set_candidate = [&](auto fn) {
      CHECK(!canditate);
      canditate = fn;
    };

    for (auto fn : funcs) {
      const auto &[from, size] = this->ParseArgs(fn);
      if (size == arch_op.size || (arch_op.size % size == 0 && size % 8 == 0 ) ) {
        set_candidate(fn);
      }
    }
    return canditate;
  }

  llvm::Value *HideValue(llvm::Value *val, llvm::BasicBlock *bb, uint64_t size) {
    llvm::IRBuilder<> ir(bb);
    return ir.CreateCall(intrinsics::InputImmediate::CreateFn(bb->getModule(), size), val);
  }

  llvm::Value *LiftOperand(remill::Instruction &inst, llvm::BasicBlock *bb,
                           llvm::Value *state_ptr, llvm::Argument *arg,
                           remill::Operand &op) override {
    LOG(INFO) << "LiftOperand: " << static_cast<uint16_t>(current_op);
    auto out = this->parent::LiftOperand(inst, bb, state_ptr, arg, op);
    current_op += 1;
    return out;
  }

  llvm::Value *LiftImmediateOperand(remill::Instruction &inst, llvm::BasicBlock *bb,
                                    llvm::Argument *arg,
                                    remill::Operand &arch_op) override {
    // We run this to run some extra checks, but we do not care about result.
    llvm::IRBuilder<> ir(bb);
    auto module = bb->getModule();

    auto constant_imm = this->parent::LiftImmediateOperand(inst, bb, arg, arch_op);

    auto size = llvm::cast<llvm::IntegerType>(constant_imm->getType())->getScalarSizeInBits();
    // If there is no shadow - or there is a shadow but it does not have size,
    // just return the original value hidden behind intrinsic
    if (!CurrentShade().immediate || CurrentShade().immediate->size() == 0) {
      return HideValue(constant_imm, bb, size);
    }

    auto imm_getters = ImmediateOperand(module);
    auto inst_fn = ChooseImm(arch_op, imm_getters);
    // Similar situation as with empty map
    if (!inst_fn) {
      return HideValue(constant_imm, bb, size);
    }
    llvm::Value * hidden_imm = ir.CreateCall(inst_fn->getFunctionType(), inst_fn);

    if (hidden_imm->getType() != constant_imm->getType()) {
      LOG(INFO) << "Coercing immediate operand of type"
                << remill::LLVMThingToString(hidden_imm->getType())
                << "to type"
                << remill::LLVMThingToString(constant_imm->getType());
      // NOTE(lukas): SExt used as it should be generally safer (we want to preserve)
      //              the sign bit.
      hidden_imm = ir.CreateSExtOrTrunc(hidden_imm, constant_imm->getType());
    }
    return HideValue(hidden_imm, bb, size);
  }

  llvm::Value *LiftRegisterOperand(remill::Instruction &inst,
                                   llvm::BasicBlock *bb,
                                   llvm::Value *state_ptr,
                                   llvm::Argument *arg,
                                   remill::Operand &op) override {
    if (!CurrentShade().reg) {
      return this->parent::LiftRegisterOperand(inst, bb, state_ptr, arg, op);
    }

    // We need to convert the hardcoded values of our translation table into
    // `llvm::APInt` and to do that we need to have them as `std::string`.
    auto as_string = [](auto full, auto from, auto size) {
      std::string out;
      for (uint64_t i = 0; i < size; ++i) {
        out += (full[from + i]) ? '1' : '0';
      }
      return out;
    };

    auto module = bb->getModule();

    // We decoded exactly one form
    // NOTE(lukas): This may be redundat - not sure yet
    std::vector<llvm::Value *> xor_ops;

    const auto &s_reg = CurrentShade().reg;
    auto concrete = this->parent::LiftRegisterOperand(inst, bb, state_ptr, arg, op);

    if (s_reg->size() == 0) {
      return concrete;
    }
    // Load register value -- even for pointer types! Since we are not really
    // storing anything anywhere we do not care about pointers - we care about
    // the value inside though.
    // Allow only some types now.
    auto locate_reg = [&](auto name) {
      if (llvm::isa<llvm::PointerType>(concrete->getType())) {
        return this->parent::LoadRegValue(bb, state_ptr, name);
      }
      if (llvm::isa<llvm::IntegerType>(concrete->getType())) {
        return this->parent::LoadRegValue(bb, state_ptr, name);
      }
      LOG(FATAL) << "Cannot locate " << name << " with type "
                 << remill::LLVMThingToString(concrete->getType());
    };

    llvm::IRBuilder<> ir(bb);
    llvm::Value *prev_select = llvm::UndefValue::get(concrete->getType());

    std::vector<llvm::Value *> fragment_checks;
    for (auto &[reg, all_mats] : s_reg->translation_map) {
      // TODO(lukas): To make our lived easier for now
      CHECK(all_mats.size() == 1);
      const auto &mats = *(all_mats.begin());

      std::size_t current = 0;
      std::vector<llvm::Value *> reg_checks;
      for (auto &[from, size] : s_reg->regions) {
        auto extract_fn = intrinsics::Extract::CreateFn(module, from, size);
        auto fragments = ir.CreateCall(extract_fn, {}, "fragments");
        auto constant = ir.getInt(
          llvm::APInt(static_cast<uint32_t>(size), as_string(mats, current, size), 2));
        auto eq = ir.CreateICmpEQ(fragments, constant, "frag_check");

        reg_checks.push_back(eq);
        // After we use `locate_reg` we need to update the builder since some extra
        // instrucitons may have been inserted.

        current += size;
      }

      auto reg_var = locate_reg(reg);
      ir.SetInsertPoint(bb);

      // NOTE(lukas): I am avoiding introducing extra intrinsics that does
      //              n-ary `and` on purpose - I would prefer to keep number
      //              of intrinsics to minimum.
      CHECK(reg_checks.size() > 0);
      llvm::Value *eq = ir.getTrue();
      for (auto fragment : reg_checks) {
        eq = ir.CreateAnd(eq, fragment);
      }

      prev_select = ir.CreateSelect(eq, reg_var, prev_select);
      fragment_checks.push_back(eq);
    }
    auto xor_all = intrinsics::make_xor(ir, fragment_checks);
    AddMetadata(xor_all, "circuitous.verify_fn_args", current_op);


    auto dst = [&]() -> llvm::Value *{
      if (llvm::isa<llvm::PointerType>(concrete->getType())) {
        auto alloca_fn = intrinsics::AllocateDst::CreateFn(
          module,
          llvm::PointerType::getUnqual(prev_select->getType()));
        auto dst = ir.CreateCall(alloca_fn, {}, "DST_" + std::to_string(current_op));

        // Add metadata so someone down the line can identify the alloca (and its
        // mapping to the operand if needed).
        AddMetadata(dst, "circuitous.dst.reg", current_op);
        ir.CreateStore(prev_select, dst);
        return dst;
      }
      return prev_select;
    }();

    LOG(INFO) << remill::LLVMThingToString(dst);
    return dst;
  }

};

} // namespace circuitous