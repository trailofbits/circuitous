/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

 #pragma once

#include <circuitous/Util/LLVMUtil.hpp>
#include <circuitous/IR/Intrinsics.hpp>
#include <circuitous/Lifter/Shadows.hpp>
#include <circuitous/Lifter/ShadowMat.hpp>

#include <remill/Arch/Arch.h>
#include <remill/Arch/Instruction.h>
#include <remill/BC/IntrinsicTable.h>
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

struct WithShadow : public intrinsics::Extract {
  // TODO(lukas): We do not have it at the time of the construction (yet at least)
  //              so we cannot take it by `const &`.
  shadowinst::Instruction *shadow = nullptr;
  // TODO(lukas): Synchronizes the operands to be lifted with the shadow instruction.
  //              It is not the best solution, but it is not invasive with respect
  //              to the parent class.
  uint8_t current_op = 0;

  auto &CurrentShade() { return shadow->operands[current_op]; }

  auto ImmediateOperand(llvm::Module *module, const shadowinst::Immediate &s_imm) {
    auto &current = s_imm;
    CHECK(current.size() >= 1);

    std::vector<llvm::Function *> out;
    for (auto [from, to] : current) {
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

struct InstructionLifter : remill::InstructionLifter, WithShadow {
  using parent = remill::InstructionLifter;
  using arch_ptr_t = const remill::Arch *;

  using functions_t = std::vector<llvm::Function *>;

  using parent::LiftIntoBlock;

  llvm::Module *module;
  llvm::LLVMContext *llvm_ctx;
  llvm::IntegerType *word_type;
  uint64_t word_size = 0;

  InstructionLifter(arch_ptr_t arch_, llvm::Module *module_)
      : parent(arch_, remill::IntrinsicTable(module_)),
        module(module_),
        llvm_ctx(&module_->getContext()),
        word_type(llvm::Type::getIntNTy(*llvm_ctx, arch_->address_size)),
        word_size(arch_->address_size)
  {}

  void SupplyShadow(shadowinst::Instruction *shadow_) {
    CHECK(!shadow) << "Shadow is already set, possible error";
    shadow = shadow_;
  }

  auto LiftIntoBlock(remill::Instruction &inst, llvm::BasicBlock *block,
                     bool is_delayed) {
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

  llvm::Function *ChooseImm(uint64_t arch_op_size, const functions_t &funcs) {
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
      if (size == arch_op_size || (arch_op_size % size == 0 && size % 8 == 0 ) ) {
        set_candidate(fn);
      }
    }
    return canditate;
  }

  llvm::Value *HideValue(llvm::Value *val, llvm::BasicBlock *bb, uint64_t size) {
    llvm::IRBuilder<> ir(bb);
    return ir.CreateCall(intrinsics::InputImmediate::CreateFn(bb->getModule(), size), val);
  }

  template<typename I>
  llvm::Value *HideValue(llvm::BasicBlock *bb, I val) {
    llvm::IRBuilder<> ir(bb);
    auto as_constant = llvm::ConstantInt::get(word_type,
                                              static_cast<uint64_t>(val),
                                              std::is_signed_v<I>);
    auto intrinsic = intrinsics::InputImmediate::CreateFn(bb->getModule(), word_type);
    return ir.CreateCall(intrinsic, { as_constant });
  }

  llvm::Value *LiftOperand(remill::Instruction &inst, llvm::BasicBlock *bb,
                           llvm::Value *state_ptr, llvm::Argument *arg,
                           remill::Operand &op) override {
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

    auto inst_fn = ChooseImm(arch_op.size, ImmediateOperand(module, *CurrentShade().immediate));
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

    const auto &s_reg = CurrentShade().reg;

    auto concrete = this->parent::LiftRegisterOperand(inst, bb, state_ptr, arg, op);
    if (s_reg->size() == 0) {
      return concrete;
    }

    // Load register value -- even for pointer types! Since we are not really
    // storing anything anywhere we do not care about pointers - we care about
    // the value inside though.
    // Allow only some types now.
    // NOTE(lukas): `LoadRegValue` can modify underlying bitcode thus invalidating
    //              IRBuilders we work with!
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

    // We need to bump the ir builder after the instructions injected
    // by `locate_reg`.
    auto safe_locate_reg = [&](auto name, auto &ir) {
      auto retrieved = locate_reg(name);
      ir.SetInsertPoint(bb);
      return retrieved;
    };

    llvm::IRBuilder<> ir(bb);

    auto [xor_all, selected] = shadowinst::make_decoder_selects(*s_reg, ir, safe_locate_reg);
    AddMetadata(xor_all, "circuitous.verify_fn_args", current_op);


    auto dst = [&](auto what) -> llvm::Value * {
      if (llvm::isa<llvm::PointerType>(concrete->getType())) {
        auto dst = intrinsics::make_alloca(ir, llvm::PointerType::getUnqual(what->getType()));
        dst->setName("DST_" + std::to_string(current_op));

        // Add metadata so someone down the line can identify the alloca (and its
        // mapping to the operand if needed).
        AddMetadata(dst, "circuitous.dst.reg", current_op);
        ir.CreateStore(what, dst);
        return ir.CreateBitCast(dst, concrete->getType());
      }
      CHECK(what->getType()->isIntOrIntVectorTy() && concrete->getType()->isIntOrIntVectorTy());
      return ir.CreateSExtOrTrunc(what, concrete->getType());
    };
    return dst(selected);
  }

  llvm::Value *LiftSReg(llvm::BasicBlock *block,
                        llvm::Value *state_ptr,
                        const shadowinst::Reg &s_reg)
  {
    llvm::IRBuilder<> ir(block);
    auto zero = llvm::ConstantInt::get(word_type, 0, false);

    auto locate_reg = [&](auto name) {
      return this->parent::LoadWordRegValOrZero(block, state_ptr, name, zero);
    };


    // We need to bump the ir builder after the instructions injected
    // by `locate_reg`.
    auto safe_locate_reg = [&](auto name, auto &ir) {
      auto retrieved = locate_reg(name);
      ir.SetInsertPoint(block);
      return retrieved;
    };

    auto [xor_all, selected] = shadowinst::make_decoder_selects(s_reg, ir, safe_locate_reg);
    // Nothing was there to be chained together!
    if (!selected) {
      return this->parent::LoadWordRegValOrZero(block, state_ptr, "", zero);
    }

    AddMetadata(xor_all, "circuitous.verify_fn_args", current_op);
    return selected;
  }


  llvm::Value *LiftSReg(
      llvm::BasicBlock *block, llvm::Value *state_ptr,
      const std::optional<shadowinst::Reg> &maybe_s_reg,
      const remill::Operand &r_op)
  {
    auto zero = llvm::ConstantInt::get(word_type, 0, false);
    if (!maybe_s_reg) {
      return this->LoadWordRegValOrZero(block, state_ptr, r_op.reg.name, zero);
    }
    return LiftSReg(block, state_ptr, *maybe_s_reg);
  }

  template<typename I>
  llvm::Value *LiftSImmediate(
      llvm::BasicBlock *block, const std::optional<shadowinst::Immediate> &maybe_s_imm,
      I concrete_val)
  {
    if (!maybe_s_imm || maybe_s_imm->size() == 0) {
      return HideValue(block, concrete_val);
    }
    auto &s_imm = *maybe_s_imm;

    auto arch_op_size = sizeof(I) * 8;
    auto inst_fn = ChooseImm(arch_op_size, ImmediateOperand(module, s_imm));
    // Similar situation as with empty map
    if (!inst_fn) {
      return HideValue(block, concrete_val);
    }
    llvm::IRBuilder<> ir(block);
    llvm::Value * hidden_imm = ir.CreateCall(inst_fn->getFunctionType(), inst_fn);

    auto expected_type = llvm::IntegerType::get(*llvm_ctx, static_cast<uint32_t>(arch_op_size));
    if (hidden_imm->getType() != expected_type) {
      LOG(INFO) << "Coercing immediate operand of type"
                << remill::LLVMThingToString(hidden_imm->getType())
                << "to type"
                << remill::LLVMThingToString(expected_type);
      // NOTE(lukas): SExt used as it should be generally safer (we want to preserve)
      //              the sign bit.
      if constexpr (std::is_signed_v< I >) {
        hidden_imm = ir.CreateSExtOrTrunc(hidden_imm, expected_type);
      } else {
        hidden_imm = ir.CreateZExtOrTrunc(hidden_imm, expected_type);
      }
    }
    auto x = HideValue(hidden_imm, block, arch_op_size);
    return x;
  }

  llvm::Value *LiftAddressOperand(
      remill::Instruction &rinst, llvm::BasicBlock *block, llvm::Value *state_ptr,
      llvm::Argument *arg, remill::Operand &r_op) override
  {
    auto concrete = this->parent::LiftAddressOperand(rinst, block, state_ptr, arg, r_op);
    if (!CurrentShade().address) {
      return concrete;
    }
    auto &s_base_reg = CurrentShade().address->base_reg;
    auto base_reg = LiftSReg(block, state_ptr, s_base_reg, r_op);
    auto index_reg = LiftSReg(block, state_ptr, CurrentShade().address->index_reg, r_op);
    CHECK(index_reg);

    auto scale = LiftSImmediate(
        block,
        CurrentShade().address->scale,
        static_cast<uint64_t>(r_op.addr.scale));
    CHECK(scale);
    auto displacement = LiftSImmediate(block, CurrentShade().address->displacement, r_op.addr.displacement);

    auto zero = llvm::ConstantInt::get(word_type, 0, false);
    auto segment_reg = this->parent::LoadWordRegValOrZero(
        block, state_ptr, r_op.addr.segment_base_reg.name, zero);

    llvm::IRBuilder<> ir(block);

    llvm::Value *out = ir.CreateAdd(segment_reg, base_reg);
    auto scale_shift = ir.CreateShl(ir.getIntN(static_cast<uint32_t>(word_size), 1), scale);
    auto scale_factor = ir.CreateMul(scale_shift, index_reg);
    out = ir.CreateAdd(out, scale_factor);
    out = ir.CreateAdd(out, displacement);

    if (r_op.addr.address_size < word_size) {
      auto new_t = llvm::Type::getIntNTy(*llvm_ctx, static_cast<uint32_t>(r_op.addr.address_size));
      return ir.CreateZExt(ir.CreateTrunc(out, new_t), word_type);
    }
    return out;
  }
};

} // namespace circuitous