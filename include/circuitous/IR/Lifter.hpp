/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Util/LLVMUtil.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Fuzz/InstNavigation.hpp>
#include <circuitous/Lifter/Shadows.hpp>
#include <circuitous/Lifter/SReg.hpp>
#include <circuitous/Lifter/ShadowMat.hpp>

#include <remill/Arch/Arch.h>
#include <remill/Arch/Instruction.h>
#include <remill/BC/Annotate.h>
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

namespace circ {

// Adds knowledge of a `shadowinst::Instruction` to a class that
// inherits from it.
struct WithShadow  {
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
      out.push_back(irops::Extract::create_fn(module, from, to));
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

  arch_ptr_t arch;

  llvm::Module *module;
  llvm::LLVMContext *llvm_ctx;
  llvm::IntegerType *word_type;
  uint64_t word_size = 0;
  uint64_t select_counter = 0;

  std::unordered_map< uint64_t, llvm::Value * > reg_op_dsts;

  InstructionLifter(arch_ptr_t arch_, llvm::Module *module_)
      : parent(arch_, remill::IntrinsicTable(module_)),
        arch(arch_),
        module(module_),
        llvm_ctx(&module_->getContext()),
        word_type(llvm::Type::getIntNTy(*llvm_ctx, arch_->address_size)),
        word_size(arch_->address_size)
  {}

  void SupplyShadow(shadowinst::Instruction *shadow_) {
    CHECK(!shadow) << "Shadow is already set, possible error";
    shadow = shadow_;
  }

  bool possible_isel(llvm::CallInst *c) {
    auto callee = c->getCalledFunction();
    if (!callee || !callee->hasName()) {
      return false;
    }
    auto name = callee->getName();
    return !name.startswith("__remill") && !name.startswith("__circuitous");
  }

  llvm::CallInst *fetch_sem_call(llvm::BasicBlock *block) {
    llvm::CallInst *out = nullptr;
    for (auto &inst : *block) {
      if (auto call = llvm::dyn_cast<llvm::CallInst>(&inst); call && possible_isel(call)) {
        CHECK(!out) << dbg_dump(block->getParent());
        out = call;
      }
    }
    return out;
  }


  using maybe_gep_t = std::optional< llvm::GetElementPtrInst * >;
  maybe_gep_t loads_from_state(llvm::LoadInst *load) {
    auto block = load->getParent();
    // NOTE(lukas): Since there is `PC` argument missing, we cannot use
    //              usual remill enum.
    auto state_ptr = remill::NthArgument(block->getParent(), 1);

    auto gep = llvm::dyn_cast<llvm::GetElementPtrInst>(load->getPointerOperand());
    if (!gep) {
      return {};
    }

    return (gep->getPointerOperand() == state_ptr) ? std::make_optional(gep) : nullptr;
  }

  struct cisel : remill::Semantics {
    using P = remill::Semantics;
    inline static const std::string metadata_value = P::metadata_value + ".cisel";
  };

  void consolidate_isel(llvm::Function *isel) {
    if (reg_op_dsts.empty()) {
      return;
    }

    // TODO(lukas): Will require some more complicated iteration down the line
    if (isel->size() != 1) {
      return;
    }

    // Is this one already transformed?
    if (remill::HasOriginType<cisel>(isel)) {
      return;
    }

    remill::Annotate<cisel>(isel);

    for (auto [idx, _] : reg_op_dsts) {
      consolidate_isel_(isel, idx);
    }
  }

  void consolidate_isel_(llvm::Function *isel, auto idx) {
    std::vector<llvm::StoreInst *> writes;
    auto val = remill::NthArgument(isel, 2 + idx);
    auto &s_reg = shadow->operands[idx].reg;

    // Get all stores that are storing into the destination operand
    for (auto &bb : *isel) {
      for (auto &inst : bb) {
        if (auto store = llvm::dyn_cast< llvm::StoreInst >(&inst)) {
          if (store->getPointerOperand() == val) {
            writes.push_back(store);
          }
        }
      }
    }

    CHECK(writes.size() == 1);

    // Continuing from the write into dst, collect all loads from the `State`
    // TODO(lukas): What about loads?
    using entry_t = std::tuple< llvm::LoadInst *, llvm::GetElementPtrInst * >;
    std::vector< entry_t > loads;
    for (auto &inst : make_range(writes.front(), isel->begin()->getTerminator())) {
      if (auto load = llvm::dyn_cast< llvm::LoadInst >(&inst)) {
        if (auto gep = loads_from_state(load)) {
          CHECK(*gep != nullptr);
          loads.emplace_back(load, *gep);
        }
      }
    }

    for (auto [load, gep] : loads) {
      auto enclosed = coerce_reg(load, reg_from_gep(gep, arch));
      CHECK(enclosed);

      auto name = enclosed->name;
      if (!s_reg->translation_map.count(name)) {
        continue;
      }

      s_reg->mark_dirty(name);

      llvm::IRBuilder<> ir(load);
      auto cond = shadowinst::make_explicit_decode(ir, *s_reg, name);
      auto select = irops::make< irops::Select >(ir, {cond, ir.CreateLoad(gep), ir.CreateLoad(val)});
      load->replaceAllUsesWith(select);
    }
  }

  auto LiftIntoBlock(remill::Instruction &inst, llvm::BasicBlock *block,
                     bool is_delayed) {
    auto lift_status = this->parent::LiftIntoBlock(inst, block, is_delayed);

    // If the instruction was not lifted correctly we do not wanna do anything
    // in the block.
    if (lift_status != remill::kLiftedInstruction) {
      return lift_status;
    }
    auto state_ptr = remill::NthArgument(block->getParent(), remill::kStatePointerArgNum);

    auto call = fetch_sem_call(block);
    CHECK(call);
    consolidate_isel(call->getCalledFunction());

    // We need to actually store the new value of instruction pointer
    // into the corresponding register.
    // NOTE(lukas): This should technically be responsiblity of remill::InstructionLifter
    //              but it is not happening for some reason.
    llvm::IRBuilder ir(block);
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
      const auto &[from, size] = irops::Extract::parse_args(fn);
      if (size == arch_op_size || (arch_op_size % size == 0 && size % 8 == 0 ) ) {
        set_candidate(fn);
      }
    }
    return canditate;
  }

  // Hide some constant value behind a call to intrinsic, so llvm cannot
  // optimize it away.
  llvm::Value *HideValue(llvm::Value *val, llvm::BasicBlock *bb, uint64_t size) {
    llvm::IRBuilder<> ir(bb);
    CHECK(size == val->getType()->getIntegerBitWidth());
    return irops::make< irops::InputImmediate >(ir, val);
  }

  template<typename I>
  llvm::Value *HideValue(llvm::BasicBlock *bb, I val) {
    llvm::IRBuilder<> ir(bb);
    auto as_constant = llvm::ConstantInt::get(word_type,
                                              static_cast<uint64_t>(val),
                                              std::is_signed_v<I>);
    return irops::make< irops::InputImmediate >(ir, as_constant);
  }

  llvm::Value *LiftOperand(remill::Instruction &inst, llvm::BasicBlock *bb,
                           llvm::Value *state_ptr, llvm::Argument *arg,
                           remill::Operand &op) override {
    auto out = this->parent::LiftOperand(inst, bb, state_ptr, arg, op);
    current_op += 1;
    return out;
  }

  // Compared to original lifter which almost alwyas produced a constant (or
  // at least a constant expression), here lifter needs to do more.
  // There are 2 main branches of what can happen:
  //  * Immediate has no shadow -> method returns original constant hidden
  //    behind intrinsic call to avoid subsequent llvm opts.
  //    e.g. instead of `i64 5` method return `__circuitous.input_immediate(i64 5)`
  //  * Immediate has a shadow -> method will try to reconstruct the immediate
  //    using instruction bytes.
  //    e.g. `__circuitous.input_immediate(__circuitous.extract.0.32())`
  //    Note that `extract` takes no argument -> by default it always references
  //    instruction bytes (we do not have them as object in llvm IR in the scope
  //    of the semantic function).
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
      if (arch_op.imm.is_signed)
        hidden_imm = ir.CreateSExtOrTrunc(hidden_imm, constant_imm->getType());
      else
        hidden_imm = ir.CreateZExtOrTrunc(hidden_imm, constant_imm->getType());
    }
    return HideValue(hidden_imm, bb, size);
  }

  std::string input_name(std::string_view name) {
    if (auto base = arch->RegisterByName(name)) {
      return base->EnclosingRegister()->name;
    }
    if (name == "NEXT_PC" || name.starts_with("__remill_zero")) {
      return std::string(name);
    }

    LOG(FATAL) << "Cannot locate input " << name;
  }

  llvm::Value *LiftRegisterOperand(remill::Instruction &inst,
                                   llvm::BasicBlock *bb,
                                   llvm::Value *state_ptr,
                                   llvm::Argument *arg,
                                   remill::Operand &op) override
  {
    auto lifted = LiftRegisterOperand_impl(inst, bb, state_ptr, arg, op);
    if (inst.operands[current_op].action == remill::Operand::kActionWrite) {
      reg_op_dsts[current_op] = lifted;
    }
    return lifted;
  }

  // If register does not have a shadow we can procees the same way default lifter
  // does. However, if a shadow is present we need to do more complex decision
  // mostly in form of `selectN` with possible `undef` that represents that a register
  // is not present for given encoding. See `LiftSReg` for more detailed info.
  // If register is being written into, we emit an extra pointer that represent a place
  // where the register "lives". Note that this is not the pointer into `State`,
  // but it is initialized appropriately (e.g. if we write into `RAX` then load
  // from this pointer will yield value of `RAX`).
  llvm::Value *LiftRegisterOperand_impl(
      remill::Instruction &inst, llvm::BasicBlock *bb,
      llvm::Value *state_ptr, llvm::Argument *arg, remill::Operand &op)
  {
    if (!CurrentShade().reg) {
      return this->parent::LiftRegisterOperand(inst, bb, state_ptr, arg, op);
    }

    const auto &s_reg = *CurrentShade().reg;

    auto concrete = this->parent::LiftRegisterOperand(inst, bb, state_ptr, arg, op);
    if (s_reg.size() == 0) {
      return concrete;
    }

    llvm::IRBuilder<> ir(bb);

    auto locate_reg = [&](auto &name) {
      return this->parent::LoadRegValue(bb, state_ptr, input_name(name));
    };

    auto selected = LiftSReg(bb, ir, s_reg, locate_reg);

    auto dst = [&](auto what) -> llvm::Value * {
      if (llvm::isa<llvm::PointerType>(concrete->getType())) {
        auto dst = irops::make< irops::AllocateDst >(
            ir, { state_ptr }, llvm::PointerType::getUnqual(what->getType()));
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

  // Lift register in a following way
  // `selector = concat(extract_regions())`
  // `reg_to_use = selectN(selector, r1, r2, ..., r(2^N))`
  template<typename Locator>
  llvm::Value *LiftSReg(llvm::BasicBlock *block,
                        llvm::IRBuilder<> &ir,
                        const shadowinst::Reg &s_reg,
                        Locator &&locate_reg)
  {
    auto safe_locate_reg = [&](auto &ir, auto &name) {
      auto retrieved = locate_reg(name);
      ir.SetInsertPoint(block);
      return retrieved;
    };

    // TODO(lukas): Handle holes.
    auto [cond, select] = shadowinst::make_intrinsics_decoder(s_reg, ir, safe_locate_reg);
    AddMetadata(select, "__circuitous.ordering", select_counter++);
    if (cond) {
      auto wrapped = irops::make< irops::Transport >(ir, cond);
      AddMetadata(llvm::dyn_cast<llvm::Instruction>(wrapped), "circuitous.verify_fn_args", 0);
    }

    if (s_reg.is_saturated_by_zeroes()) {
      return llvm::ConstantInt::get(select->getType(), 0, false);
    }
    return shadowinst::mask_shift_coerce(select, ir, s_reg, *arch);
  }


  llvm::Value *LiftSReg(llvm::BasicBlock *block,
                        llvm::Value *state_ptr,
                        const shadowinst::Reg &s_reg)
  {
    llvm::IRBuilder<> ir(block);
    auto zero = llvm::ConstantInt::get(word_type, 0, false);

    auto locate_reg = [&](auto &name) {
      return this->parent::LoadWordRegValOrZero(block, state_ptr, input_name(name), zero);
    };

    return LiftSReg(block, ir, s_reg, locate_reg);
  }

  // TODO(lukas): Port to remill, only differrence is the type of `zero`
  //              since remill::InstructionLifter expects `llvm::Constant`.
  llvm::Value *LoadWordRegValOrZero_(llvm::BasicBlock *block,
                                    llvm::Value *state_ptr,
                                    std::string_view reg_name,
                                    llvm::Value *zero) {

    if (reg_name.empty()) {
      return zero;
    }

    auto val = LoadRegValue(block, state_ptr, reg_name);
    auto val_type = llvm::dyn_cast_or_null<llvm::IntegerType>(val->getType());
    auto word_type = llvm::cast<llvm::IntegerType>(zero->getType());

    CHECK(val_type) << "Register " << reg_name << " expected to be an integer.";

    auto val_size = val_type->getBitWidth();
    auto word_size = word_type->getBitWidth();
    CHECK(val_size <= word_size)
        << "Register " << reg_name << " expected to be no larger than the "
        << "machine word size (" << word_type->getBitWidth() << " bits).";

    if (val_size < word_size) {
      val = new llvm::ZExtInst(val, word_type, llvm::Twine::createNull(), block);
    }

    return val;
  }


  template< uint32_t I >
  llvm::Value *LiftSReg(
      llvm::BasicBlock *block, llvm::Value *state_ptr,
      remill::Operand &r_op)
  {
    llvm::Value *zero = HideValue(llvm::ConstantInt::get(word_type, 0, false), block, word_size);

    auto &r_reg = ifuzz::get_reg< I >(r_op);
    if (!ifuzz::has_reg< I >(CurrentShade()))
      return LoadWordRegValOrZero_(block, state_ptr, r_reg.name, zero);

    auto &s_reg = ifuzz::get_reg< I >(CurrentShade());

    if (s_reg->empty()) {
      if (s_reg->translation_map.size() == 0) {
        return LoadWordRegValOrZero_(block, state_ptr, r_reg.name, zero);
      }
      CHECK_EQ(s_reg->translation_map.size(), 1);
      auto &entry = *s_reg->translation_map.begin();
      CHECK_EQ(entry.second.size(), 1);
      CHECK(entry.second.begin()->empty());
      return LoadWordRegValOrZero_(block, state_ptr, entry.first, zero);
    }
    return LiftSReg(block, state_ptr, *s_reg);
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

    auto arch_op_size = arch->address_size;
    auto inst_fn = ChooseImm(arch_op_size, ImmediateOperand(module, s_imm));
    // Similar situation as with empty map
    if (!inst_fn) {
      return HideValue(block, concrete_val);
    }
    llvm::IRBuilder<> ir(block);
    llvm::Value * hidden_imm = ir.CreateCall(inst_fn->getFunctionType(), inst_fn);

    auto expected_type = llvm::IntegerType::get(*llvm_ctx, static_cast<uint32_t>(arch_op_size));
    if (hidden_imm->getType() != expected_type) {
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

  // Lift basically the same way as default lifter, but use mechanism of this class
  // to lift both immediates and registers.
  llvm::Value *LiftAddressOperand(
      remill::Instruction &rinst, llvm::BasicBlock *block, llvm::Value *state_ptr,
      llvm::Argument *arg, remill::Operand &r_op) override
  {
    auto concrete = this->parent::LiftAddressOperand(rinst, block, state_ptr, arg, r_op);
    if (!CurrentShade().address) {
      return concrete;
    }
    auto base_reg = LiftSReg< ifuzz::sel::base >(block, state_ptr, r_op);
    auto index_reg = LiftSReg< ifuzz::sel::index >(block, state_ptr, r_op);

    // NOTE(lukas): In x86 16bit addresing mode it can happen, that
    //              we fuzz together [SI] and [BP + SI]. Neither will have shadow
    //              scale, but the second will have set remill scale to one (artifact
    //              of xed).
    auto concrete_scale = [&]() -> uint64_t {
      if (CurrentShade().address->scale->empty() &&
          !CurrentShade().address->index_reg->empty())
      {
        CHECK(CurrentShade().address->index_reg->regions ==
              CurrentShade().address->base_reg->regions);
        if (r_op.addr.scale != 1)
          LOG(WARNING) << "Overriding scale to 1 based on shadow info.";
        return 1ul;
      }
      return static_cast< uint64_t >(r_op.addr.scale);
    }();

    auto scale = LiftSImmediate(block, CurrentShade().address->scale, concrete_scale);
    CHECK(scale);
    auto displacement = LiftSImmediate(block, CurrentShade().address->displacement,
                                       r_op.addr.displacement);

    auto segment_reg = LiftSReg< ifuzz::sel::segment >(block, state_ptr, r_op);

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

struct OpaqueILifter : InstructionLifter {

  using InstructionLifter::InstructionLifter;


  llvm::Value *LiftOperand(remill::Instruction &inst, llvm::BasicBlock *bb,
                           llvm::Value *state_ptr, llvm::Argument *arg,
                           remill::Operand &op) override
  {
    // TODO(lukas): Determine when is `current_op` supposed to be incremented
    auto op_idx = this->current_op;
    auto out = this->InstructionLifter::LiftOperand(inst, bb, state_ptr, arg, op);
    if (inst.operands[op_idx].action == remill::Operand::kActionWrite) {
      return out;
    }

    llvm::IRBuilder<> irb(bb);
    auto dummy = irops::make_leaf< irops::Operand >(irb, op_idx, out->getType());
    auto wrap = irops::make< irops::AdviceConstraint >(irb, {out, dummy});
    AddMetadata(wrap, "circuitous.verify_fn_args", 0);

    return dummy;
  }
};

} // namespace circ
