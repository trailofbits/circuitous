/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include "CircuitBuilder.h"
#include "DependencyVisitor.hpp"

#include "Flatten.hpp"

#include <circuitous/Lifter/BaseLifter.hpp>
#include <circuitous/Lifter/SelectFold.hpp>
#include <circuitous/Lifter/Error.hpp>
#include <circuitous/Lifter/Memory.hpp>
#include <circuitous/IR/Lifter.hpp>

#include <remill/BC/Compat/CallSite.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <gflags/gflags.h>
#include <glog/logging.h>
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Verifier.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
#include <llvm/Transforms/Utils/Cloning.h>
#pragma clang diagnostic pop

#include <sstream>

namespace circ {
namespace {

// `v.name = u.name`
static inline void CopyName(llvm::Value *v, llvm::Value *u) {
  v->setName(u->getName());
}

static inline void CopyArgName(
    llvm::Function *f, llvm::Function* g, std::size_t i)
{
  return CopyName(remill::NthArgument(f, i), remill::NthArgument(g, i));
}

// TODO(pag): Add other architecture flag names here.
static const std::string kFlagRegisters =
    ",SF,OF,PF,AF,ZF,CF"  // x86, amd64.
    ",N,Z,C,V"  // AArch64.
    ",icc_c,icc_v,icc_z,icc_n"  // SPARCv8.
    ",xcc_c,xcc_v,xcc_z,xcc_n"  // SPARCv9.
    ",";

// Return an integral type that is big enough to hold any value can inhabit the
// register associated with `reg`.
static llvm::IntegerType *IntegralRegisterType(llvm::Module &module,
                                               const remill::Register *reg) {
  if (reg->type->isIntegerTy()) {
    // Optimization for flag registers, which should only occupy a single
    // bit. We look to see if it's in the set of
    if (auto found_at = kFlagRegisters.find(reg->name);
        found_at != std::string::npos && 0 < found_at &&
        (found_at + 1u) < kFlagRegisters.size() &&
        kFlagRegisters[found_at - 1u] == ',' &&
        kFlagRegisters[found_at + reg->name.size()] == ',') {

      return llvm::Type::getInt1Ty(module.getContext());
    }
    return llvm::dyn_cast<llvm::IntegerType>(reg->type);
  }
  return llvm::Type::getIntNTy(
      module.getContext(),
      static_cast<unsigned>(
          module.getDataLayout().getTypeAllocSize(reg->type) * 8u));
}

// Looks for calls to a function like `__remill_error`, and
// replace its state pointer with a null pointer so that the state
// pointer never escapes.
static void MuteStateEscape(llvm::Module &module, const char *func_name) {
  auto func = module.getFunction(func_name);
  if (!func) {
    return;
  }

  for (auto user : func->users()) {
    if (auto call_inst = llvm::dyn_cast<llvm::CallInst>(user)) {
      auto arg_op = call_inst->getArgOperand(remill::kStatePointerArgNum);
      call_inst->setArgOperand(remill::kStatePointerArgNum,
                               llvm::UndefValue::get(arg_op->getType()));
    }
  }
}

using reg_ptr_t = const remill::Register *;
std::vector<reg_ptr_t> EnclosedClosure(reg_ptr_t ptr) {
  std::vector<reg_ptr_t> out;
  std::vector<reg_ptr_t> todo{ ptr };
  // Note(lukas): I assume that registers are a tree like structure!
  while (!todo.empty()) {
    out.push_back(todo.back());
    todo.pop_back();
    for (auto x : out.back()->EnclosedRegisters()) {
      todo.push_back(x);
    }
  }
  // Just a sanity check
  CHECK(std::unordered_set<reg_ptr_t>(out.begin(), out.end()).size() == out.size());
  return out;
}

template<typename T = uint64_t>
T inst_fragments_count(llvm::CallInst *inst) {
  auto maybe_count = GetMetadata(inst, CircuitBuilder::bytes_fragments_count_kind);
  CHECK(maybe_count) << "Byte fragments size not set for"
                     << remill::LLVMThingToString(inst);
  return static_cast<T>(*maybe_count);
}

}  // namespace

void State::store(
  llvm::IRBuilder<> &ir, const reg_ptr_t reg, llvm::Value *val)
{
  auto bb = ir.GetInsertBlock();
  const auto &dl = bb->getModule()->getDataLayout();
  auto gep = reg->AddressOf(state, bb);
  ir.SetInsertPoint(bb);

  // How much space does register occupy in form iN. There is an
  // optimization for flag registers.
  auto reg_type = IntegralRegisterType(*bb->getModule(), reg);
  auto store_type = ir.getIntNTy(static_cast<unsigned>(dl.getTypeAllocSize(reg_type) * 8u));
  auto coerced_type = ir.CreateBitCast(gep, llvm::PointerType::getUnqual(store_type));

  if (reg_type != store_type) {
    val = ir.CreateZExt(val, store_type);
  }
  ir.CreateStore(val, coerced_type);

}

llvm::Value *State::load(llvm::IRBuilder<> &ir, const reg_ptr_t reg) {
  auto bb = ir.GetInsertBlock();
  const auto &dl = bb->getModule()->getDataLayout();
  auto gep = reg->AddressOf(state, bb);
  ir.SetInsertPoint(bb);

  // How much space does register occupy in form iN. There is an
  // optimization for flag registers.
  auto reg_type = IntegralRegisterType(*bb->getModule(), reg);
  auto store_type = ir.getIntNTy(static_cast<unsigned>(dl.getTypeAllocSize(reg_type) * 8u));
  auto coerced_type = ir.CreateBitCast(gep, llvm::PointerType::getUnqual(store_type));

  auto loaded = ir.CreateLoad(coerced_type);
  if (reg_type != store_type) {
    return ir.CreateTrunc(loaded, reg_type);
  }
  return loaded;
}

llvm::Function *CircuitBuilder::Build(llvm::StringRef buff) {
  if (auto used = ctx.module()->getGlobalVariable("llvm.used"); used) {
    used->eraseFromParent();
  }

  // TODO(pag): Create an immediate operand analysis that can identify bits
  //            associated with immediates (e.g. by bit flipping, re-decoding,
  //            then structurally comparing the decoded instructions for
  //            near-equivalence). Feed results of this analysis into a class
  //            deriving from the instruction lifter, and then use it to
  //            lift all immediates as something like:
  //
  //                __circuitous_immediate(i64 val, iN mask)
  //
  //            Where the `iN mask` is a bitmask on the instruction encoding
  //            representing the bits that produced `val`.

  // TODO(pag): Simplify certain patterns into intrinsic calls, e.g. getting
  //            the sign bit, getting the low N bits of an instruction (i.e.
  //            convert the AND into a function call.

  auto isels = BaseLifter<InstructionLifter>(ctx).Run(buff);
  EraseFns(ctx.module(), { "__remill_intrinsics", "__remill_mark_as_used" });

  // These improve optimizability.
  MuteStateEscape(*ctx.module(), "__remill_function_return");
  MuteStateEscape(*ctx.module(), "__remill_error");
  MuteStateEscape(*ctx.module(), "__remill_missing_block");

  return BuildCircuit1(BuildCircuit0(std::move(isels)));
}

// Build the first level circuit. We will analyze this function later to
// get an accurate picture of instruction dependencies.
auto CircuitBuilder::BuildCircuit0(std::vector<InstructionSelection> isels) -> Circuit0 {

  auto circuit0 = Circuit0(ctx);
  auto circuit0_func = circuit0.Create();
  circuit0.InjectISELs(std::move(isels));
  ctx.clean_module({circuit0_func});

  circuit0.constraint_unused();

  intrinsics::disable_opts<intrinsics::VerifyInst, intrinsics::Select>(ctx.module());

  OptimizeSilently(ctx.arch(), ctx.module(), {circuit0_func});

  SelectFolder folder{ std::move(circuit0.used_selects), circuit0_func };
  folder.run();

  remill::VerifyModule(ctx.module());
  intrinsics::disable_opts<intrinsics::Select, intrinsics::Advice>(ctx.module());
  intrinsics::enable_opts<intrinsics::VerifyInst, intrinsics::AdviceConstraint>(ctx.module());

  OptimizeSilently(ctx.arch(), ctx.module(), {circuit0_func});
  remill::VerifyModule(ctx.module());
  return circuit0;
}

void CircuitBuilder::FoldOutputChecks() {
  std::vector<llvm::CallInst *> to_fold;

  for (auto matcher : intrinsics::OutputCheck::All(ctx.module())) {
    if (!matcher) {
      continue;
    }
    for (auto user : matcher->users()) {
      if (const auto call_inst = llvm::dyn_cast<llvm::CallInst>(user);
          call_inst &&
          call_inst->getArgOperand(0) == call_inst->getArgOperand(1)) {
        to_fold.push_back(call_inst);
      }
    }
  }

  for (auto call_inst : to_fold) {
    call_inst->replaceAllUsesWith(llvm::IRBuilder<>(call_inst).getTrue());
    call_inst->eraseFromParent();
  }
}

// Build the second level circuit. Here we analyze how the instruction checkers
// use registers and try to eliminate unneeded registers from the function's
// argument list.
llvm::Function *CircuitBuilder::BuildCircuit1(Circuit0 circuit0) {
  auto circuit0_func = circuit0.GetFn();
  RegisterDependencyCollector deps(ctx.arch());

  // Look at all calls to instruction verifiers. These function calls take as
  // input the integer index (into `insts`) of the instruction being verified,
  // followed by a variable (really: `regs.size()`) number of `i1` values that
  // should be the results of ICmp instructions, each testing whether or not
  // the current value of a register matches the next expected value of the
  // register.
  intrinsics::VerifyInst::ForAllIn(circuit0_func, [&](llvm::CallInst *verify_call_inst) {
    for (auto &arg_use : verify_call_inst->arg_operands()) {
      auto arg = arg_use.get();

      if (!llvm::isa<llvm::CallInst>(arg)) {
        continue;
      }
      auto call = llvm::cast<llvm::CallInst>(arg);
      auto callee = call->getCalledFunction();

      CHECK(intrinsics::is_any(callee));

      if (intrinsics::one_of< intrinsics::ReadConstraint,
                              intrinsics::WriteConstraint >(callee))
      {
        for (auto i = 0u; i < call->getNumArgOperands(); ++i) {
          deps.Visit(call->getArgOperandUse(i));
        }
      }

      // We need to do something special only for `OutputCheck`.
      if (!intrinsics::OutputCheck::IsIntrinsic(callee)) {
        continue;
      }

      auto lhs = call->getArgOperand(0);
      auto rhs = call->getArgOperand(1);
      if (!llvm::isa<llvm::Argument>(rhs) || !Names::is_out_reg(rhs)) {
        continue;
      }

      // We need to check that `rhs` is an output register.
      CHECK(llvm::isa<llvm::Argument>(rhs) && Names::is_out_reg(rhs))
          << "Expected second argument to " << callee->getName().str()
          << " to match the output register"
          << " instead got: " << remill::LLVMThingToString(call);
      // We just pass the input into the output -> nothing changes for this reg
      if (Names::are_tied(lhs, rhs)) {
        continue;
      }
      if (llvm::isa<llvm::Argument>(lhs) ||
          llvm::isa<llvm::Instruction>(lhs) ||
          llvm::isa<llvm::ConstantInt>(lhs)) {
        deps.read_registers.insert(Names::dual_reg(circuit0_func, rhs));
        deps.written_registers.insert(llvm::cast<llvm::Argument>(rhs));
        auto &new_reg_val_use = call->getArgOperandUse(0);
        deps.Visit(new_reg_val_use);

      // Some unrecognized pattern.
      } else {
        LOG(FATAL) << "Neither side of integer comparison associated with "
                   << Names::name(rhs).str()
                   << " was itself an argument of circuit_0 in "
                   << remill::LLVMThingToString(call);
      }
    }
  });
  LOG(INFO) << "Gathering parameters of circuit_1";

  // Create the type for circuit1. Here, we'll have all output registers come
  // after all input registers.
  std::vector<llvm::Type *> circuit1_arg_types;
  std::vector<const remill::Register *> new_regs;

  auto alien_size = circuit0.surface.copy_aliens(circuit1_arg_types);

  for (auto in_reg : deps.read_registers) {
    new_regs.push_back(ctx.arch()->RegisterByName(in_reg->getName().str()));
    circuit1_arg_types.push_back(in_reg->getType());
  }

  for (auto out_reg : deps.written_registers) {
    circuit1_arg_types.push_back(out_reg->getType());
  }

  // The read set should be a subset of the written set.
  CHECK_LE(deps.read_registers.size(), deps.written_registers.size());

  llvm::IRBuilder<> ir(*ctx.llvm_ctx());
  auto circuit1_func = llvm::Function::Create(
      llvm::FunctionType::get(ir.getInt1Ty(), circuit1_arg_types, false),
      llvm::GlobalValue::ExternalLinkage, "circuit1_func", ctx.module());
  circuit1_func->addFnAttr(llvm::Attribute::ReadNone);
  LOG(INFO) << "Synthetized circuit_1";

  std::size_t i = 0;
  for (; i < alien_size; ++i) {
    CopyArgName(circuit1_func, circuit0_func, i);
  }

  // Rename the parameters to correspond with our input/output registers.
  // i = num_instruction_parts;
  for (auto in_reg : deps.read_registers) {
    remill::NthArgument(circuit1_func, i++)->setName(in_reg->getName());
  }

  for (auto out_reg : deps.written_registers) {
    remill::NthArgument(circuit1_func, i++)->setName(out_reg->getName());
  }

  // Mark circuit0 for inlining and elimination.
  circuit0_func->setLinkage(llvm::GlobalValue::PrivateLinkage);
  circuit0_func->addFnAttr(llvm::Attribute::InlineHint);
  circuit0_func->addFnAttr(llvm::Attribute::AlwaysInline);

  std::vector<llvm::Value *> args;

  for (std::size_t j = 0; j < alien_size; ++j ) {
    args.push_back(remill::NthArgument(circuit1_func, j));
  }

  // Build up an argument list to call circuit0_func from circuit1_func. We
  // pass through the arguments associated with registers that the above
  // analysis determined to be used, and pass in null values (zeroes) for the
  // rest. We'll be able to observe calls like the following:
  //
  //    %34 = tail call i1 (i8, i8, ...) @intrinsics::OutputCheck(i8 0, i8 0)
  //
  // Removing uses of these redundant comparisons will let us shrink down the
  // verification call arg lists to only verify used registers.
  for (auto reg : ctx.regs()) {
    auto orig_reg = remill::FindVarInFunction(circuit0_func, Names::as_in_reg(reg->name));
    if (auto in_reg =
        remill::FindVarInFunction(circuit1_func, Names::as_in_reg(reg->name), true)) {
      args.push_back(in_reg);
    } else {
      args.push_back(llvm::Constant::getNullValue(orig_reg->getType()));
    }

    if (auto out_reg =
        remill::FindVarInFunction(circuit1_func, Names::as_out_reg(reg->name), true)) {
      args.push_back(out_reg);
    } else {
      args.push_back(llvm::Constant::getNullValue(orig_reg->getType()));
    }
  }

  auto entry = llvm::BasicBlock::Create(*ctx.llvm_ctx(), "", circuit1_func);
  auto res = llvm::CallInst::Create(circuit0_func, args, "", entry);
  llvm::ReturnInst::Create(*ctx.llvm_ctx(), res, entry);

  // Optimizing the module again will inline circuit0_func into circuit1_func
  // and propagate the null (i.e. zero) values for all unused registers down
  // through the inlined body of circuit0_func.
  OptimizeSilently(ctx.arch(), ctx.module(), {circuit1_func});
  FoldOutputChecks();

  std::vector<std::pair<llvm::CallInst *, llvm::CallInst *>> to_replace;

  // Now go through an change the arguments to `__circuitous.verify_inst` to
  // to reflect the new register transfer comparisons.
  intrinsics::VerifyInst::ForAllIn(circuit1_func, [&](llvm::CallInst *call_inst) {
    std::vector<llvm::Value *> args;

    for (auto i = 0u; i < call_inst->getNumArgOperands(); ++i) {
      using CS = remill::compat::llvm::CallSite;
      auto op = call_inst->getArgOperand(i);
      auto cs  = CS(llvm::dyn_cast_or_null<llvm::Instruction>(op));
      if (op == ir.getTrue()) {
        continue;
      }
      if (!cs) {
        args.push_back(op);
        continue;
      }
      CHECK(cs && intrinsics::is_any(cs.getCalledFunction()))
          << "We do not expect verify inst to have non-intrinsic operand "
          << "[ " << i << " ]: "
          << remill::LLVMThingToString(op);
      auto callee = cs.getCalledFunction();
      if (!intrinsics::OutputCheck::IsIntrinsic(callee)) {
        args.push_back(op);
      } else {
        auto op_as_call = llvm::cast<llvm::CallInst>(op);
        auto lhs = op_as_call->getArgOperand(0);
        auto rhs = op_as_call->getArgOperand(1);
        if (llvm::dyn_cast<llvm::Constant>(lhs) &&
            llvm::dyn_cast<llvm::Constant>(rhs) &&
            lhs == rhs) {
          continue;
        }
        args.push_back(op);
      }

    }

    // TODO(lukas): Make sure we can use `ir` here.
    llvm::IRBuilder<> tir(call_inst);
    auto new_call = intrinsics::make_verify(tir, args);
    to_replace.emplace_back(call_inst, new_call);
  });

  for (auto [old_call, new_call] : to_replace) {
    old_call->replaceAllUsesWith(new_call);
    old_call->eraseFromParent();
  }

  return circuit1_func;
}

llvm::Function *Circuit0::GetFn() {
  return ctx.module()->getFunction(name);
}

llvm::Function *Circuit0::Create() {
  circuit_fn = surface.Create(name);

  // Sanity check, we are going to need pc later on.
  CHECK(surface.pc != nullptr)
      << "Couldn't find program counter register "
      << ctx.arch()->ProgramCounterRegisterName();
  return circuit_fn;
}

void Circuit0::constraint_unused_advices(verif_diff_t &out) {

  std::unordered_set< llvm::Value * > all_advices;
  auto gather = [&](auto x) { all_advices.insert(x); };
  intrinsics::Advice::ForAllIn(circuit_fn, gather);

  auto handle_ctx = [](auto ctx_i, auto advices) {
    auto ctx = llvm::dyn_cast<llvm::CallInst>(ctx_i);
    CHECK(ctx);
    for (uint32_t i = 0; i < ctx->getNumArgOperands(); ++i) {
      auto intrinsic_call = llvm::dyn_cast< llvm::CallInst >(ctx->getArgOperand(i));
      if (!intrinsic_call ||
          !intrinsics::AdviceConstraint::IsIntrinsic(intrinsic_call->getCalledFunction()))
      {
        continue;
      }

      auto advice = intrinsic_call->getArgOperand(1u);
      advices.erase(advice);
    }
    return advices;
  };


  for (auto verif : verified_insts) {
    llvm::IRBuilder<> ir(llvm::dyn_cast<llvm::Instruction>(verif));
    for (auto unconstrained : handle_ctx(verif, all_advices)) {
      out[verif].push_back(intrinsics::make_advice_constraint(ir, unconstrained, 0ull));
    }
  }
}

void Circuit0::constraint_unused_hints(verif_diff_t &out) {
  auto all_mem = mem::get_all(ctx.module());

  for (std::size_t i = 0; i < verified_insts.size(); ++i) {
    auto ctx = llvm::dyn_cast< llvm::CallInst >(verified_insts[i]);
    CHECK(ctx) << remill::LLVMThingToString(verified_insts[i]);

    auto constrained = mem::constrained_by(ctx);

    llvm::IRBuilder<> ir(ctx);
    for (auto [idx, fn] : all_mem) {
      // Already constrained by this context, no need to do more
      if (constrained.count(idx)) {
        continue;
      }

      auto mem_hint = intrinsics::make_memory(ir, intrinsics::Memory::id(fn));
      out[verified_insts[i]].push_back(intrinsics::make_unused_constraint(ir, {mem_hint}));
    }
  }
}

void Circuit0::constraint_unused() {
  verif_diff_t diff;
  constraint_unused_hints(diff);
  constraint_unused_advices(diff);

  for (std::size_t i = 0; i < verified_insts.size(); ++i) {
    if (!diff.count(verified_insts[i])) {
      continue;
    }
    auto additional_args = diff[verified_insts[i]];

    auto selects = used_selects[verified_insts[i]];
    used_selects.erase(verified_insts[i]);

    auto ctx = llvm::dyn_cast< llvm::CallInst >(verified_insts[i]);
    verified_insts[i] = extend(ctx, std::move(additional_args));
    used_selects[verified_insts[i]] = std::move(selects);
  }
}

void Circuit0::InjectISELs(std::vector<InstructionSelection> isels) {
  CHECK(circuit_fn);

  auto circuit0_func = circuit_fn;

  auto entry_block = llvm::BasicBlock::Create(*ctx.llvm_ctx(), "", circuit0_func);
  auto exit_block = llvm::BasicBlock::Create(*ctx.llvm_ctx(), "", circuit0_func);
  auto prev_block = entry_block;

  for (const auto &isel : isels) {
    prev_block = InjectISEL(isel,  prev_block, exit_block);
  }

  llvm::BranchInst::Create(exit_block, prev_block);

  llvm::IRBuilder<> ir(exit_block);
  ir.CreateRet(intrinsics::make_xor(ir, verified_insts));
}

llvm::BasicBlock *Circuit0::InjectISEL(
    const InstructionSelection &isel,
    llvm::BasicBlock *prev_block,
    llvm::BasicBlock *exit_block) {
  // Add one basic block per lifted instruction. Each block allocates a
  // separate state structure.
  for (auto i = 0u; i < isel.instructions.size(); ++i) {
    auto inst_block = llvm::BasicBlock::Create(*ctx.llvm_ctx(), "", circuit_fn);
    llvm::BranchInst::Create(inst_block, prev_block);
    prev_block = inst_block;

    InjectSemantic(inst_block, exit_block, ISEL_view(isel, i));
  }
  return prev_block;
}

std::vector<llvm::Instruction *> Circuit0::LowerDstRegs(std::vector<llvm::Value *> &dsts) {
  std::vector<llvm::Instruction *> out;

  for (auto dst : dsts) {
    CHECK(llvm::isa<llvm::PointerType>(dst->getType()))
        << "Dst reg type before lowering is not pointer";

    auto p_type = llvm::cast<llvm::PointerType>(dst->getType());
    llvm::IRBuilder<> ir(llvm::cast<llvm::Instruction>(dst));
    out.push_back(ir.CreateAlloca(p_type->getPointerElementType(), nullptr, "DSTA_"));
    dst->replaceAllUsesWith(out.back());
    llvm::dyn_cast<llvm::Instruction>(dst)->eraseFromParent();
  }
  return out;
}

std::vector<llvm::Value *> Circuit0::HandleDstRegs(
    llvm::IRBuilder<> &ir,
    llvm::Value *current_ebit,
    std::vector<llvm::Instruction *> &dst_regs, ISEL_view isel, State &state)
{
  // Comparisons on whether or not the resulting
  // register after the semantic has executed matches the next state of that
  // register.
  std::vector<llvm::Value *> params;
  for (auto [reg, input_reg, expected_reg_val] : surface.reg_to_args) {
    llvm::Value *original_val = state.load(ir, reg);
    llvm::Value *reg_val = original_val;
    // We need to keep track which operand we are about to handle so we can index into
    // `dst_regs`.
    uint64_t proccessed = 0;

    for (std::size_t i = 0; i < isel.instruction.operands.size(); ++i) {
      // We care only for write operands
      if (isel.instruction.operands[i].action != remill::Operand::Action::kActionWrite) {
        continue;
      }
      // Everything destination is "hardcoded", we do not need to take care
      // of anything.
      if (dst_regs.size() == 0) {
        continue;
      }

      auto &s_op = isel.shadow.operands[i];
      if (!s_op.reg) {
        continue;
      }

      ++proccessed;
      auto &table = s_op.reg->translation_map;

      if (s_op.reg->is_dirty(reg->name)) {
        continue;
      }

      for (auto reg_part : EnclosedClosure(reg)) {
        if (!table.count(reg_part->name)) {
          continue;
        }

        // The basic idea here (we need to handle partial registers)
        // is that we first "refresh" the top-level with originally loaded value.
        // That is needed because in previous iteration something else may have been
        // store there.
        // Then we write the value into the partial register and we again load from
        // the top-level to retrieve the value (with correctly stored value).
        // This is a lot of memory operations and we rely heavily on llvm
        // `mm2reg` pass to help us out.

        // Someone before us may have written something - we need to reset the value.
        state.store(ir, reg, original_val);
        auto reg_checks = shadowinst::decoder_conditions(*s_op.reg, reg_part->name, ir);

        // Check if everything is still valid.
        CHECK(proccessed - 1 < dst_regs.size()) << proccessed - 1 << " >= " << dst_regs.size();
        auto eq = intrinsics::make_xor<true>(ir, reg_checks);
        auto dst_load = ir.CreateLoad(dst_regs[proccessed - 1]);
        auto reg_addr = reg_part->AddressOf(state.raw(), ir);

        auto store_ty = llvm::cast<llvm::PointerType>(reg_addr->getType())->getElementType();

        ir.CreateStore(ir.CreateSExtOrTrunc(dst_load, store_ty), reg_addr);
        auto full_val = state.load(ir, reg);
        reg_val = ir.CreateSelect(eq, full_val, reg_val);
      }
    }
    CHECK_NOTNULL(current_ebit);
    CHECK_NOTNULL(input_reg);
    CHECK_NOTNULL(reg_val);
    // If error bit is raised we are not moving anywhere
    auto guard = ir.CreateSelect(current_ebit, input_reg, reg_val);
    params.push_back(intrinsics::make_outcheck(ir, {guard, expected_reg_val}));
  }
  return params;
}


void Circuit0::InjectSemantic(
    llvm::BasicBlock *inst_block, llvm::BasicBlock *exit_block, ISEL_view isel) {

  CHECK_NOTNULL(isel.lifted);

  // Initialize the state.
  auto state = State{inst_block, ctx.state_ptr_type()->getElementType()};
  auto state_ptr = state.raw();
  llvm::IRBuilder<> ir(inst_block);

  for (const auto &[reg, arg, _] : surface.reg_to_args) {
    state.store(ir, reg, arg);
  }

  auto sem_call = CallSemantic(
    ir, isel.lifted, state_ptr, surface.pc, llvm::UndefValue::get(ctx.memory_ptr_type()));

  auto make_breakpoint = [](auto ir) {
    return intrinsics::make_breakpoint(ir);
  };
  auto [begin, end] = inline_flattened(sem_call, make_breakpoint);
  ir.SetInsertPoint(inst_block);


  // Create encoding comparisons
  auto params = ByteFragments(ir, isel);
  // Collect selects, they will be needed later for folding
  auto selects = intrinsics::collect<intrinsics::Select>(begin, end);

  auto fragments_size = params.size();
  params.push_back(surface.timestamp_property(ir));
  params.push_back(surface.saturation_property(ir));

  auto [i_ts, _] = surface.fetch_timestamps();
  auto mem_checks = mem::synthetize_memory(i_ts, begin, end);

  params.insert(params.end(), mem_checks.begin(), mem_checks.end());

  auto [ebit_in, ebit_out] = surface.fetch_ebits();
  auto current_err = err::synthesise_current(ir, begin, end);
  if (!current_err) {
    // This instruction cannot raise error bit -> input error bit
    // cannot be set.
    params.push_back(ir.CreateICmpEQ(ebit_in, ir.getFalse()));
    current_err = ir.getFalse();
  } else {
    // Error bit is saturated, so we need to `or` input and current.
    current_err = ir.CreateOr(ebit_in, current_err);
  }
  params.push_back(intrinsics::make_outcheck(ir, {current_err, ebit_out}));

  // Collect annotated instructions - this is the way separate components
  // of the lfiting pipleline communicate
  auto collected = shadowinst::collect_annotated(begin, end);
  auto dst_intrinsics = std::move(collected[Names::meta::dst_reg]);

  auto &extra_params = collected[Names::meta::verify_args];
  for (std::size_t i = 0; i < extra_params.size(); ++i) {
    extra_params[i] = intrinsics::Transport::unwrap(extra_params[i]);
  }
  params.insert(params.end(), extra_params.begin(), extra_params.end());

  begin->eraseFromParent();
  end->eraseFromParent();

  auto dst_regs = LowerDstRegs(dst_intrinsics);
  // LowerDstRegs may have added some instructions.
  ir.SetInsertPoint(inst_block);
  auto additional_checks = HandleDstRegs(ir, current_err, dst_regs, isel, state);
  params.insert(params.end(), additional_checks.begin(), additional_checks.end());

  // Call the instruction verification function. This returns `1` iff we
  // verified the isel decoding (first argument) and if all other arguments
  // (register comparisons) are true.
  ir.SetInsertPoint(exit_block);
  auto call = intrinsics::make_verify(ir, params);
  AddMetadata(call, CircuitBuilder::bytes_fragments_count_kind, fragments_size);
  verified_insts.push_back(call);
  used_selects.emplace(call, std::move(selects));
}

std::vector<llvm::Value *> Circuit0::ByteFragments(
    llvm::IRBuilder<> &ir, ISEL_view isel) {

  // Reorders bytes so that they can be matched to extract from instruction
  // bytes without extra concats.
  auto generate_raw_bytes = [](auto full, uint64_t from, uint64_t to) {
    std::string out;
    while(true) {
      // NOTE(lukas): To handle un-aligned values.
      uint64_t y = std::min(from + (8 - from % 8), to);
      std::string partial = full.substr(from, y - from);
      std::reverse(partial.begin(), partial.end());
      out = partial + out;
      if (y == to) {
        return out;
      }
      from = y;
    }
  };

  std::vector<llvm::Value *> out;
  auto rinst_size = isel.instruction.bytes.size() * 8;

  auto create_bit_check = [&](auto from, auto to) {
    auto fn = intrinsics::ExtractRaw::CreateFn(ctx.module(), from, to - from );

    std::string full_inst;
    // Encoding check needed since `x` is unsigned.
    for (auto x = rinst_size - 1; x >= 0 && x < isel.encoding.size(); --x) {
      full_inst += (isel.encoding[x]) ? '1' : '0';
    }
    // TODO(lukas): Sorry, most likely there is a more sane way to do this
    //              but since it may change in rather near future I am keeping
    //              it this way.
    std::reverse(full_inst.begin(), full_inst.end());
    std::string expected = generate_raw_bytes(full_inst, from, to);

    auto size = static_cast<uint32_t>(expected.size());
    CHECK(size == to - from) << size << " != " << to - from;

    auto expected_v = ir.getInt(llvm::APInt(size, expected, 2));
    const auto rhs = remill::NthArgument(circuit_fn, 0);
    auto x = ir.CreateCall(fn, {rhs});
    return intrinsics::make_bitcompare(ir, {expected_v, x}, size);
  };

  auto unknown_regions = isel.shadow.UnknownRegions(rinst_size);
  // `unknown_regions` are in `[ from, size ]` format.
  for (auto [from, to] : shadowinst::FromToFormat(unknown_regions)) {
    out.push_back(create_bit_check(from, to));
  }

  // TODO(lukas): Now we need to check the tail.
  //              Try to lift `6689d8` and `89d8` to demonstrate the issue.
  // TODO(lukas): For now we assume it is padded with 0s.
  auto tail_size = static_cast<uint32_t>(kMaxNumInstBits - rinst_size);
  auto tail = ir.getInt(llvm::APInt(tail_size, 0, false));

  auto extracted = intrinsics::make_raw_ib_extract(ir, rinst_size, tail_size);
  auto compare = intrinsics::make_bitcompare(ir, {tail, extracted}, tail_size);
  out.push_back(compare);
  return out;
}

auto Surface::Regs() const -> types_t {
  types_t params_types;
  for (auto reg : ctx.regs()) {
    const auto reg_type = IntegralRegisterType(*ctx.module(), reg);
    params_types.push_back(reg_type);
    params_types.push_back(reg_type);
  }
  return params_types;
}

}  // namespace circ
