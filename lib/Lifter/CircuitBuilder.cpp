/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include "CircuitBuilder.h"
#include "DependencyVisitor.hpp"
#include "InstructionFuzzer.hpp"

#include "Flatten.hpp"

#include <circuitous/Lifter/BaseLifter.hpp>
#include <circuitous/IR/Lifter.hpp>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <gflags/gflags.h>
#include <glog/logging.h>
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/IR/CallSite.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Codegen/IntrinsicLowering.h>
#include <llvm/Transforms/Utils/Cloning.h>
#pragma clang diagnostic pop

#include <sstream>

namespace circuitous {
namespace {

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

  remill::VerifyModule(ctx.module());
  OptimizeSilently(ctx.arch(), ctx.module(), {circuit0_func});
  remill::VerifyModule(ctx.module());
  return circuit0;
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
    auto arg_num = 0u;
    auto reg_idx = 0u;
    auto inst_fragments_prefix = inst_fragments_count(verify_call_inst);
    // TODO(lukas): Refactor, this is clunky at best and very error prone.
    for (auto &arg_use : verify_call_inst->arg_operands()) {
      llvm::Value *arg = arg_use.get();
      CHECK(inst_fragments_prefix > 0);
      if (arg_num < inst_fragments_prefix) {
        ++arg_num;
        continue;
      }

      const auto call_inst = llvm::dyn_cast<llvm::CallInst>(arg);
      CHECK(call_inst) << "Expected argument of verify_inst to be calls";

      const auto icmp_eq = call_inst->getCalledFunction();
      CHECK_NOTNULL(icmp_eq);
      if (intrinsics::OneOf::IsIntrinsic(icmp_eq)) {
        ++arg_num;
        continue;
      }
      CHECK(intrinsics::Eq::IsIntrinsic(icmp_eq)) << LLVMName(icmp_eq);

      // Figure out the input and output registers to the circuit function.
      // const auto reg_id = arg_num - num_instruction_parts;
      // const auto in_reg_arg_index = num_instruction_parts + (2u * reg_id);
      const auto in_reg_arg_index = circuit0.inst_bytes.size() + (2u * reg_idx);
      const auto in_reg_arg =
          remill::NthArgument(circuit0_func, in_reg_arg_index);
      const auto out_reg_arg =
          remill::NthArgument(circuit0_func, in_reg_arg_index + 1u);

      CHECK(out_reg_arg->getName().endswith("_next"))
          << out_reg_arg->getName().str();
      CHECK(out_reg_arg->getName().startswith(in_reg_arg->getName()));

      const auto in_reg_name = in_reg_arg->getName().str();
      const auto out_reg_name = out_reg_arg->getName().str();
      ++arg_num; ++reg_idx;

      const auto lhs = call_inst->getArgOperand(0);
      const auto rhs = call_inst->getArgOperand(1);

      const auto rhs_arg = llvm::dyn_cast<llvm::Argument>(rhs);
      CHECK_EQ(rhs_arg, out_reg_arg)
          << "Expected second argument to " << icmp_eq->getName().str()
          << " to match the output register " << out_reg_name << " but got "
          << remill::LLVMThingToString(rhs)
          << " instead: " << remill::LLVMThingToString(call_inst);

      // Input is compared to output, i.e. the instruction doesn't write
      // to this register.
      if ((llvm::isa<llvm::Argument>(lhs) && lhs == in_reg_arg)) {
        continue;

      // Something that wasn't this register's input (might be a different
      // register's input, or a constant, or a dynamic comparison) is
      // compared with the output reg, i.e. this instuction writes to the
      // register.
      } else if (llvm::isa<llvm::Argument>(lhs) ||
                 llvm::isa<llvm::Instruction>(lhs) ||
                 llvm::isa<llvm::ConstantInt>(lhs)) {
        deps.read_registers.insert(in_reg_arg);
        deps.written_registers.insert(out_reg_arg);
        auto &new_reg_val_use = call_inst->getArgOperandUse(0);
        deps.Visit(new_reg_val_use);

      // Some unrecognized pattern.
      } else {
        LOG(FATAL) << "Neither side of integer comparison associated with "
                   << out_reg_name << " was itself an argument of circuit_0 in "
                   << remill::LLVMThingToString(call_inst);
      }
    }
  });

  // Create the type for circuit1. Here, we'll have all output registers come
  // after all input registers.
  std::vector<llvm::Type *> circuit1_arg_types;
  std::vector<const remill::Register *> new_regs;
  auto i = 0u;

  circuit1_arg_types.push_back(
      remill::NthArgument(circuit0_func, 0)->getType());

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

  // Rename the parameters to correspond with our input/output registers.
  // i = num_instruction_parts;
  i = 1u;
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

  args.push_back(remill::NthArgument(circuit1_func, 0));

  // Build up an argument list to call circuit0_func from circuit1_func. We
  // pass through the arguments associated with registers that the above
  // analysis determined to be used, and pass in null values (zeroes) for the
  // rest. We'll be able to observe calls like the following:
  //
  //    %34 = tail call i1 (i8, i8, ...) @intrinsics::Eq(i8 0, i8 0)
  //
  // Removing uses of these redundant comparisons will let us shrink down the
  // verification call arg lists to only verify used registers.
  for (auto reg : ctx.regs()) {
    const auto orig_reg = remill::FindVarInFunction(circuit0_func, reg->name);
    if (auto in_reg = remill::FindVarInFunction(circuit1_func, reg->name, true);
        in_reg) {
      args.push_back(in_reg);
    } else {
      args.push_back(llvm::Constant::getNullValue(orig_reg->getType()));
    }

    if (auto out_reg =
            remill::FindVarInFunction(circuit1_func, reg->name + "_next", true);
        out_reg) {
      args.push_back(out_reg);
    } else {
      args.push_back(llvm::Constant::getNullValue(orig_reg->getType()));
    }
  }

  auto entry = llvm::BasicBlock::Create(*ctx.llvm_ctx(), "", circuit1_func);
  auto res = llvm::CallInst::Create(circuit0_func, args, "", entry);
  (void) llvm::ReturnInst::Create(*ctx.llvm_ctx(), res, entry);

  // Optimizing the module again will inline circuit0_func into circuit1_func
  // and propagate the null (i.e. zero) values for all unused registers down
  // through the inlined body of circuit0_func.
  OptimizeSilently(ctx.arch(), ctx.module(), {circuit1_func});

  // "Constant fold" the uses of `__circuitous.icmp_eq.8`.
  std::vector<llvm::CallInst *> to_fold;

  for (auto matcher : intrinsics::Eq::All(ctx.module())) {
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

  std::vector<std::pair<llvm::CallInst *, llvm::CallInst *>> to_replace;

  // Now go through an change the arguments to `__circuitous.verify_inst` to
  // to reflect the new register transfer comparisons.
  intrinsics::VerifyInst::ForAllIn(circuit1_func, [&](llvm::CallInst *call_inst) {
    args.clear();
    auto inst_fragments_prefix = inst_fragments_count<uint32_t>(call_inst);
    for (auto j = 0u; j < inst_fragments_prefix; ++j) {
      args.push_back(call_inst->getArgOperand(j));
    }
    // TODO(lukas): This is hack, rework.
    while (true) {
      auto cs = llvm::CallSite(call_inst->getArgOperand(inst_fragments_prefix));
      if (!cs || intrinsics::Eq::IsIntrinsic(cs.getCalledFunction())) {
        break;
      }
      args.push_back(call_inst->getArgOperand(inst_fragments_prefix));
      ++inst_fragments_prefix;
    }
    for (i = 0u; i < ctx.regs().size(); ++i) {
      const auto reg = ctx.regs()[i];

      // const auto arg = call_inst->getArgOperand(i + num_instruction_parts);
      const auto arg = call_inst->getArgOperand(i + inst_fragments_prefix);
      if (std::find(new_regs.begin(), new_regs.end(), reg) != new_regs.end()) {
        CHECK(!llvm::isa<llvm::Constant>(arg)) << remill::LLVMThingToString(arg);
        args.push_back(arg);
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

llvm::FunctionType *Circuit0::FnT() {
  llvm::IRBuilder<> ir(*ctx.llvm_ctx());
  std::vector<llvm::Type *> param_types;

  // First parameter is the bit enconding of the instruction being verified
  param_types.push_back(ir.getIntNTy(kMaxNumInstBits));

    // The remaining parameters will be input/output registers for verification.
  for (auto reg : ctx.regs()) {
    const auto reg_type = IntegralRegisterType(*ctx.module(), reg);
    param_types.push_back(reg_type);
    param_types.push_back(reg_type);
  }

  return llvm::FunctionType::get(ir.getInt1Ty(), param_types, false);
}

llvm::Function *Circuit0::GetFn() {
  return ctx.module()->getFunction(name);
}

llvm::Function *Circuit0::Create() {
  llvm::IRBuilder<> ir(*ctx.llvm_ctx());
  if (circuit_fn) {
    LOG(FATAL) << "Circuit already has an associated llvm function";
    return circuit_fn;
  }

  if (auto fn = GetFn()) {
    LOG(FATAL) << "The module already has " << name << " function.";
    return fn;
  }

  circuit_fn =
    llvm::Function::Create(FnT(), llvm::GlobalValue::ExternalLinkage, name, ctx.module());
  circuit_fn->addFnAttr(llvm::Attribute::ReadNone);

  CHECK(circuit_fn->arg_size() > 0);
  inst_bytes.push_back(&*circuit_fn->arg_begin());
  // The rest of arguments should be the registers in/out pairs
  CHECK((circuit_fn->arg_size() - inst_bytes.size()) % 2 == 0);
  for (auto i = inst_bytes.size(); i < circuit_fn->arg_size(); i += 2) {
    // This "order" can be flexible, but better let it go in order
    // of regs.
    auto reg = ctx.regs()[(i - inst_bytes.size()) / 2];

    auto input_reg = remill::NthArgument(circuit_fn, i);
    auto output_reg = remill::NthArgument(circuit_fn, i + 1);

    input_reg->setName(reg->name);
    output_reg->setName(reg->name + "_next");

    reg_to_args.emplace_back(reg, input_reg, output_reg);

    if (reg->name == ctx.arch()->ProgramCounterRegisterName()) {
      pc = input_reg;
    }
  }

  // Sanity check, we are going to need pc later on.
  CHECK(pc != nullptr) << "Couldn't find program counter register "
                      << ctx.arch()->ProgramCounterRegisterName();
  return circuit_fn;
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
  }
  return out;
}

std::vector<llvm::Value *> Circuit0::HandleDstRegs(
    llvm::IRBuilder<> &ir,
    std::vector<llvm::Instruction *> &dst_regs, ISEL_view isel, State &state)
{
  // Comparisons on whether or not the resulting
  // register after the semantic has executed matches the next state of that
  // register.
  std::vector<llvm::Value *> params;
  for (auto [reg, _, expected_reg_val] : reg_to_args) {
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
        auto eq = intrinsics::make_xor(ir, reg_checks);
        auto dst_load = ir.CreateLoad(dst_regs[proccessed - 1]);
        auto reg_addr = reg_part->AddressOf(state.raw(), ir);

        auto store_ty = llvm::cast<llvm::PointerType>(reg_addr->getType())->getElementType();

        ir.CreateStore(ir.CreateSExtOrTrunc(dst_load, store_ty), reg_addr);
        auto full_val = state.load(ir, reg);
        reg_val = ir.CreateSelect(eq, full_val, reg_val);
      }
    }

    auto eq_func = intrinsics::Eq::CreateFn(ctx.module(), reg_val->getType());
    params.push_back(ir.CreateCall(eq_func, {reg_val, expected_reg_val}));
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

  for (auto [reg, arg, _] : reg_to_args) {
    state.store(ir, reg, arg);
  }

  auto begin = intrinsics::make_breakpoint(ir);
  auto sem_call = CallSemantic(
    ir, isel.lifted, state_ptr, pc, llvm::UndefValue::get(ctx.memory_ptr_type()));
  auto end = intrinsics::make_breakpoint(ir);

  llvm::InlineFunctionInfo info;
  llvm::InlineFunction(sem_call, info);

  // Create encoding comparisons
  auto params = ByteFragments(ir, isel);
  auto fragments_size = params.size();

  // Collect annotated instructions - this is the way separate components
  // of the lfiting pipleline communicate
  auto collected = shadowinst::collect_annotated(begin, end);
  auto dst_intrinsics = std::move(collected["circuitous.dst.reg"]);
  auto &extra_params = collected["circuitous.verify_fn_args"];
  params.insert(params.end(), extra_params.begin(), extra_params.end());

  begin->eraseFromParent();
  end->eraseFromParent();

  auto dst_regs = LowerDstRegs(dst_intrinsics);
  // LowerDstRegs may have added some instructions.
  ir.SetInsertPoint(inst_block);
  auto additional_checks = HandleDstRegs(ir, dst_regs, isel, state);
  params.insert(params.end(), additional_checks.begin(), additional_checks.end());

  // Call the instruction verification function. This returns `1` iff we
  // verified the isel decoding (first argument) and if all other arguments
  // (register comparisons) are true.
  ir.SetInsertPoint(exit_block);
  auto call = intrinsics::make_verify(ir, params);
  AddMetadata(call, CircuitBuilder::bytes_fragments_count_kind, fragments_size);
  verified_insts.push_back(call);
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

  auto extracted = intrinsics::make_raw_extract(ir, rinst_size, tail_size);
  auto compare = intrinsics::make_bitcompare(ir, {tail, extracted}, tail_size);
  out.push_back(compare);
  return out;
}

}  // namespace circuitous
