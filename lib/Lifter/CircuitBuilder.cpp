/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include "CircuitBuilder.h"
#include "DependencyVisitor.hpp"
#include "circuitous/IR/Lifter.hpp"
#include "InstructionFuzzer.hpp"

#include "Flatten.hpp"

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

// Get the semantic name for an instruction encoding. We attach on the size of
// the instruction in bytes as on x86, the iforms from XED don't guarantee us
// the same size of bits.
static std::string IselName(const remill::Instruction &inst) {
  CHECK_GE(15, inst.bytes.size());
  return inst.function + ("123456789abcdef"[inst.bytes.size()]);
}

// Apply a callback `cb` to every instruction in the buffer `buff`.
template <typename CB>
static void ForEachInstructionInBuffer(const remill::Arch::ArchPtr &arch,
                                       llvm::StringRef buff, CB cb) {
  const auto max_inst_size = arch->MaxInstructionSize();

  remill::Instruction inst;
  for (size_t i = 0u, max_i = buff.size(); i < max_i; inst.Reset()) {
    auto next_i = std::min<size_t>(max_i, i + max_inst_size);
    std::string_view bytes(&(buff.data()[i]), next_i - i);

    if (!arch->DecodeInstruction(0, bytes, inst) || !inst.IsValid()) {
      LOG(ERROR) << "Unable to decode instruction at byte offset " << i;
      ++i;
    } else {
      cb(inst);
      i += inst.bytes.size();
    }
  }
}

// Check that the decoding of a particular instruction results in position-
// independent operands. It's possible that some operands have PC-relative
// operands and have pre-calculated those values.
static bool IsDecodePositionIndependent(const remill::Arch::ArchPtr &arch,
                                        const remill::Instruction &inst) {
  remill::Instruction copy;
  if (!arch->DecodeInstruction(inst.pc + 32, inst.bytes, copy) ||
      inst.operands.size() != copy.operands.size()) {
    return false;
  }

  for (auto i = 0u; i < inst.operands.size(); ++i) {
    if (inst.operands[i].Serialize() != copy.operands[i].Serialize()) {
      return false;
    }
  }

  return true;
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


static void OptimizeSilently(const remill::Arch *arch, llvm::Module *module,
                      const std::vector<llvm::Function *> &fns) {
  auto saved_threshold = module->getContext().getDiagnosticsHotnessThreshold();
  module->getContext().setDiagnosticsHotnessThreshold(1);
  remill::OptimizeModule(arch, module, fns);
  // Set the logging back to the original values.
  module->getContext().setDiagnosticsHotnessThreshold(saved_threshold);

  // TOOD(lukas): This most likely wants its own class
  // We want to lower some intrinsics that llvm optimizations introduced
  // `usub.sat` is a result of InstCombining.
  std::vector<llvm::Instruction *> calls;
  for (auto fn : fns) {
    for (auto &bb : *fn) {
      for (auto &inst : bb) {
        // NOTE(lukas): I opted to go inst by inst to avoid accidentaly
        //              modifying semantic functions we do not use.
        if (auto cs = llvm::CallSite(&inst)) {
          if (cs.isCall() &&
              cs.getCalledFunction()->getIntrinsicID() == llvm::Intrinsic::usub_sat)
              calls.push_back(cs.getInstruction());
        }
      }
    }
  }
  for (auto inst : calls) {
    llvm::IRBuilder<> ir(inst);
    auto call = llvm::cast<llvm::CallInst>(inst);
    auto a = call->getOperand(0);
    auto b = call->getOperand(1);
    auto size = static_cast<uint32_t>(inst->getType()->getPrimitiveSizeInBits());
    // sub = a - b
    auto sub = ir.CreateSub(a, b);
    // select  = (a < b) ? 0 : a - b;
    auto flag = ir.CreateICmpULT(a, b);
    auto zero = ir.getIntN(size, 0);
    auto select = ir.CreateSelect(flag, zero, sub);
    // replace & erase
    call->replaceAllUsesWith(select);
    call->eraseFromParent();
  }
  // Verify we did not broke anything
  remill::VerifyModule(module);
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

}  // namespace

void CircuitBuilder::State::store(
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

llvm::Value *CircuitBuilder::State::load(llvm::IRBuilder<> &ir, const reg_ptr_t reg) {
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
  if (auto used = module->getGlobalVariable("llvm.used"); used) {
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

  auto isels = DecodeInstructions(buff);
  IdentifyImms(isels);
  LiftInstructions(isels);

  // Delete the `__remill_intrinsics` so that we can get rid of more
  // functions.
  if (auto intrinsics = module->getFunction("__remill_intrinsics");
      intrinsics) {
    intrinsics->eraseFromParent();
  }

  if (auto mark_as_used = module->getFunction("__remill_mark_as_used");
      mark_as_used) {
    mark_as_used->eraseFromParent();
  }

  // TODO(lukas): Move to ctor
  one_of_func = intrinsics::OneOf::CreateFn(module.get());
  verify_inst_func = intrinsics::VerifyInst::CreateFn(module.get());

  // Mark these functions as not touching memory; this will help LLVM to
  // better optimize code that calls these functions.
  one_of_func->addFnAttr(llvm::Attribute::ReadNone);
  verify_inst_func->addFnAttr(llvm::Attribute::ReadNone);

  // These improve optimizability.
  MuteStateEscape(*module, "__remill_function_return");
  MuteStateEscape(*module, "__remill_error");
  MuteStateEscape(*module, "__remill_missing_block");

  return BuildCircuit1(BuildCircuit0(std::move(isels)));
}

template<typename T = uint64_t>
T CircuitBuilder::inst_fragments_count(llvm::CallInst *inst) const {
  auto maybe_count = GetMetadata(inst, bytes_fragments_count_kind);
  CHECK(maybe_count) << "Byte fragments size not set for"
                     << remill::LLVMThingToString(inst);
  return static_cast<T>(*maybe_count);
}

void CircuitBuilder::IdentifyImms(CircuitBuilder::InstSelections &insts) {
  for (auto &inst : insts) {
    for (auto i = 0U; i < inst.instructions.size(); ++i) {
      if (reduce_imms) {
        LOG(INFO) << "Searching for immediate operands regions in:";
        LOG(INFO) << inst.instructions[i].Serialize();
        auto s_inst = InstructionFuzzer{arch, inst.instructions[i]}.FuzzOps();
        inst.imms.emplace_back();
        inst.shadows.push_back(std::move(s_inst));
      } else {
        inst.imms.emplace_back();
        inst.shadows.emplace_back();
      }
    }
  }
}

// Decode all instructions in `buff` using `arch`. Group the instructions in
// terms of a general semantic category/class.
std::vector<InstructionSelection>
CircuitBuilder::DecodeInstructions(llvm::StringRef buff) {
  std::vector<InstructionSelection> grouped_insts;
  std::set<std::string> inst_bytes;
  std::unordered_map<std::string, size_t> isel_index;

  ForEachInstructionInBuffer(arch, buff, [&](remill::Instruction &inst) {
    auto all_zeroes = true;
    for (auto b : inst.bytes) {
      if (b) {
        all_zeroes = false;
        break;
      }
    }

    CHECK(!all_zeroes)
        << "Instructions whose machine code representation is all zeroes are "
        << "not permitted as they would invalidate the XOR-based checking of "
        << "encode verification checks";

    // It's likely that some of Remill's decoders implicitly put position-
    // dependent operands into the operands list, so try to catch that, warn
    // about them, and skip them.
    if (!IsDecodePositionIndependent(arch, inst)) {
      LOG(ERROR) << "Skipping position-dependent instruction: "
                 << inst.Serialize();
      return;
    }

    if (auto num_bits = static_cast<unsigned>(inst.bytes.size() * 8u);
        num_bits > encoded_inst_size) {
      encoded_inst_size = num_bits;
      CHECK_LE(encoded_inst_size, kMaxNumInstBits);
    }

    // Group the unique decoded instructions in terms of their ISELs, i.e. the
    // general semantic category of those instructions.
    if (auto [it, inserted] = inst_bytes.insert(inst.bytes);
        (void) it, inserted) {

      InstructionSelection *iclass = nullptr;

      // Make the isel specific to the `inst.function` and its size.
      const auto isel = IselName(inst);
      if (auto index_it = isel_index.find(isel); index_it != isel_index.end()) {
        iclass = &(grouped_insts[index_it->second]);
        CHECK_EQ(inst.bytes.size(), iclass->instructions.back().bytes.size());

      } else {
        isel_index.emplace(isel, grouped_insts.size());
        grouped_insts.emplace_back();
        iclass = &(grouped_insts.back());

        // iclass->known_bits.set();
      }

      iclass->encodings.emplace_back();

      auto &encoding = iclass->encodings.back();
      size_t i = 0u;
      for (char byte_ : std::string(inst.bytes.rbegin(), inst.bytes.rend())) {
        const auto byte = static_cast<uint8_t>(byte_);
        for (auto b = 0u; b < 8u; ++b, ++i) {
          if ((byte >> b) & 1u) {
            encoding.set(i);
          }
        }
      }

      iclass->instructions.emplace_back(std::move(inst));
    }
  });

  return grouped_insts;
}

// Flatten all control flow into pure data-flow inside of a function.
void CircuitBuilder::FlattenControlFlow(
    llvm::Function *func, const remill::IntrinsicTable &intrinsics) {
  Flattener(func, intrinsics.error).Run();
}

// Decode all instructions in `buff` using `arch` and fill up `inst_funcs`.
void CircuitBuilder::LiftInstructions(
    std::vector<InstructionSelection> &isels) {
  remill::IntrinsicTable intrinsics(module);
  std::vector<llvm::Function *> inst_funcs;

  for (auto &group : isels) {
      CHECK(group.instructions.size() == group.encodings.size());

    for (auto i = 0ull; i < group.instructions.size(); ++i) {
      auto &inst = group.instructions[i];
      std::stringstream ss;
      ss << "inst_" << inst.bytes;

      auto func = remill::DeclareLiftedFunction(module.get(), ss.str());
      group.lifted_fns.push_back(func);

      remill::CloneBlockFunctionInto(func);
      auto block = &func->getEntryBlock();

      InstructionLifter lifter(arch, intrinsics);
      lifter.SupplyShadow(&group.shadows[i]);
      switch (lifter.LiftIntoBlock(inst, block, false, group.imms[i])) {
        case remill::LiftStatus::kLiftedInstruction:
          (void) llvm::ReturnInst::Create(
              context, remill::LoadMemoryPointer(block), block);

          // Make sure these functions stick around.
          func->removeFnAttr(llvm::Attribute::InlineHint);
          func->removeFnAttr(llvm::Attribute::AlwaysInline);
          func->setLinkage(llvm::GlobalValue::ExternalLinkage);
          func->addFnAttr(llvm::Attribute::NoInline);

          inst_funcs.push_back(func);
          continue;

        case remill::LiftStatus::kLiftedUnsupportedInstruction:
          LOG(ERROR) << "Missing semantics for instruction: "
                     << inst.Serialize();
          func->eraseFromParent();
          continue;

        case remill::LiftStatus::kLiftedInvalidInstruction:
          LOG(ERROR) << "Invalid instruction: " << inst.Serialize();
          func->eraseFromParent();
          continue;
        default:
          break;
      }
    }
    CHECK(group.instructions.size() == group.lifted_fns.size());
  }

  OptimizeSilently(arch.get(), module.get(), inst_funcs);

  std::vector<llvm::Function *> reopt_funcs;
  for (auto func : inst_funcs) {
    if (func->size() == 1) {
      continue;  // Pure data-flow; doesn't need to be re-optimized.
    }

    reopt_funcs.push_back(func);
    FlattenControlFlow(func, intrinsics);
  }

  if (!reopt_funcs.empty()) {
    OptimizeSilently(arch.get(), module.get(), reopt_funcs);
  }

  // We're done; make the instruction functions more amenable for inlining
  // and elimination.
  for (auto func : inst_funcs) {
    func->setLinkage(llvm::GlobalValue::PrivateLinkage);
    func->removeFnAttr(llvm::Attribute::NoInline);
    func->addFnAttr(llvm::Attribute::InlineHint);
    func->addFnAttr(llvm::Attribute::AlwaysInline);
  }
}

// Apply a callback `cb(unsgined, const remill::Instruction &, llvm::CallInst *)`
// to each call of `__circuitous.verify_inst` in `circuit_func`.
template <typename T>
void CircuitBuilder::ForEachVerification(llvm::Function *circuit_func, T cb) {
  std::vector<llvm::CallInst *> verify_calls;
  for (auto user : verify_inst_func->users()) {
    if (auto call_inst = llvm::dyn_cast<llvm::CallInst>(user);
        call_inst && call_inst->getParent()->getParent() == circuit_func) {
      verify_calls.push_back(call_inst);
    }
  }
  for (auto call_inst : verify_calls) {
    cb(call_inst);
  }
}

// Build the first level circuit. We will analyze this function later to
// get an accurate picture of instruction dependencies.
auto CircuitBuilder::BuildCircuit0(std::vector<InstructionSelection> isels)
-> Circuit0 {

  auto circuit0 = Circuit0(*this);
  auto circuit0_func = circuit0.Create();

  circuit0.InjectISELs(std::move(isels));

  OptimizeSilently(arch.get(), module.get(), {circuit0_func});

  return circuit0;
}

// Build the second level circuit. Here we analyze how the instruction checkers
// use registers and try to eliminate unneeded registers from the function's
// argument list.
llvm::Function *CircuitBuilder::BuildCircuit1(Circuit0 circuit0) {
  auto circuit0_func = circuit0.GetFn();
  RegisterDependencyCollector deps(arch.get());

  // Look at all calls to instruction verifiers. These function calls take as
  // input the integer index (into `insts`) of the instruction being verified,
  // followed by a variable (really: `regs.size()`) number of `i1` values that
  // should be the results of ICmp instructions, each testing whether or not
  // the current value of a register matches the next expected value of the
  // register.
  ForEachVerification(circuit0_func, [&](llvm::CallInst *verify_call_inst) {
    auto arg_num = 0u;
    auto reg_idx = 0u;
    auto inst_fragments_prefix = inst_fragments_count(verify_call_inst);
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

  // Start with the bits associated with the parts of an encoded instruction.
  // for (i = 0u; i < num_instruction_parts; ++i) {
  //   circuit1_arg_types.push_back(
  //       remill::NthArgument(circuit0_func, i)->getType());
  // }

  circuit1_arg_types.push_back(
      remill::NthArgument(circuit0_func, 0)->getType());

  for (auto in_reg : deps.read_registers) {
    new_regs.push_back(arch->RegisterByName(in_reg->getName().str()));
    circuit1_arg_types.push_back(in_reg->getType());
  }

  for (auto out_reg : deps.written_registers) {
    circuit1_arg_types.push_back(out_reg->getType());
  }

  // The read set should be a subset of the written set.
  CHECK_LE(deps.read_registers.size(), deps.written_registers.size());

  auto circuit1_func = llvm::Function::Create(
      llvm::FunctionType::get(bool_type, circuit1_arg_types, false),
      llvm::GlobalValue::ExternalLinkage, "circuit1_func", module.get());
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
  //    %34 = tail call i1 (i8, i8, ...) @instrinsics::Eq(i8 0, i8 0)
  //
  // Removing uses of these redundant comparisons will let us shrink down the
  // verification call arg lists to only verify used registers.
  for (auto reg : regs) {
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

  auto entry = llvm::BasicBlock::Create(context, "", circuit1_func);
  auto res = llvm::CallInst::Create(circuit0_func, args, "", entry);
  (void) llvm::ReturnInst::Create(context, res, entry);

  // Optimizing the module again will inline circuit0_func into circuit1_func
  // and propagate the null (i.e. zero) values for all unused registers down
  // through the inlined body of circuit0_func.
  OptimizeSilently(arch.get(), module.get(), {circuit1_func});

  // "Constant fold" the uses of `__circuitous.icmp_eq.8`.
  std::vector<llvm::CallInst *> to_fold;

  for (auto matcher : intrinsics::Eq::All(module.get())) {
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
    call_inst->replaceAllUsesWith(true_value);
    call_inst->eraseFromParent();
  }

  std::vector<std::pair<llvm::CallInst *, llvm::CallInst *>> to_replace;

  // Now go through an change the arguments to `__circuitous.verify_inst` to
  // to reflect the new register transfer comparisons.
  ForEachVerification(circuit1_func, [&](llvm::CallInst *call_inst) {
    args.clear();
    auto inst_fragments_prefix = inst_fragments_count<uint32_t>(call_inst);
    for (auto j = 0u; j < inst_fragments_prefix; ++j) {
      args.push_back(call_inst->getArgOperand(j));
    }
    for (i = 0u; i < regs.size(); ++i) {
      const auto reg = regs[i];

      // const auto arg = call_inst->getArgOperand(i + num_instruction_parts);
      const auto arg = call_inst->getArgOperand(i + inst_fragments_prefix);
      if (std::find(new_regs.begin(), new_regs.end(), reg) != new_regs.end()) {
        CHECK(!llvm::isa<llvm::Constant>(arg)) << remill::LLVMThingToString(arg);
        args.push_back(arg);
      }
    }

    auto new_call =
        llvm::CallInst::Create(verify_inst_func, args, "", call_inst);
    to_replace.emplace_back(call_inst, new_call);
  });

  for (auto [old_call, new_call] : to_replace) {
    old_call->replaceAllUsesWith(new_call);
    old_call->eraseFromParent();
  }

  // Make sure `regs` represents the new and reduced set of registers that
  // we're going to be verifying.
  regs.swap(new_regs);

  return circuit1_func;
}

llvm::FunctionType *CircuitBuilder::Circuit0::FnT() {
  llvm::IRBuilder<> ir(parent.context);
  std::vector<llvm::Type *> param_types;

  // First parameter is the bit enconding of the instruction being verified
  param_types.push_back(ir.getIntNTy(kMaxNumInstBits));

    // The remaining parameters will be input/output registers for verification.
  for (auto reg : parent.regs) {
    const auto reg_type = IntegralRegisterType(*parent.module, reg);
    param_types.push_back(reg_type);
    param_types.push_back(reg_type);
  }

  return llvm::FunctionType::get(ir.getInt1Ty(), param_types, false);
}

llvm::Function *CircuitBuilder::Circuit0::GetFn() {
  return parent.module->getFunction(name);
}

llvm::Function *CircuitBuilder::Circuit0::Create() {
  llvm::IRBuilder<> ir(parent.context);
  if (circuit_fn) {
    LOG(FATAL) << "Circuit already has an associated llvm function";
    return circuit_fn;
  }

  if (auto fn = GetFn()) {
    LOG(FATAL) << "The module already has " << name << " function.";
    return fn;
  }

  circuit_fn =
    llvm::Function::Create(FnT(), llvm::GlobalValue::ExternalLinkage, name, parent.module.get());
  circuit_fn->addFnAttr(llvm::Attribute::ReadNone);

  CHECK(circuit_fn->arg_size() > 0);
  inst_bytes.push_back(&*circuit_fn->arg_begin());
  // The rest of arguments should be the registers in/out pairs
  CHECK((circuit_fn->arg_size() - inst_bytes.size()) % 2 == 0);
  for (auto i = inst_bytes.size(); i < circuit_fn->arg_size(); i += 2) {
    // This "order" can be flexible, but better let it go in order
    // of regs.
    auto reg = parent.regs[(i - inst_bytes.size()) / 2];

    auto input_reg = remill::NthArgument(circuit_fn, i);
    auto output_reg = remill::NthArgument(circuit_fn, i + 1);

    input_reg->setName(reg->name);
    output_reg->setName(reg->name + "_next");

    reg_to_args.emplace_back(reg, input_reg, output_reg);

    if (reg->name == parent.arch->ProgramCounterRegisterName()) {
      pc = input_reg;
    }
  }

  // Sanity check, we are going to need pc later on.
  CHECK(pc != nullptr) << "Couldn't find program counter register "
                      << parent.arch->ProgramCounterRegisterName();
  return circuit_fn;
}

void CircuitBuilder::Circuit0::InjectISELs(std::vector<InstructionSelection> isels) {
  CHECK(circuit_fn);

  auto circuit0_func = circuit_fn;

  auto entry_block = llvm::BasicBlock::Create(parent.context, "", circuit0_func);
  auto exit_block = llvm::BasicBlock::Create(parent.context, "", circuit0_func);
  auto prev_block = entry_block;

  for (const auto &isel : isels) {
    prev_block = InjectISEL(isel,  prev_block, exit_block);
  }

  llvm::BranchInst::Create(exit_block, prev_block);

  llvm::IRBuilder<> ir(exit_block);
  ir.CreateRet(ir.CreateCall(parent.one_of_func, verified_insts));
}

llvm::BasicBlock *CircuitBuilder::Circuit0::InjectISEL(
    const InstructionSelection &isel,
    llvm::BasicBlock *prev_block,
    llvm::BasicBlock *exit_block) {
  // Add one basic block per lifted instruction. Each block allocates a
  // separate state structure.
  for (auto i = 0u; i < isel.instructions.size(); ++i) {
    auto inst_block = llvm::BasicBlock::Create(parent.context, "", circuit_fn);
    llvm::BranchInst::Create(inst_block, prev_block);
    prev_block = inst_block;

    InjectSemantic(inst_block, exit_block, ISEL_view(isel, i));
  }
  return prev_block;
}

void CircuitBuilder::Circuit0::InjectSemantic(
    llvm::BasicBlock *inst_block, llvm::BasicBlock *exit_block, ISEL_view isel) {

  const auto &dl = parent.module->getDataLayout();
  CHECK_NOTNULL(isel.lifted);

  llvm::IRBuilder<> ir(inst_block);
  auto state_ptr = ir.CreateAlloca(parent.state_ptr_type->getElementType());

  // All of the following lambdas capture `ir` and `state_ptr`.
  auto access_reg = [&](const auto &reg) {
    const auto reg_type = IntegralRegisterType(*parent.module, reg);
    const auto reg_store_type = ir.getIntNTy(
        static_cast<unsigned>(dl.getTypeAllocSize(reg_type) * 8u));
    auto reg_addr = reg->AddressOf(state_ptr, inst_block);
    auto reg_addr_type = llvm::PointerType::get(reg_store_type, 0);
    if (reg_addr->getType() != reg_addr_type) {
      reg_addr = ir.CreateBitCast(reg_addr, reg_addr_type);
    }
    return std::make_tuple(reg_type, reg_addr_type, reg_store_type, reg_addr);
  };

  auto store_to_reg = [&](const auto &reg, llvm::Value *val) {
    const auto &[reg_type, reg_addr_type, reg_store_type, reg_addr] = access_reg(reg);
    if (reg_type != reg_store_type) {
      val = ir.CreateZExt(val, reg_store_type);
    }
    ir.CreateStore(val, reg_addr);
  };

  auto load_from_reg = [&](const auto &reg) -> llvm::Value * {
    const auto &[reg_type, reg_addr_type, reg_store_type, reg_addr] = access_reg(reg);
    llvm::Value *val = ir.CreateLoad(reg_addr);
    if (reg_type != reg_store_type) {
      val = ir.CreateTrunc(val, reg_type);
    }
    return val;
  };

  for (auto [reg, arg, _] : reg_to_args) {
    store_to_reg(reg, arg);
  }

  auto fn_t = llvm::FunctionType::get(ir.getVoidTy(), {});
  auto breakpoint_begin_fn =
    llvm::cast<llvm::Function>(parent.module->getOrInsertFunction(
      "__circ.break.begin", fn_t).getCallee());
  auto breakpoint_end_fn =
    llvm::cast<llvm::Function>(parent.module->getOrInsertFunction(
      "__circ.end.begin", fn_t).getCallee());

  auto begin = ir.CreateCall(breakpoint_begin_fn, {});
  auto sem_call = CallSemantic(
    ir, isel.lifted, state_ptr, pc, llvm::UndefValue::get(parent.mem_ptr_type));
  auto end = ir.CreateCall(breakpoint_end_fn, {});
  llvm::InlineFunctionInfo info;
  llvm::InlineFunction(sem_call, info);

  auto params = ByteFragments(ir, isel);
  auto fragments_size = params.size();

  LOG(INFO) << "Collecting dst regs";
  std::vector<llvm::Value *> dst_regs;
  for (auto it = llvm::BasicBlock::iterator(begin); it != begin->getParent()->end(); ++it) {
    auto &inst = *it;
    if (auto dst_alloca = GetMetadata(&inst, "circuitous.dst.reg")) {
      dst_regs.push_back(&inst);
    }
    if (auto verify_arg = GetMetadata(&inst, "circuitous.verify_fn_args")) {
      params.push_back(&inst);
    }
  }
  begin->eraseFromParent();
  end->eraseFromParent();

  for (std::size_t i = 0; i < dst_regs.size(); ++i) {
    CHECK(llvm::isa<llvm::PointerType>(dst_regs[i]->getType()));
    auto p_type = llvm::cast<llvm::PointerType>(dst_regs[i]->getType());
    ir.SetInsertPoint(llvm::dyn_cast<llvm::Instruction>(dst_regs[i]));
    auto as_alloca = ir.CreateAlloca(p_type->getPointerElementType(), nullptr,
                                     "DSTA_" + std::to_string(i));
    dst_regs[i]->replaceAllUsesWith(as_alloca);
    llvm::cast<llvm::Instruction>(dst_regs[i])->eraseFromParent();
    dst_regs[i] = as_alloca;
  }
  ir.SetInsertPoint(inst_block);

  auto as_string = [](auto full, auto from, auto size) {
    std::string out;
    for (uint64_t i = 0; i < size; ++i) {
      out += (full[from + i]) ? '1' : '0';
    }
    return out;
  };

  // Final set of parameters are comparisons on whether or not the resulting
  // register after the semantic has executed matches the next state of that
  // register.
  for (auto [reg, _, expected_reg_val] : reg_to_args) {
    LOG(INFO) << "Processing " << reg->name;
    llvm::Value *reg_val = load_from_reg(reg);
    uint64_t proccessed = 0;

    for (std::size_t i = 0; i < isel.instruction.operands.size(); ++i) {
      if (isel.instruction.operands[i].action != remill::Operand::Action::kActionWrite) {
        continue;
      }
      auto &s_op = isel.shadow.operands[i];

      if (!s_op.reg) {
        continue;
      }
      ++proccessed;
      auto &table = s_op.reg->translation_map;
      LOG(INFO) << reg->name;
      LOG(INFO) << table.count(reg->name);
      if (!table.count(reg->name)) {
        continue;
      }
      auto &all_mats = table.find(reg->name)->second;
      CHECK(all_mats.size() == 1);
      const auto &mats = *(all_mats.begin());

      std::size_t current = 0;
      std::vector<llvm::Value *> reg_checks;
      for (auto &[from, size] : s_op.reg->regions) {
        auto extract_fn = intrinsics::Extract::CreateFn(parent.module.get(), from, size);
        auto fragments = ir.CreateCall(extract_fn, {});
        auto constant = ir.getInt(
          llvm::APInt(static_cast<uint32_t>(size), as_string(mats, current, size), 2));
        auto eq = ir.CreateICmpEQ(fragments, constant);

        reg_checks.push_back(eq);
        // After we use `locate_reg` we need to update the builder since some extra
        // instrucitons may have been inserted.

        current += size;
      }

      // TODO(lukas): Generalize.
      CHECK(reg_checks.size() == 2);
      CHECK(proccessed - 1 < dst_regs.size());
      auto eq = ir.CreateAnd(reg_checks[0], reg_checks[1]);
      auto dst_load = ir.CreateLoad(dst_regs[proccessed - 1]);
      reg_val = ir.CreateSelect(eq, dst_load, reg_val);
    }


    auto eq_func = intrinsics::Eq::CreateFn(parent.module.get(), reg_val->getType());
    llvm::Value *eq_args[] = {reg_val, expected_reg_val};
    params.push_back(ir.CreateCall(eq_func, eq_args));
  }

  // Call the instruction verification function. This returns `1` iff we
  // verified the isel decoding (first argument) and if all other arguments
  // (register comparisons) are true.
  ir.SetInsertPoint(exit_block);
  auto call = ir.CreateCall(parent.verify_inst_func, params);
  AddMetadata(call, bytes_fragments_count_kind, fragments_size);
  verified_insts.push_back(call);
}

std::vector<llvm::Value *> CircuitBuilder::Circuit0::ByteFragments(
    llvm::IRBuilder<> &ir, ISEL_view isel) {

  // Reorders bytes so that they can be matched to extract from instruction
  // bytes without extra concats.
  auto generate_raw_bytes = [](auto full, uint64_t from, uint64_t to) {
    std::string out;
    while(true) {
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
    auto fn = intrinsics::ExtractRaw::CreateFn(parent.module.get(), from, to - from );

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
    auto y = ir.CreateCall(
      intrinsics::BitCompare::CreateFn(parent.module.get(), size), {expected_v, x});
    return y;
  };

  auto unknown_regions = isel.shadow.UnknownRegions(rinst_size);
  for (auto [from, to] : shadowinst::FromToFormat(unknown_regions)) {
    LOG(INFO) << "[ " << from << " , " << to << " ]";
    out.push_back(create_bit_check(from, to));
  }
  return out;
}

}  // namespace circuitous
