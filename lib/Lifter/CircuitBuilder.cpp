/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include "CircuitBuilder.h"

#include <gflags/gflags.h>
#include <glog/logging.h>

#include <algorithm>
#include <cstdlib>
#include <initializer_list>
#include <iostream>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <thread>
#include <unordered_set>
#include <vector>

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
    } else {
      return llvm::dyn_cast<llvm::IntegerType>(reg->type);
    }
  } else {
    return llvm::Type::getIntNTy(
        module.getContext(),
        static_cast<unsigned>(
            module.getDataLayout().getTypeAllocSize(reg->type) * 8u));
  }
}

}  // namespace

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

  return BuildCircuit1(BuildCircuit0(std::move(isels)));
}

// Update any references we might have held to functions that could be
// optimized away.
void CircuitBuilder::Refresh(void) {
  auto i = 0u;
  for (auto &func : bit_match_funcs) {
    if (func) {
      std::stringstream ss;
      ss << "__circuitous_icmp_eq_" << i;
      func = module->getFunction(ss.str());
    }
    ++i;
  }

  for (auto &func : select_funcs) {
    if (func) {
      std::stringstream ss;
      ss << "__circuitous_select_" << i;
      func = module->getFunction(ss.str());
    }
    ++i;
  }

  for (auto &func : concat_funcs) {
    if (func) {
      std::stringstream ss;
      ss << "__circuitous_concat_" << i;
      func = module->getFunction(ss.str());
    }
    ++i;
  }
}

// Return a function that does a bitwise comparison of two values of
// type `type`.
llvm::Function *CircuitBuilder::BitMatcherFunc(llvm::Type *type_) {
  const auto type = llvm::dyn_cast<llvm::IntegerType>(type_);
  CHECK_NOTNULL(type);
  const auto num_bits = type->getScalarSizeInBits();
  if (num_bits >= bit_match_funcs.size()) {
    bit_match_funcs.resize(num_bits + 1);
  }

  auto &func = bit_match_funcs[num_bits];
  if (!func) {
    std::stringstream ss;
    ss << "__circuitous_icmp_eq_" << num_bits;
    llvm::Type *param_types[] = {type, type};
    func = llvm::Function::Create(
        llvm::FunctionType::get(bool_type, param_types, false),
        llvm::GlobalValue::ExternalLinkage, ss.str(), module.get());
    func->addFnAttr(llvm::Attribute::ReadNone);
  }

  return func;
}

// Return a function that encodes input bit argument values into an integer
// type.
llvm::Function *CircuitBuilder::BitConcatFunc(llvm::Type *type_) {
  const auto type = llvm::dyn_cast<llvm::IntegerType>(type_);
  CHECK_NOTNULL(type);
  const auto num_bits = type->getScalarSizeInBits();
  if (num_bits >= concat_funcs.size()) {
    concat_funcs.resize(num_bits + 1);
  }

  auto &func = concat_funcs[num_bits];
  if (!func) {
    std::stringstream ss;
    ss << "__circuitous_concat_" << num_bits;
    func = llvm::Function::Create(llvm::FunctionType::get(type, true),
                                  llvm::GlobalValue::ExternalLinkage, ss.str(),
                                  module.get());
    func->addFnAttr(llvm::Attribute::ReadNone);
  }

  return func;
}

// Return a function that selects from one or more values.
llvm::Function *CircuitBuilder::SelectorFunc(llvm::Type *selector_type,
                                             llvm::Type *type_) {
  const auto type = llvm::dyn_cast<llvm::IntegerType>(type_);
  CHECK_NOTNULL(type);
  const auto num_bits = type->getScalarSizeInBits();
  if (num_bits >= select_funcs.size()) {
    select_funcs.resize(num_bits + 1);
  }

  auto &func = select_funcs[num_bits];
  if (!func) {
    const auto num_inputs = selector_type->getScalarSizeInBits();
    std::stringstream ss;
    ss << "__circuitous_select_" << num_bits;
    std::vector<llvm::Type *> param_types(num_inputs + 1, type);
    param_types[0] = selector_type;
    func = llvm::Function::Create(
        llvm::FunctionType::get(type, param_types, false),
        llvm::GlobalValue::ExternalLinkage, ss.str(), module.get());
    func->addFnAttr(llvm::Attribute::ReadNone);
  }

  return func;
}

llvm::CallInst *CircuitBuilder::FinalXor(llvm::Function *in_func) const {
  for (auto &use : xor_all_func->uses()) {
    if (auto call_user = llvm::dyn_cast<llvm::CallInst>(use.getUser());
        call_user && call_user->getParent()->getParent() == in_func) {
      return call_user;
    }
  }

  LOG(FATAL) << "Could not find call to function __circuitous_xor_all in "
             << in_func->getName().str();
  return nullptr;
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
        iclass->known_bits.set();
      }

      iclass->encodings.emplace_back();

      auto &encoding = iclass->encodings.back();
      size_t i = 0u;
      for (char byte_ : inst.bytes) {
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

  for (auto &group : grouped_insts) {

    // Compute known bits.
    const auto encoding_size = group.instructions.back().bytes.size() * 8u;
    for (auto i = 0u; i < encoding_size; ++i) {
      bool all_same = true;
      for (auto j = 1u; j < group.encodings.size(); ++j) {
        const auto &prev_encoding = group.encodings[j - 1u];
        const auto &curr_encoding = group.encodings[j];
        if (prev_encoding.test(i) != curr_encoding.test(i)) {
          all_same = false;
          break;
        }
      }
      group.known_bits.set(i, all_same);
    }
  }

  return grouped_insts;
}

// Breaks apart the instruction encoding into runs of always-known or maybe-
// known bits.
EncodedInstructionParts CircuitBuilder::CreateEncodingTable(
    const std::vector<InstructionSelection> &isels) {

  EncodedInstructionParts parts;
  std::map<std::string, uint64_t> known_by;
  std::set<std::string> unknown_by;

  for (auto i = 0u; i < encoded_inst_size; ++i) {
    known_by.clear();
    unknown_by.clear();

    for (const auto &group : isels) {
      const auto isel = IselName(group.instructions.back());
      if (group.known_bits.test(i)) {
        known_by[isel] = group.encodings.back().test(i);
      } else {
        unknown_by.insert(isel);
      }
    }

    // Extend the prior encoded part, which has the same known-by subset.
    if (!parts.empty() && parts.back().num_bits < 64 &&
        parts.back().unknown_by == unknown_by) {

      auto &prev_part = parts.back();
      prev_part.num_bits += 1u;

      for (auto [func, val] : known_by) {
        auto &prev_val = prev_part.known_by[func];
        prev_val <<= 1ull;
        prev_val |= val;
      }

    } else {
      parts.emplace_back();
      auto &part = parts.back();
      part.known_by.swap(known_by);
      part.unknown_by.swap(unknown_by);
      part.offset = i;
      part.num_bits = 1u;
    }
  }

  std::reverse(parts.begin(), parts.end());

  return parts;
}

// Decode all instructions in `buff` using `arch` and fill up `inst_funcs`.
void CircuitBuilder::LiftInstructions(
    std::vector<InstructionSelection> &isels) {
  remill::IntrinsicTable intrinsics(module);
  remill::InstructionLifter lifter(arch, intrinsics);
  std::vector<llvm::Function *> inst_funcs;

  unsigned g = 0u;
  for (auto &group : isels) {

    unsigned i = 0u;
    for (auto &inst : group.instructions) {
      std::stringstream ss;
      ss << "inst_" << g << '_' << (i++);

      auto func = remill::DeclareLiftedFunction(module.get(), ss.str());
      remill::CloneBlockFunctionInto(func);
      auto block = &func->getEntryBlock();
      switch (lifter.LiftIntoBlock(inst, block, false)) {
        case remill::LiftStatus::kLiftedInstruction:
          (void) llvm::ReturnInst::Create(
              context, remill::LoadMemoryPointer(block), block);

          // Make this function more amenable for inlining and elimination.
          func->setLinkage(llvm::GlobalValue::PrivateLinkage);
          func->removeFnAttr(llvm::Attribute::NoInline);
          func->addFnAttr(llvm::Attribute::InlineHint);
          func->addFnAttr(llvm::Attribute::AlwaysInline);
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
      }
    }

    ++g;
  }
}

// Apply a callback `cb(unsgined, const remill::Instruction &, llvm::CallInst *)`
// to each call of `__circuitous_verify_inst` in `circuit_func`.
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
llvm::Function *
CircuitBuilder::BuildCircuit0(std::vector<InstructionSelection> isels) {

  const auto &dl = module->getDataLayout();

  std::vector<llvm::Type *> param_types;

  const auto encoded_parts = CreateEncodingTable(isels);
  num_instruction_parts = static_cast<unsigned>(encoded_parts.size());

  // The first several parameters will be encoded instruction parts.
  for (const auto &part : encoded_parts) {
    param_types.push_back(llvm::Type::getIntNTy(context, part.num_bits));
  }

  // The remaining parameters will be input/output registers for verification.
  for (auto reg : regs) {
    const auto reg_type = IntegralRegisterType(*module, reg);
    param_types.push_back(reg_type);
    param_types.push_back(reg_type);
  }

  // The `circuit_0` function will be our first attempt at a circuit. It's
  // not meant to be particularly smart. It will use all the registers available,
  // even if the set of input registers doesn't use all those registers. It
  // exists to get us to a point where we can analyze `circuit_0` and learn
  // about the dependencies of the instructions.
  auto circuit0_type = llvm::FunctionType::get(bool_type, param_types, false);
  auto circuit0_func =
      llvm::Function::Create(circuit0_type, llvm::GlobalValue::ExternalLinkage,
                             "circuit_0", module.get());
  circuit0_func->addFnAttr(llvm::Attribute::ReadNone);

  auto entry_block = llvm::BasicBlock::Create(context, "", circuit0_func);
  auto exit_block = llvm::BasicBlock::Create(context, "", circuit0_func);
  auto prev_block = entry_block;

  llvm::Value *pc = nullptr;

  // Build up a mapping between Remill registers and and the input/output
  // register values.
  //
  // NOTE(pag): The first argument is the encoded instruction.
  std::vector<std::pair<const remill::Register *, llvm::Argument *>>
      input_reg_arg;
  std::vector<std::pair<const remill::Register *, llvm::Argument *>>
      output_reg_arg;

  CHECK_LT(0, num_instruction_parts);

  for (auto reg : regs) {
    const auto arg_num = (input_reg_arg.size() * 2u) + num_instruction_parts;
    const auto input_arg = remill::NthArgument(circuit0_func, arg_num);
    const auto output_arg = remill::NthArgument(circuit0_func, arg_num + 1u);

    input_arg->setName(reg->name);
    output_arg->setName(reg->name + "_next");

    input_reg_arg.emplace_back(reg, input_arg);
    output_reg_arg.emplace_back(reg, output_arg);
    if (reg->name == arch->ProgramCounterRegisterName()) {
      pc = input_arg;
    }
  }

  CHECK(pc != nullptr) << "Couldn't find program counter register "
                       << arch->ProgramCounterRegisterName();

  llvm::Value *inst_func_args[remill::kNumBlockArgs] = {};
  inst_func_args[remill::kPCArgNum] = pc;
  inst_func_args[remill::kMemoryPointerArgNum] =
      llvm::UndefValue::get(mem_ptr_type);

  // General parameters array. First used for collecting results of comparing
  // register states. Then used for collecting the bits that we use to encode
  // instructions given a verified instruction.
  std::vector<llvm::Value *> params;
  std::vector<llvm::Value *> eq_params;

  // Vector of return values, one for each result of doing a
  // `__circuitous_verify_decode`.
  std::vector<llvm::Value *> verified_insts;

  auto g = 0u;
  for (const auto &isel : isels) {
    auto i = 0u;
    const auto inst_name = IselName(isel.instructions.back());

    params.clear();

    // Do a decode check of the known bits for this ISEL.
    auto num_known_parts = 0u;
    for (auto &encoded_part : encoded_parts) {
      const auto actual_bits = remill::NthArgument(circuit0_func, i++);
      const auto encoded_part_type = actual_bits->getType();
      const auto encoded_part_bits_it = encoded_part.known_by.find(inst_name);
      if (encoded_part_bits_it != encoded_part.known_by.end()) {
        eq_params.clear();
        eq_params.push_back(llvm::ConstantInt::get(
            encoded_part_type, encoded_part_bits_it->second, false));
        eq_params.push_back(actual_bits);
        params.push_back(
            llvm::CallInst::Create(BitMatcherFunc(encoded_part_type), eq_params,
                                   llvm::None, "", exit_block));
        ++num_known_parts;
      } else {
        params.push_back(nullptr);
      }
    }

    CHECK_EQ(params.size(), num_instruction_parts);
    LOG_IF(FATAL, !num_known_parts)
        << "The encoding of the instruction " << inst_name
        << " is not distinguishable by any specific bits";

    // Add one basic block per lifted instruction. Each block allocates a
    // separate state structure.
    for (i = 0u; i < isel.instructions.size(); ++i) {
      std::stringstream ss;
      ss << "inst_" << g << '_' << i;
      const auto inst_func = module->getFunction(ss.str());
      CHECK_NOTNULL(inst_func);

      auto inst_block = llvm::BasicBlock::Create(context, "", circuit0_func);
      llvm::BranchInst::Create(inst_block, prev_block);
      prev_block = inst_block;

      llvm::IRBuilder<> ir(inst_block);
      const auto state_ptr = new llvm::AllocaInst(
          state_ptr_type->getElementType(), 0, nullptr, "", inst_block);

      for (auto [reg, arg] : input_reg_arg) {
        const auto reg_type = IntegralRegisterType(*module, reg);
        const auto reg_store_type = llvm::Type::getIntNTy(
            context, static_cast<unsigned>(dl.getTypeAllocSize(reg_type) * 8u));

        auto reg_addr = reg->AddressOf(state_ptr, inst_block);
        auto reg_addr_type = llvm::PointerType::get(reg_store_type, 0);
        if (reg_addr->getType() != reg_addr_type) {
          reg_addr =
              new llvm::BitCastInst(reg_addr, reg_addr_type, "", inst_block);
        }

        llvm::Value *reg_val = arg;
        if (reg_type != reg_store_type) {
          reg_val = new llvm::ZExtInst(reg_val, reg_store_type, "", inst_block);
        }

        (void) new llvm::StoreInst(reg_val, reg_addr, false, inst_block);
      }

      inst_func_args[remill::kStatePointerArgNum] = state_ptr;

      llvm::CallInst::Create(inst_func, inst_func_args, "", inst_block);

      // First few arguments are the known parts of this instruction's encoding
      // and are common across all instructions sharing the same ISEL.
      params.resize(num_instruction_parts);

      // Next few arguments are the "unknown" parts, i.e. specific to this
      // encoding of this ISEL.
      const auto &inst_encoding = isel.encodings[i];
      auto p = 0u;
      for (auto &encoded_part : encoded_parts) {
        const auto actual_bits = remill::NthArgument(circuit0_func, p);
        auto &param = params[p++];
        if (!encoded_part.unknown_by.count(inst_name)) {
          CHECK_NOTNULL(param);
          continue;
        }

        CHECK_LE(encoded_part.num_bits, 64);

        uint64_t expected_val = {};
        for (auto b = 0u; b < encoded_part.num_bits; ++b) {
          expected_val <<= 1ull;
          if (inst_encoding.test(encoded_part.offset + b)) {
            expected_val |= 1ull;
          }
        }

        const auto encoded_part_type = actual_bits->getType();

        eq_params.clear();
        eq_params.push_back(
            llvm::ConstantInt::get(encoded_part_type, expected_val));
        eq_params.push_back(actual_bits);
        param = llvm::CallInst::Create(BitMatcherFunc(encoded_part_type),
                                       eq_params, llvm::None, "", exit_block);
      }

      // Final set of paramters are comparisons on whether or not the resulting
      // register after the semantic has executed matches the next state of that
      // register.
      for (auto [reg, expected_reg_val] : output_reg_arg) {
        const auto reg_type = IntegralRegisterType(*module, reg);
        const auto reg_store_type = llvm::Type::getIntNTy(
            context, static_cast<unsigned>(dl.getTypeAllocSize(reg_type) * 8u));

        auto reg_addr = reg->AddressOf(state_ptr, inst_block);
        auto reg_addr_type = llvm::PointerType::get(reg_store_type, 0);
        if (reg_addr->getType() != reg_addr_type) {
          reg_addr =
              new llvm::BitCastInst(reg_addr, reg_addr_type, "", inst_block);
        }

        llvm::Value *reg_val = new llvm::LoadInst(reg_addr, "", inst_block);
        if (reg_type != reg_store_type) {
          reg_val = new llvm::TruncInst(reg_val, reg_type, "", inst_block);
        }

        const auto eq_func = BitMatcherFunc(reg_type);
        llvm::Value *eq_args[] = {reg_val, expected_reg_val};
        params.push_back(
            llvm::CallInst::Create(eq_func, eq_args, "", inst_block));
      }

      // Call the instruction verification function. This returns `1` iff we
      // verified the isel decoding (first argument) and if all other arguments
      // (register comparisons) are true.
      verified_insts.push_back(llvm::CallInst::Create(
          verify_inst_func, params, llvm::None, "", exit_block));
    }
    ++g;
  }

  llvm::BranchInst::Create(exit_block, prev_block);
  (void) llvm::ReturnInst::Create(
      context,
      llvm::CallInst::Create(xor_all_func, verified_insts, llvm::None, "",
                             exit_block),
      exit_block);

  remill::OptimizeModule(arch.get(), module.get(), {circuit0_func});

  return circuit0_func;
}

// Keeps track of instruction dependencies.
template <typename T>
class DependencyVisitor {
 public:
  void VisitArgument(llvm::Use &, llvm::Argument *) {}
  bool VisitInstruction(llvm::Use &, llvm::Instruction *) {
    return true;
  }
  void VisitConstant(llvm::Use &, llvm::Constant *) {}
  void Visit(llvm::Use &use_);
};

// Analyze how `use_` is produced.
template <typename T>
void DependencyVisitor<T>::Visit(llvm::Use &use_) {
  auto self = reinterpret_cast<T *>(this);

  std::vector<llvm::Use *> work_list;
  work_list.emplace_back(&use_);

  while (!work_list.empty()) {
    const auto use = work_list.back();
    work_list.pop_back();

    const auto val = use->get();

    // Bottom out at an argument; it should be an input register.
    if (auto arg_val = llvm::dyn_cast<llvm::Argument>(val); arg_val) {
      self->VisitArgument(*use, arg_val);

    // Instruction; follow the dependency chain.
    } else if (auto inst_val = llvm::dyn_cast<llvm::Instruction>(val);
               inst_val) {
      if (self->VisitInstruction(*use, inst_val)) {
        if (auto call_val = llvm::dyn_cast<llvm::CallInst>(inst_val);
            call_val) {
          for (auto &op_use : call_val->arg_operands()) {
            work_list.push_back(&op_use);
          }
        } else {
          for (auto &op_use : inst_val->operands()) {
            work_list.push_back(&op_use);
          }
        }
      }

    // Bottom out at a constant, ignore for now.
    } else if (auto const_val = llvm::dyn_cast<llvm::Constant>(val);
               const_val) {
      if (!llvm::isa<llvm::Function>(const_val)) {
        self->VisitConstant(*use, const_val);
      }

    } else {
      LOG(ERROR) << "Unexpected value encountered during dependency analysis: "
                 << remill::LLVMThingToString(val);
    }
  }
}

// Collect register dependencies.
class RegisterDependencyCollector
    : public DependencyVisitor<RegisterDependencyCollector> {
 public:
  explicit RegisterDependencyCollector(const remill::Arch *arch_)
      : arch(arch_) {}

  // Analyze how `val` is produced, and what input registers are read to
  // compute `val` or other output registers in the same logical instruction.
  void VisitArgument(llvm::Use &use, llvm::Argument *arg_val) {
    const auto arg_name = arg_val->getName();
    CHECK(!arg_name.endswith("_next"))
        << "Unexpected output register " << arg_val->getName().str()
        << " appears in use chain for computation of next value of "
        << remill::LLVMThingToString(use.getUser());

    CHECK(arch->RegisterByName(arg_name.str()))
        << "Argument " << remill::LLVMThingToString(arg_val)
        << " is not associated with a register";

    read_registers.insert(arg_val);
    auto func = arg_val->getParent();
    for (auto arg_it = func->arg_begin() + arg_val->getArgNo() + 1u;
         arg_it != func->arg_end(); ++arg_it) {
      if (arg_it->getName().endswith("_next") &&
          arg_it->getName().startswith(arg_name)) {
        written_registers.insert(&*arg_it);
        break;
      }
    }
  }

  const remill::Arch *const arch;
  std::set<llvm::Argument *> read_registers;
  std::set<llvm::Argument *> written_registers;
};

// Build the second level circuit. Here we analyze how the instruction checkers
// use registers and try to eliminate unneeded registers from the function's
// argument list.
llvm::Function *CircuitBuilder::BuildCircuit1(llvm::Function *circuit0_func) {

  RegisterDependencyCollector deps(arch.get());

  // Look at all calls to instruction verifiers. These function calls take as
  // input the integer index (into `insts`) of the instruction being verified,
  // followed by a variable (really: `regs.size()`) number of `i1` values that
  // should be the results of ICmp instructions, each testing whether or not
  // the current value of a register matches the next expected value of the
  // register.
  ForEachVerification(circuit0_func, [&](llvm::CallInst *verify_call_inst) {
    auto arg_num = 0u;

    for (auto &arg_use : verify_call_inst->arg_operands()) {
      llvm::Value *arg = arg_use.get();
      if (arg_num < num_instruction_parts) {
        ++arg_num;
        continue;
      }

      // Figure out the input and output registers to the circuit function.
      const auto reg_id = arg_num - num_instruction_parts;
      const auto in_reg_arg_index = num_instruction_parts + (2u * reg_id);
      const auto in_reg_arg =
          remill::NthArgument(circuit0_func, in_reg_arg_index);
      const auto out_reg_arg =
          remill::NthArgument(circuit0_func, in_reg_arg_index + 1u);

      CHECK(out_reg_arg->getName().endswith("_next"));
      CHECK(out_reg_arg->getName().startswith(in_reg_arg->getName()));

      const auto in_reg_name = in_reg_arg->getName().str();
      const auto out_reg_name = out_reg_arg->getName().str();
      ++arg_num;

      const auto call_inst = llvm::dyn_cast<llvm::CallInst>(arg);
      CHECK(call_inst != nullptr)
          << "Unexpected argument value for " << in_reg_name << ": "
          << remill::LLVMThingToString(arg);

      const auto icmp_eq = call_inst->getCalledFunction();
      CHECK_NOTNULL(icmp_eq);
      CHECK(icmp_eq->getName().startswith("__circuitous_icmp_eq_"));

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
  for (i = 0u; i < num_instruction_parts; ++i) {
    circuit1_arg_types.push_back(
        remill::NthArgument(circuit0_func, i)->getType());
  }

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
  i = num_instruction_parts;
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

  // Encoding of instruction to be verified.
  for (i = 0u; i < num_instruction_parts; ++i) {
    args.push_back(remill::NthArgument(circuit1_func, i));
  }

  // Build up an argument list to call circuit0_func from circuit1_func. We
  // pass through the arguments associated with registers that the above
  // analysis determined to be used, and pass in null values (zeroes) for the
  // rest. We'll be able to observe calls like the following:
  //
  //    %34 = tail call i1 (i8, i8, ...) @__circuitous_icmp_eq_8(i8 0, i8 0)
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
  remill::OptimizeModule(arch.get(), module.get(), {circuit1_func});
  Refresh();

  // "Constant fold" the uses of `__circuitous_icmp_eq_8`.
  std::vector<llvm::CallInst *> to_fold;

  for (auto matcher : bit_match_funcs) {
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

  // Now go through an change the arguments to `__circuitous_verify_inst` to
  // to reflect the new register transfer comparisons.
  ForEachVerification(circuit1_func, [&](llvm::CallInst *call_inst) {
    args.clear();
    for (i = 0u; i < num_instruction_parts; ++i) {
      args.push_back(call_inst->getArgOperand(i));
    }
    for (i = 0u; i < regs.size(); ++i) {
      const auto reg = regs[i];
      const auto arg = call_inst->getArgOperand(i + num_instruction_parts);
      if (std::find(new_regs.begin(), new_regs.end(), reg) != new_regs.end()) {
        CHECK(!llvm::isa<llvm::Constant>(arg));
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

// If `val` is a call to a bitwise comparison function, then return the two
// compared values; otherwise return two NULL pointers.
static std::pair<llvm::Value *, llvm::Value *> GetICmpArgs(llvm::Value *val) {
  if (auto call_inst = llvm::dyn_cast<llvm::CallInst>(val);
      call_inst && call_inst->getCalledFunction()->getName().startswith(
                       "__circuitous_icmp_eq_")) {
    return {call_inst->getArgOperand(0), call_inst->getArgOperand(1)};
  } else {
    return {nullptr, nullptr};
  }
}

// Build the third level circuit. Here we merge together multiple
// verification calls so that we can merge decoding effort. This is register-
// centric, rather than instruction encoding centric. That is, we're looking
// for sets of instructions that preserve lots of the same registers, and trying
// to merge their decoders.
llvm::Function *CircuitBuilder::BuildCircuit2(llvm::Function *circuit1_func) {
  using RegBitSet = std::bitset<128>;
  CHECK_GT(128, regs.size());

  std::unordered_map<llvm::CallInst *, RegBitSet> preserved_regs;

  ForEachVerification(circuit1_func, [&](llvm::CallInst *verify_call_inst) {
    auto arg_num = 0u;

    auto &known_regs = preserved_regs[verify_call_inst];

    for (auto &arg_use : verify_call_inst->arg_operands()) {
      llvm::Value *arg = arg_use.get();
      if (arg_num < num_instruction_parts) {
        ++arg_num;
        continue;
      }

      // Figure out the input and output registers to the circuit function.
      const auto reg_id = arg_num - num_instruction_parts;
      ++arg_num;
      if (reg_id >= regs.size()) {
        return;
      }

      const auto reg = regs[reg_id];
      const auto in_reg_arg_index = num_instruction_parts + reg_id;
      const auto in_reg_arg =
          remill::NthArgument(circuit1_func, in_reg_arg_index);
      CHECK_EQ(in_reg_arg->getName().str(), reg->name)
          << "Could not correlate argument to __circuitous_verify_inst with "
          << "register " << reg->name << "; correlated to "
          << remill::LLVMThingToString(in_reg_arg) << " instead";

      const auto call_inst = llvm::dyn_cast<llvm::CallInst>(arg);
      CHECK(call_inst != nullptr)
          << "Unexpected argument value for transfer check of " << reg->name
          << ": " << remill::LLVMThingToString(arg);

      const auto icmp_eq = call_inst->getCalledFunction();
      CHECK_NOTNULL(icmp_eq);
      CHECK(icmp_eq->getName().startswith("__circuitous_icmp_eq_"));

      const auto lhs = call_inst->getArgOperand(0);
      const auto rhs = call_inst->getArgOperand(1);

      const auto rhs_arg = llvm::dyn_cast<llvm::Argument>(rhs);
      CHECK(rhs_arg->getName().startswith(reg->name) &&
            rhs_arg->getName().endswith("_next"))
          << "Expected second argument to " << icmp_eq->getName().str()
          << " to match the output register " << reg->name << "_next but got "
          << remill::LLVMThingToString(rhs)
          << " instead: " << remill::LLVMThingToString(call_inst);

      // Input is compared to output, i.e. the instruction doesn't write
      // to this register.
      if ((llvm::isa<llvm::Argument>(lhs) && lhs == in_reg_arg)) {
        known_regs.set(reg_id);
      }
    }
  });

  using PairwisePreservedRegs =
      std::tuple<llvm::CallInst *, llvm::CallInst *, RegBitSet>;
  std::vector<PairwisePreservedRegs> pairwise_preserved_regs;
  std::unordered_map<RegBitSet, std::set<llvm::CallInst *>> common;
  for (auto [c1, c1_bits] : preserved_regs) {
    for (auto [c2, c2_bits] : preserved_regs) {
      if (c1 < c2) {
        const auto common_regs = c1_bits & c2_bits;
        pairwise_preserved_regs.emplace_back(c1, c2, common_regs);
        auto &common_set = common[common_regs];
        common_set.insert(c1);
        common_set.insert(c2);
      }
    }
  }

  // First, sort so that the pairs with the largest associated set of
  // instructions in common ends up at the end.
  std::sort(
      pairwise_preserved_regs.begin(), pairwise_preserved_regs.end(),
      [&](const PairwisePreservedRegs &a, const PairwisePreservedRegs &b) {
        return common[std::get<2>(a)].size() < common[std::get<2>(b)].size();
      });

  // Then, stable sort the list in so that the sets with the highest population
  // count, i.e. the most number of preserved registers in common, ends up at
  // the end. This will make it more likely that we end up discovering groups of
  // related instructions.
  std::stable_sort(
      pairwise_preserved_regs.begin(), pairwise_preserved_regs.end(),
      [=](const PairwisePreservedRegs &a, const PairwisePreservedRegs &b) {
        return std::get<2>(a).count() < std::get<2>(b).count();
      });

  std::set<llvm::CallInst *> seen;
  std::unordered_map<RegBitSet, std::vector<llvm::CallInst *>> merge_sets;

  const auto max_args = num_instruction_parts + regs.size();

  size_t selector_size = 0;
  while (!pairwise_preserved_regs.empty()) {
    auto [c1, c2, common_regs] = pairwise_preserved_regs.back();
    pairwise_preserved_regs.pop_back();

    // This captures NOPs, e.g. `mov eax, eax` on 32-bit x86. Ideally, we want
    // these NOP-like operations to end up getting handled along with the non-
    // NOP variants, e.g. `mov ebx, eax`.
    if (common_regs.count() == regs.size()) {
      continue;
    }

    CHECK_LT(common_regs.count(), regs.size());

    auto &candidates = merge_sets[common_regs];

    // Find the set of related instructions that we want to merge (in terms of
    // decoding).
    for (llvm::CallInst *call_inst : common[common_regs]) {
      if (seen.count(call_inst)) {
        continue;
      } else {
        CHECK_EQ(call_inst->getNumArgOperands(), max_args);
        candidates.push_back(call_inst);
        seen.insert(call_inst);
      }
    }

    selector_size = std::max(selector_size, candidates.size());
  }

  if (!selector_size) {
    return circuit1_func;
  }

  for (auto &[bits, set] : merge_sets) {
    if (set.empty()) {
      continue;
    }


    std::cerr << "Set of size " << set.size() << " with " << bits.count()
              << " common regs: ";
    for (auto i = 0u; i < regs.size(); ++i) {
      std::cerr << ("01"[bits.test(i)]);
    }
    std::cerr << '\n';
  }

  auto inst_selector_type =
      llvm::IntegerType::get(context, static_cast<unsigned>(selector_size));
  auto hint_matcher_func = BitMatcherFunc(inst_selector_type);
  auto inst_selector_func =
      llvm::Function::Create(llvm::FunctionType::get(inst_selector_type, false),
                             llvm::GlobalValue::ExternalLinkage,
                             "__circuitous_inst_select", module.get());
  inst_selector_func->addFnAttr(llvm::Attribute::ReadNone);

  const auto entry_inst =
      &*(circuit1_func->getEntryBlock().getFirstInsertionPt());

  // We might need to add selectors that need to select dummy values for some
  // registers. In those cases, we want to select the negation of the expected
  // output of the register, because that will never verify against the
  // expected output.
  std::vector<llvm::Value *> negated_regs;
  for (auto reg : regs) {
    auto i = negated_regs.size();
    const auto out_reg = remill::NthArgument(
        circuit1_func, num_instruction_parts + regs.size() + i);
    CHECK(out_reg->getName().startswith(reg->name));
    CHECK(out_reg->getName().endswith("_next"));
    const auto negated_out_reg = llvm::BinaryOperator::CreateXor(
        out_reg, llvm::ConstantInt::getAllOnesValue(out_reg->getType()), "");
    negated_out_reg->insertBefore(entry_inst);
    negated_regs.push_back(negated_out_reg);
  }

  const auto decode_hint = llvm::CallInst::Create(
      inst_selector_func, llvm::None, llvm::None, "decode_hint", entry_inst);

  std::vector<llvm::Value *> vals_to_compare;
  std::vector<llvm::Value *> vals_to_concat;
  std::vector<llvm::Value *> merged_args;

  const auto xor_all = FinalXor(circuit1_func);

  for (auto &[bits, set] : merge_sets) {
    if (set.empty()) {
      continue;
    }

    merged_args.clear();

    for (auto i = 0u; i < max_args; ++i) {
      const auto [first_val, expected_output] =
          GetICmpArgs(set.front()->getArgOperand(i));
      CHECK_NOTNULL(first_val);
      const auto input_arg = remill::NthArgument(circuit1_func, i);

      CHECK_EQ(first_val->getType(), input_arg->getType())
          << "Didn't match types of " << i << "th compared value "
          << remill::LLVMThingToString(first_val) << " to circuit1 argument "
          << remill::LLVMThingToString(input_arg);

      (void) expected_output;
      auto selected_val = first_val;

      vals_to_compare.clear();

      //      vals_to_select.clear();
      //      vals_to_select.push_back(decode_hint);

      for (llvm::CallInst *call_inst : set) {
        const auto [arg, expected_output] =
            GetICmpArgs(call_inst->getArgOperand(i));
        (void) expected_output;
        vals_to_compare.push_back(arg);
        if (arg != selected_val) {
          selected_val = nullptr;
        }
      }

      llvm::Value *match_val = nullptr;

      // Instruction part.
      if (i < num_instruction_parts) {
        match_val = remill::NthArgument(circuit1_func, i);

      // Find the output register value.
      } else {
        match_val = remill::NthArgument(circuit1_func, regs.size() + i);
      }

      auto match_func = BitMatcherFunc(match_val->getType());
      llvm::Value *match_args[] = {selected_val, match_val};

      CHECK_EQ(match_val->getType(), first_val->getType())
          << "Type of " << remill::LLVMThingToString(match_val)
          << " doesn't match type of " << remill::LLVMThingToString(first_val);

      // All calls share the same value.
      if (selected_val) {

        merged_args.push_back(llvm::CallInst::Create(match_func, match_args,
                                                     llvm::None, "", xor_all));

      // Not all calls share the same value; we need to make a hint-based
      // selector.
      } else {

        // Our selector is `selector_size` bits wide, so we always need to
        // provide as many values as there are bits. Make sure that we always
        // feed the right number of values to the selector function.
        while (vals_to_compare.size() < selector_size) {
          if (i < num_instruction_parts) {
            vals_to_compare.push_back(
                llvm::ConstantInt::getNullValue(first_val->getType()));

          // If we're padding it out with a register then we want to select the
          // negation of the OUTPUT register, which will never match the output
          // register, and thus will not let this instruction accidentally
          // validate.
          } else {
            const auto negated_reg = negated_regs[i - num_instruction_parts];
            CHECK_EQ(negated_reg->getType(), first_val->getType());
            vals_to_compare.push_back(negated_reg);
          }
        }

        CHECK_EQ(vals_to_compare.size(), selector_size);

        vals_to_concat.clear();
        for (auto val : vals_to_compare) {
          match_args[0] = val;
          vals_to_concat.push_back(llvm::CallInst::Create(
              match_func, match_args, llvm::None, "", xor_all));
        }

        auto concat_func = BitConcatFunc(inst_selector_type);
        auto concat_val = llvm::CallInst::Create(concat_func, vals_to_concat,
                                                 llvm::None, "", xor_all);
        auto masked_concat_val =
            llvm::BinaryOperator::CreateAnd(concat_val, decode_hint, "");
        masked_concat_val->insertBefore(xor_all);

        match_args[0] = masked_concat_val;
        match_args[1] = decode_hint;

        merged_args.push_back(llvm::CallInst::Create(
            hint_matcher_func, match_args, llvm::None, "", xor_all));
      }
    }

    auto verify_call = llvm::CallInst::Create(verify_inst_func, merged_args,
                                              llvm::None, "", xor_all);

    set.front()->replaceAllUsesWith(verify_call);
    for (llvm::CallInst *call_inst : set) {
      call_inst->replaceAllUsesWith(false_value);
      call_inst->eraseFromParent();
    }
  }

  remill::OptimizeModule(arch.get(), module.get(), {circuit1_func});
  Refresh();

  // Remove all constants (false) that replaced prior calls to verify functions.
  std::vector<llvm::Value *> new_xor_args;
  for (auto &arg_use : xor_all->arg_operands()) {
    const auto arg = arg_use.get();
    if (!llvm::isa<llvm::Constant>(arg)) {
      new_xor_args.push_back(arg);
    }
  }

  xor_all->replaceAllUsesWith(
      llvm::CallInst::Create(xor_all_func, new_xor_args, "", xor_all));
  xor_all->eraseFromParent();

  circuit1_func->setName("circuit2_func");
  return circuit1_func;
}

// Go find the "root" operands of each instruction.
class OperandCollector : public DependencyVisitor<OperandCollector> {
 public:
  bool VisitInstruction(llvm::Use &, llvm::Instruction *inst) {

    // If it's is passing through one register to the next, then don't consider
    // this an operand. For example:
    //
    //      __circuitous_icmp_eq_32(i32 %EDX, i32 %EDX_next)
    //
    // We will return `false` for the above case.
    if (auto [lhs, rhs] = GetICmpArgs(inst);
        lhs && rhs && llvm::isa<llvm::Argument>(lhs) &&
        llvm::isa<llvm::Argument>(rhs) && rhs->getName().endswith("_next") &&
        rhs->getName().startswith(lhs->getName())) {
      return false;
    }

    return true;
  }

  //  void VisitConstant(llvm::Use &use, llvm::Constant *val) {
  //    if (llvm::isa<llvm::ConstantInt>(val) || llvm::isa<llvm::ConstantFP>(val)) {
  //      if (auto user_inst = llvm::dyn_cast<llvm::Instruction>(use.getUser());
  //          user_inst) {
  //
  //        // Don't turn these constants into operands. Things like ANDs and ORs
  //        // can likely be converted into truncations/extensions, shifts are in
  //        // general free, and XORs are cheap.
  //        switch (user_inst->getOpcode()) {
  //          case llvm::Instruction::And:
  //          case llvm::Instruction::Or:
  //          case llvm::Instruction::Xor:
  //          case llvm::Instruction::Shl:
  //          case llvm::Instruction::LShr:
  //          case llvm::Instruction::AShr:
  //            return;
  //          default:
  //            break;
  //        }
  //      }
  //
  //      operands.push_back(&use);
  //    } else {
  //      LOG(ERROR)
  //          << "Unexpected constant: " << remill::LLVMThingToString(val);
  //    }
  //  }

  // Add named, non-output registers as operands.
  void VisitArgument(llvm::Use &use, llvm::Argument *arg) {
    operands.push_back(&use);

    //    if (arg->hasName() &&
    //        !arg->getName().empty() &&
    //        !arg->getName().endswith("_next")) {
    //      operands.push_back(&use);
    //    }
  }

  std::vector<llvm::Use *> operands;
};

static uint64_t
UserPreference(std::unordered_map<llvm::Function *, uint64_t> &func_ids,
               const llvm::DataLayout &dl, llvm::Use *use) {
  uint64_t id = 0u;
  llvm::Instruction *user_inst =
      llvm::dyn_cast<llvm::Instruction>(use->getUser());
  if (!user_inst) {
    return id;
  }

  // We want the user preference to be sensitive to which operand it's part of
  // so that ideally we do things like assign operand numbers to all left-hand
  // sides of all shift operands first.
  auto op_num = 0u;
  for (auto &op_use : user_inst->operands()) {
    if (&op_use == use) {
      break;
    }
    ++op_num;
  }

  if (auto call_inst = llvm::dyn_cast<llvm::CallInst>(user_inst); call_inst) {
    if (auto called_func = call_inst->getCalledFunction(); called_func) {
      id |= func_ids[called_func];
    }

  } else if (auto cmp_inst = llvm::dyn_cast<llvm::CmpInst>(user_inst);
             cmp_inst) {
    id |= ~static_cast<unsigned>(cmp_inst->getPredicate());
  }

  id <<= 16;
  id |= dl.getTypeSizeInBits(use->get()->getType());
  id <<= 12;
  id |= user_inst->getOpcode();
  id <<= 8;
  id |= op_num;
  return id;
}

// Build the fourth level circuit. Here we analyze how registers are used
// across instructions and we try to break dependencies and merge common
// sub-expressions.
//
// Our goal is to find: how many constants and input registers do each
// instruction need, then to provide "opaque" register inputs in their place,
// along with register name identifiers. That is, if we have something like:
//
//      mov eax, ebx
//      mov eax, ecx
//
// Then we want to say this:
//
//      mov eax, op_32_0()   op_32_0() == ebx
//      mov eax, op_32_0()   op_32_0() == ecx
llvm::Function *CircuitBuilder::BuildCircuit3(llvm::Function *circuit2_func) {

  const auto &dl = module->getDataLayout();

  std::unordered_map<llvm::Function *, uint64_t> func_ids;
  for (auto &func : *module) {
    func_ids.emplace(&func, func_ids.size() + 1u);
  }

  std::unordered_map<llvm::CallInst *, std::vector<llvm::Value *>> new_params;
  std::unordered_map<llvm::Use *, llvm::Value *> use_to_val;
  std::unordered_map<llvm::Use *, std::set<llvm::CallInst *>> use_to_verifier;
  std::map<uint64_t, std::vector<llvm::Use *>> use_preferences;

  // Find all expressions transitively used by each instruction.
  ForEachVerification(circuit2_func, [&](llvm::CallInst *verify_call_inst) {
    OperandCollector collector;
    auto i = 0u;
    auto &params = new_params[verify_call_inst];
    for (auto &arg : verify_call_inst->arg_operands()) {
      if (i >= num_instruction_parts) {
        collector.Visit(arg);  // Only visit register transfer verifications.
      }
      ++i;
      params.push_back(arg.get());
    }

    for (auto use : collector.operands) {
      use_to_verifier[use].insert(verify_call_inst);
      use_to_val.emplace(use, use->get());
      use_preferences[UserPreference(func_ids, dl, use)].push_back(use);
    }
  });

  std::set<llvm::CallInst *> unique_verifiers;
  auto find_verifiers = [&](const std::vector<llvm::Use *> *uses) {
    unique_verifiers.clear();
    for (auto use : *uses) {
      auto &verifiers = use_to_verifier[use];
      unique_verifiers.insert(verifiers.begin(), verifiers.end());
    }
  };

  // Get the
  std::vector<const std::vector<llvm::Use *> *> preferences;
  for (const auto &[pred, uses] : use_preferences) {
    preferences.push_back(&uses);
  }

  std::sort(preferences.begin(), preferences.end(),
            [&](const std::vector<llvm::Use *> *a,
                const std::vector<llvm::Use *> *b) {
              if (a->size() != b->size()) {
                return a->size() > b->size();
              }

              find_verifiers(a);
              const auto a_size = unique_verifiers.size();

              find_verifiers(b);
              const auto b_size = unique_verifiers.size();

              if (a_size != b_size) {
                return a_size > b_size;
              }

              // Arbitrary.
              return UserPreference(func_ids, dl, a->at(0)) <
                     UserPreference(func_ids, dl, b->at(0));
            });

  std::unordered_map<llvm::CallInst *, std::set<std::string>> used_ops;
  std::unordered_map<std::string, unsigned> op_count;

  auto find_free_name = [&](const std::vector<llvm::Use *> *uses) {
    std::stringstream ss;
    ss << "__circuitous_op_"
       << dl.getTypeSizeInBits(uses->at(0)->get()->getType());

    find_verifiers(uses);
    auto &count = op_count[ss.str()];

    // See if any of the others are free.
    for (auto i = 0u; i < count; ++i) {
      std::stringstream ss2;
      ss2 << ss.str() << '_' << i;
      const auto sub_name = ss2.str();
      auto found = false;
      for (auto verifier : unique_verifiers) {
        if (used_ops[verifier].count(sub_name)) {
          found = true;
          break;
        }
      }

      if (!found) {
        ss << '_' << i;
        return ss.str();
      }
    }

    ss << '_' << (count++);
    return ss.str();
  };

  auto insert_pt = circuit2_func->getEntryBlock().getFirstInsertionPt();
  std::unordered_map<std::string, llvm::Value *> op_values;

  const auto xor_all = FinalXor(circuit2_func);

  for (auto uses : preferences) {
    if (2 > uses->size()) {
      continue;
    }

    const auto name = find_free_name(uses);
    find_verifiers(uses);

    auto &op_val = op_values[name];
    if (!op_val) {
      auto operand_func = module->getFunction(name);
      if (!operand_func) {
        operand_func = llvm::Function::Create(
            llvm::FunctionType::get(uses->at(0)->get()->getType(), false),
            llvm::GlobalValue::ExternalLinkage, name, module.get());
        operand_func->addFnAttr(llvm::Attribute::ReadNone);
      }

      op_val = llvm::CallInst::Create(operand_func, "", &*insert_pt);
    }

    for (llvm::CallInst *verifier : unique_verifiers) {
      used_ops[verifier].insert(name);
    }

    for (auto use : *uses) {
      llvm::Value *new_val = nullptr;
      const auto val = use_to_val[use];
      const auto val_type = val->getType();
      if (use->get() == val) {

        auto user_inst = llvm::dyn_cast<llvm::Instruction>(use->getUser());
        CHECK_NOTNULL(user_inst);

        new_val = op_val;
        use->set(new_val);

      // What's happening here is that two different instructions already
      // have this subexpression in common.
      //
      // TODO(pag): Consider moving this earlier, e.g. so that we do the
      //            'operandization' on instructions, before they ever get
      //            optimized in a full circuit setting.
      } else {
        new_val = use->get();
      }

      llvm::Value *args[] = {val, new_val};
      auto op_verifier =
          llvm::CallInst::Create(BitMatcherFunc(val_type), args, "", xor_all);

      for (auto verify_call : use_to_verifier[use]) {
        auto &params = new_params[verify_call];
        params.push_back(op_verifier);
      }
    }
  }
  //
  //  // Restructure all bit matcher function calls to come before all verify calls.
  //  std::vector<llvm::CallInst *> to_move;
  //  for (auto bit_match_func : bit_match_funcs) {
  //    if (!bit_match_func) {
  //      continue;
  //    }
  //    for (auto &use : bit_match_func->uses()) {
  //      if (auto call_user = llvm::dyn_cast<llvm::CallInst>(use.getUser());
  //          call_user && call_user->getParent()->getParent() == circuit2_func) {
  //        to_move.push_back(call_user);
  //      }
  //    }
  //  }
  //
  //  for (auto call_inst : to_move) {
  //    call_inst->removeFromParent();
  //    call_inst->insertBefore(xor_all);
  //  }

  // Put all verify calls before the xor all call.
  for (auto &[verify_call, params] : new_params) {
    verify_call->replaceAllUsesWith(llvm::CallInst::Create(
        verify_call->getCalledFunction(), params, "", xor_all));
    verify_call->eraseFromParent();
  }

  remill::OptimizeModule(arch.get(), module.get(), {circuit2_func});
  Refresh();

  new_params.clear();
  std::unordered_set<llvm::Value *> seen_args;
  const auto normal_args = num_instruction_parts + regs.size();

  // Go find the non-redundant arguments to the verify functions now that we've
  // re-optimized the module.
  ForEachVerification(circuit2_func, [&](llvm::CallInst *verify_call_inst) {
    auto &params = new_params[verify_call_inst];
    seen_args.clear();
    unsigned i = 0u;
    for (auto &arg_use : verify_call_inst->arg_operands()) {
      const auto arg = arg_use.get();
      if (i < normal_args || !seen_args.count(arg)) {
        params.push_back(arg);
        seen_args.insert(arg);
      }
      ++i;
    }
  });

  for (auto &[verify_call, params] : new_params) {
    verify_call->replaceAllUsesWith(llvm::CallInst::Create(
        verify_call->getCalledFunction(), params, "", xor_all));
    verify_call->eraseFromParent();
  }

  circuit2_func->setName("circuit3_func");
  return circuit2_func;
}

//// Produces an integral summary of an LLVM value that describes its type, kind,
//// etc.
//static uint32_t SummarizeValue(llvm::Module &module, llvm::Value *val) {
//
//  union {
//    uint32_t flat;
//    struct {
//      uint32_t size:8;
//      uint32_t kind:2;
//      uint32_t opcode:8;
//      uint32_t predicate:10;
//      static_assert((1u << 10) > llvm::Intrinsic::num_intrinsics);
//    } __attribute__((packed)) as_inst;
//    struct {
//      uint32_t size:8;
//      uint32_t kind:2;
//      uint32_t arg_num:22;
//    } __attribute__((packed)) as_arg;
//    struct {
//      uint32_t size:8;
//      uint32_t kind:2;
//      uint32_t _unused:22;
//    } __attribute__((packed)) as_const;
//  } __attribute__((packed)) key = {};
//
//  static_assert(sizeof(key) == sizeof(uint32_t));
//
//  const auto &dl = module.getDataLayout();
//  const auto val_size = dl.getTypeSizeInBits(val->getType());
//  CHECK_EQ(val_size, static_cast<uint8_t>(val_size));
//
//  if (auto arg_val = llvm::dyn_cast<llvm::Argument>(val); arg_val) {
//    key.as_arg.size = static_cast<uint32_t>(val_size);
//    key.as_arg.kind = 1;
//    key.as_arg.arg_num = arg_val->getArgNo();
//
//  } else if (auto inst_val = llvm::dyn_cast<llvm::Instruction>(val);
//             inst_val) {
//    key.as_inst.size = static_cast<uint32_t>(val_size);
//    key.as_inst.kind = 2;
//    key.as_inst.opcode = inst_val->getOpcode();
//
//    if (auto cmp_inst = llvm::dyn_cast<llvm::CmpInst>(inst_val); cmp_inst) {
//      key.as_inst.predicate = cmp_inst->getPredicate();
//    } else if (auto call_inst = llvm::dyn_cast<llvm::CallInst>(inst_val);
//               call_inst) {
//      key.as_inst.predicate = call_inst->getIntrinsicID();
//    }
//
//  } else if (auto const_val = llvm::dyn_cast<llvm::Constant>(val);
//             const_val) {
//    key.as_const.size = static_cast<uint32_t>(val_size);
//    key.as_const.kind = 3;
//
//  } else {
//    LOG(FATAL)
//        << "Unknown value: " << remill::LLVMThingToString(val);
//  }
//
//  return key.flat;
//}
//
//// Context of a worker thread.
//class alignas(64) Context {
// public:
//  explicit Context(llvm::Function *func)
//      : module("", context) {
//
//    auto mod = func->getParent();
//    module.setTargetTriple(mod->getTargetTriple());
//    module.setDataLayout(mod->getDataLayout());
//
//    auto circuit_func_type = llvm::dyn_cast<llvm::FunctionType>(
//        remill::RecontextualizeType(func->getFunctionType(), context));
//    circuit_func = llvm::Function::Create(
//        circuit_func_type, llvm::GlobalValue::InternalLinkage, func->getName(),
//        &module);
//
//    remill::ValueMap map;
//    for (auto &arg : func->args()) {
//      map.emplace(&arg, remill::NthArgument(circuit_func, arg.getArgNo()));
//    }
//
//    remill::CloneFunctionInto(func, circuit_func, map);
//  }
//
// protected:
//  std::thread worker;
//  llvm::LLVMContext context;
//  llvm::Module module;
//  llvm::Function *circuit_func;
//};
//
//static bool UsesInterfere(const std::set<llvm::CallInst *> &group_a,
//                          const std::set<llvm::CallInst *> &group_b) {
//  for (auto a : group_a) {
//    if (group_b.count(a)) {
//      return true;
//    }
//  }
//  return false;
//}

//
//  std::unordered_map<uint64_t, size_t> min_pref_size;
//
//  for (auto &[pref, op_lists] : use_preferences) {
//    (void) pref;
//    op_lists.resize(v);
//
//    auto has_empty = false;
//    for (const auto &op_list : op_lists) {
//      if (op_list.empty()) {
//        has_empty = true;
//      }
//    }
//
//    if (has_empty || op_lists.empty()) {
//      continue;
//    }
//
//    auto &min = min_pref_size[pref];
//    min = op_lists[0].size();
//
//    for (const auto &op_list : op_lists) {
//      min = std::min(min, op_list.size());
//    }
//  }


//  std::unordered_map<std::string, unsigned> op_num;
//  for (const auto &[pref, op_lists] : use_preferences) {
//    for (auto i = 0u; i < v; ++i) {
//      (void) pref;
//      for (auto use : op_lists[v]) {
//
//      }
//    }
//  }
//
//
//
//    // TODO(pag):
//    //
//    //    - Instead of collecting all the things ahead-of-time once, do it one
//    //      at a time here
//    //    - Don't do replacements for pass-through verifications
//    //    - Look into memory addresses -- why aren't they replaced?
//
//
//    // Copy the parameters of the old verification call.
//    params.clear();
//    for (auto &arg : verify_call->arg_operands()) {
//      params.push_back(arg.get());
//    }
//
//    for (const auto &[_, operands] : use_preferences) {
//      for (auto use : operands) {
//        llvm::Value *new_val = nullptr;
//        const auto val = use_to_val[use];
//        const auto val_type = val->getType();
//        if (val == use->get()) {
//

//

//      }
//    }
//
//    verify_call->eraseFromParent();
//  });


//  (void) UsesInterfere;
//  (void) ValueId;
//
//
//  std::unordered_map<uint32_t, std::vector<llvm::Use *>> merge_candidates;
//
//  // Group the transitively used expressions in terms of their kind, i.e. all
//  // 8-bit adds get grouped together.
//  for (auto &[use, verifiers] : tainter.taints) {
//    if (auto id = ValueId(dl, use); id != 0u) {
//      merge_candidates[id].push_back(use);
//    }
//  }
//
//  std::unordered_map<uint32_t, std::vector<std::pair<llvm::Use *, llvm::Use *>>> non_interfering_uses;
//
//  // Go through and find all intersections of expressions, organized by size of
//  // the intersections.
//  for (auto &[id, uses] : merge_candidates) {
//
//    for (auto a : uses) {
//      const auto &group_a = tainter.taints[a];
//      for (auto b : uses) {
//        if (a >= b) {
//          continue;
//        }
//
//        const auto &group_b = tainter.taints[b];
//
//        // If the intersection is nonempty then these two expressions interfere
//        // and cannot be merged.
//        if (UsesInterfere(group_a, group_b)) {
//          continue;
//        }
//
//        non_interfering_uses[id].emplace_back(a, b);
//      }
//    }
//  }
//
////  std::vector<std::unique_ptr<Context>> contexts;
//  for (auto &[id, use_pairs] : non_interfering_uses) {
//    if (use_pairs.empty()) {
//      continue;
//    }
//
//    std::nth_element(
//        use_pairs.begin(), use_pairs.begin(), use_pairs.end(),
//        [&] (std::pair<llvm::Use *, llvm::Use *> a,
//            std::pair<llvm::Use *, llvm::Use *> b) {
//          auto a_size = tainter.taints[a.first].size() +
//                        tainter.taints[a.second].size();
//          auto b_size = tainter.taints[b.first].size() +
//                        tainter.taints[b.second].size();
//          if (a_size != b_size) {
//            return a_size > b_size;
//          }
//
//          // Everything below is basically arbitrary.
//
//          auto a_id = ValueId(dl, a.first);
//          auto b_id = ValueId(dl, b.first);
//          if (a_id != b_id) {
//            return a_id > b_id;
//          }
//
//          return std::min(a.first, a.second) > std::min(b.first, b.second);
//        });
//
//
//    auto [a, b] = use_pairs.front();
//
//    a->get()->getType();
//
//    std::cout
//        << "Merge:\n\t" << remill::LLVMThingToString(a->get())
//        << "\n\t" << remill::LLVMThingToString(b->get())
//        << "\n\n";
//  }
//

//  std::vector<std::pair<llvm::Use *, std::set<llvm::CallInst *>>> use_to_non_users;
//
//  std::unordered_map<uint32_t, std::set<std::set<llvm::CallInst *> *>> grouped_taints;
//  for (auto &[val, taints] : tainter.taints) {
//    grouped_taints[SummarizeValue(*module, val)].insert(&taints);
//  }
//

//  for (auto i = 0u, max_i = std::thread::hardware_concurrency();
//       i < max_i; ++i) {
//    contexts.emplace_back(new Context(circuit1_func));
//  }

}  // namespace circuitous
