/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/MemoryBuffer.h>
#include <remill/Arch/Arch.h>
#include <remill/BC/Compat/Error.h>
#include <remill/BC/IntrinsicTable.h>
#include <remill/BC/Lifter.h>
#include <remill/BC/Optimizer.h>
#include <remill/BC/Util.h>

#include <bitset>
#include <vector>

namespace circuitous {

enum : size_t { kMaxNumInstBits = 15 * 8 };

using InstructionEncoding = std::bitset<kMaxNumInstBits>;

// Groups together instructions that share a common semantic, and information
// that is common across all of the encodings of this instruction.
struct InstructionSelection {

  // Decoded instructs that all share a common ISEL, i.e. instruction semantic.
  std::vector<remill::Instruction> instructions;

  // Bitvector representations of the encodings of the instructions from
  // `instructions`.
  std::vector<InstructionEncoding> encodings;

  // Each `1` bit in this bitset represents a bit that is always zero or always
  // one at the same position across all of the encodings in `encodings`.
  InstructionEncoding known_bits;
};

struct EncodedInstructionPart {

  // A bit offset in an `InstructionEncoding`.
  unsigned offset{0};

  // The number of bits spanned by this part in an `InstructionEncoding`.
  unsigned num_bits{0};

  // Which instruction semantics know the values of these bits. Maps the
  // function name to the value of the bits.
  std::map<std::string, uint64_t> known_by;

  // Which instruction semantics don't know the values of these bits.
  std::set<std::string> unknown_by;
};

// Represents the components of an instruction decoding.
using EncodedInstructionParts = std::vector<EncodedInstructionPart>;

enum : unsigned { kMaxNumBytesRead = 16u };

class CircuitBuilder {
 public:
  template <typename T>
  explicit CircuitBuilder(T initialize_arch)
      : arch(initialize_arch(context)),
        module(remill::LoadArchSemantics(arch)),
        state_ptr_type(remill::StatePointerType(module.get())),
        mem_ptr_type(remill::MemoryPointerType(module.get())),
        i32_type(llvm::Type::getInt32Ty(context)),
        bool_type(llvm::Type::getInt1Ty(context)),
        true_value(llvm::ConstantInt::get(bool_type, 1)),
        false_value(llvm::ConstantInt::get(bool_type, 0)),
        xor_all_func(
            llvm::Function::Create(llvm::FunctionType::get(bool_type, true),
                                   llvm::GlobalValue::ExternalLinkage,
                                   "__circuitous_xor_all", module.get())),
        verify_inst_func(llvm::Function::Create(
            llvm::FunctionType::get(bool_type, {bool_type}, true),
            llvm::GlobalValue::ExternalLinkage, "__circuitous_verify_inst",
            module.get())),
        verify_decode_func(llvm::Function::Create(
            llvm::FunctionType::get(bool_type, {bool_type}, true),
            llvm::GlobalValue::ExternalLinkage, "__circuitous_verify_decode",
            module.get())) {

    (void) i32_type;

    // Mark these functions as not touching memory; this will help LLVM to
    // better optimize code that calls these functions.
    xor_all_func->addFnAttr(llvm::Attribute::ReadNone);
    verify_inst_func->addFnAttr(llvm::Attribute::ReadNone);
    verify_decode_func->addFnAttr(llvm::Attribute::ReadNone);

    // The remaining parameters will be input/output registers for verification.
    arch->ForEachRegister([&](const remill::Register *reg_) {
      if (auto reg = reg_->EnclosingRegister(); reg == reg_) {
        regs.push_back(reg);
      }
    });
  }

  llvm::Function *Build(llvm::StringRef buff);

 protected:
  // Return a function that does a bitwise comparison of two values of
  // type `type`.
  llvm::Function *BitMatcherFunc(llvm::Type *type);

  // Return a function that encodes input bit argument values into an integer
  // type.
  llvm::Function *BitConcatFunc(llvm::Type *type_);

  // Return a function that selects from one or more values.
  llvm::Function *SelectorFunc(llvm::Type *selector_type, llvm::Type *type_);

  // Decode all instructions in `buff` using `arch`. Group the instructions in
  // terms of a general semantic category/class.
  std::vector<InstructionSelection> DecodeInstructions(llvm::StringRef buff);

  // Breaks apart the instruction encoding into runs of always-known or maybe-
  // known bits.
  EncodedInstructionParts
  CreateEncodingTable(const std::vector<InstructionSelection> &isels);

  // Decode all instructions in `buff` using `arch`.
  void LiftInstructions(std::vector<InstructionSelection> &isels);

  // Apply a callback `cb(llvm::CallInst *)`
  // to each call of `__circuitous_verify_inst` in `circuit_func`.
  template <typename T>
  void ForEachVerification(llvm::Function *circuit_func, T cb);

  // Build the first level circuit. We will analyze this function later to
  // get an accurate picture of instruction dependencies.
  llvm::Function *BuildCircuit0(std::vector<InstructionSelection> isels);

  // Build the second level circuit. Here we analyze how the instruction checkers
  // use registers and try to eliminate unneeded registers from the function's
  // argument list.
  llvm::Function *BuildCircuit1(llvm::Function *circuit0_func);

  // Build the fourth level circuit. Here we merge together multiple
  // verification calls so that we can merge decoding effort.
  llvm::Function *BuildCircuit2(llvm::Function *circuit1_func);

  // Build the third level circuit. Here we analyze how registers are used
  // across instructions and we try to break dependencies and merge common
  // sub-expressions.
  llvm::Function *BuildCircuit3(llvm::Function *circuit2_func);

 public:
  llvm::LLVMContext context;

  // The architecture associated with
  const remill::Arch::ArchPtr arch;

  // Module into which we'll do most lifting. It will contain `arch`-specific
  // semantics.
  const std::unique_ptr<llvm::Module> module;

 private:
  llvm::CallInst *FinalXor(llvm::Function *in_func) const;

  // Update any references we might have held to functions that could be
  // optimized away.
  void Refresh(void);

  // The LLVM type of `State *` in Remill semantics.
  llvm::PointerType *const state_ptr_type;

  // The LLVM type of the opawue `Memory *` in Remill semantics.
  llvm::PointerType *const mem_ptr_type;

  // The maximum number of bits needed to hold any instruction that can be
  // verified by this ciruit.
  unsigned encoded_inst_size{0};

  // The number of distinct "parts" to instructions.
  unsigned num_instruction_parts{0};

  llvm::Type *const i32_type;
  llvm::Type *const bool_type;
  llvm::Value *const true_value;
  llvm::Value *const false_value;

  // This function is used as a final step in the circuit. Every instruction
  // goes through two general phases: state transition verification, and
  // encoding verification. Thus we should have one one of two situations:
  //
  //    1)  No instruction verifies both steps, and so all arguments are `0`,
  //        and thus teh result of XORing all the zero bits will be `0`, i.e.
  //        we failed to verify the stepping.
  //    2)  Only a single instruction verifies both steps, and all others
  //        fail at either the state check step, or the encoding verification
  //        step, and thus the result of XORing all the bits will be `1`, i.e.
  //        that we successfully verified.
  llvm::Function *const xor_all_func;

  // Verify that an instruction completed a successful state transfer. This
  // has the equivalent meaning of an "all ones" function, except that the
  // first parameter is whether or not we verified the decoding for this
  // instruction's selection.
  llvm::Function *const verify_inst_func;

  // Verify that we decoded an instruction category/semantic. This
  // has the equivalent meaning of an "all ones" function
  llvm::Function *const verify_decode_func;

  // Encodes an instruction by concatenating zero bits with variable bits
  // produced by verified the register state transfer of a given instruction.
  std::vector<llvm::Function *> concat_funcs;

  // Functions representing equality comparisons of integer values of specific
  // bit widths.
  //
  // Technically you can use `llvm::CmpInst` with `ICMP_EQ`, but LLVM's
  // will sometimes do annoying XOR tricks, especially on single bit compares,
  // so it's nice to get the predictability of a single opaque function with a
  // well-defined semantic.
  std::vector<llvm::Function *> bit_match_funcs;

  // Functions that select a value.
  std::vector<llvm::Function *> select_funcs;

  // Top-level registers.
  std::vector<const remill::Register *> regs;
};

}  // namespace circuitous
