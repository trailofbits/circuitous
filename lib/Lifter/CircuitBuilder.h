/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>
#include <circuitous/Lifter/Shadows.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/IntrinsicTable.h>
#include <remill/BC/Lifter.h>
#include <remill/BC/Optimizer.h>
#include <remill/BC/Util.h>

#include <unordered_map>
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

  // We try to guess which bytes in the encoding of an instruction are actually
  // immediates.
  using imm_meta_t = std::map<uint64_t, uint64_t>;
  using imm_meta_list_t = std::map<remill::Operand *, imm_meta_t>;
  std::vector<imm_meta_list_t> imms;
  std::vector<shadowinst::Instruction> shadows;

  std::vector<llvm::Function *> lifted_fns;
};

struct ISEL_view {
  const remill::Instruction &instruction;
  const InstructionEncoding &encoding;
  const InstructionSelection::imm_meta_list_t &imms;
  const shadowinst::Instruction &shadow;
  llvm::Function *lifted;

  ISEL_view(const InstructionSelection &isel, uint64_t i)
    : instruction(isel.instructions[i]), encoding(isel.encodings[i]),
      imms(isel.imms[i]), shadow(isel.shadows[i]), lifted(isel.lifted_fns[i])
  {}
};

enum : unsigned { kMaxNumBytesRead = 16u };

class CircuitBuilder {
 public:
  using InstSelections = std::vector<InstructionSelection>;
  static constexpr const char *bytes_fragments_count_kind =
    "__circuitous.byte_fragment_count";

  struct Circuit0 {
    static constexpr const char *name = "circuit_0";

    Circuit0(CircuitBuilder &parent_)
      : parent(parent_)
    {}

    llvm::FunctionType *FnT();
    llvm::Function *GetFn();
    llvm::Function *Create();

    // Helper functions to decouple a lot of stuff that was originally in
    // one huge function.
    void InjectISELs(std::vector<InstructionSelection> isels);
    llvm::BasicBlock *InjectISEL(const InstructionSelection &isel,
                    llvm::BasicBlock *prev,
                    llvm::BasicBlock *exit);

    // Inject ISEL into block `into`. Into `exit` a verify call is emitted.
    void InjectSemantic(llvm::BasicBlock *into, llvm::BasicBlock *exit, ISEL_view isel);

    void CallSemantic(llvm::IRBuilder<> &ir, llvm::Function *fn,
                      llvm::Value *state, llvm::Value *pc, llvm::Value *memory) {
      llvm::Value *inst_func_args[remill::kNumBlockArgs] = {};
      inst_func_args[remill::kPCArgNum] = pc;
      inst_func_args[remill::kMemoryPointerArgNum] = memory;
      inst_func_args[remill::kStatePointerArgNum] = state;
      ir.CreateCall(fn, inst_func_args);
    }

    // For given ISEL return back list of byte checks that should be included in
    // the final verify.
    std::vector<llvm::Value *> ByteFragments(llvm::IRBuilder<> &ir, ISEL_view isel);

    using remill_reg = const remill::Register *;
    // register - input - output
    using Arg = std::tuple<remill_reg, llvm::Argument *, llvm::Argument *>;
    // TODO(lukas): For compatibility with history, this must be
    //              ordered hence vector and not map.
    std::vector<Arg> reg_to_args;
    std::vector<llvm::Argument *> inst_bytes;
    llvm::Value *pc = nullptr;

    // Vector of return values, one for each result of doing a
    // `__circuitous_verify_decode`.
    std::vector<llvm::Value *> verified_insts;

    llvm::Function *circuit_fn = nullptr;

    // TODO(lukas): Eventually transform into lift context after all lift methods
    //              are refactored out.
    CircuitBuilder &parent;
  };

  template <typename T>
  explicit CircuitBuilder(T initialize_arch)
      : arch(initialize_arch(context)),
        module(remill::LoadArchSemantics(arch)),
        state_ptr_type(arch->StatePointerType()),
        mem_ptr_type(arch->MemoryPointerType()),
        i32_type(llvm::Type::getInt32Ty(context)),
        bool_type(llvm::Type::getInt1Ty(context)),
        true_value(llvm::ConstantInt::get(bool_type, 1)),
        false_value(llvm::ConstantInt::get(bool_type, 0)) {

    (void) i32_type;

    // The remaining parameters will be input/output registers for verification.
    arch->ForEachRegister([&](const remill::Register *reg_) {
      if (auto reg = reg_->EnclosingRegister(); reg == reg_) {
        regs.push_back(reg);
      }
    });
  }

  llvm::Function *Build(llvm::StringRef buff);

 protected:

  void IdentifyImms(InstSelections &insts);
  template<typename T> T inst_fragments_count(llvm::CallInst *call) const;

  // Decode all instructions in `buff` using `arch`. Group the instructions in
  // terms of a general semantic category/class.
  std::vector<InstructionSelection> DecodeInstructions(llvm::StringRef buff);

  // Breaks apart the instruction encoding into runs of always-known or maybe-
  // known bits.
  // EncodedInstructionParts
  // CreateEncodingTable(const std::vector<InstructionSelection> &isels);

  // Flatten all control flow into pure data-flow inside of a function.
  void FlattenControlFlow(llvm::Function *func,
                          const remill::IntrinsicTable &intrinsics);

  // Decode all instructions in `buff` using `arch`.
  void LiftInstructions(std::vector<InstructionSelection> &isels);

  // Apply a callback `cb(llvm::CallInst *)`
  // to each call of `__circuitous.verify_inst` in `circuit_func`.
  template <typename T>
  void ForEachVerification(llvm::Function *circuit_func, T cb);

  // Build the first level circuit. We will analyze this function later to
  // get an accurate picture of instruction dependencies.
  Circuit0 BuildCircuit0(std::vector<InstructionSelection> isels);

  // Build the second level circuit. Here we analyze how the instruction checkers
  // use registers and try to eliminate unneeded registers from the function's
  // argument list.
  llvm::Function *BuildCircuit1(Circuit0 circuit0);
 public:
  llvm::LLVMContext context;

  // The architecture associated with
  const remill::Arch::ArchPtr arch;

  // Module into which we'll do most lifting. It will contain `arch`-specific
  // semantics.
  const std::unique_ptr<llvm::Module> module;

  // TODO(lukas): Currently turned off, later we want to hide this behind some
  //              cmd flag. I do not want to spaghet it in at this point.
  bool reduce_imms = false;

 private:
  // llvm::CallInst *FinalXor(llvm::Function *in_func) const;

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
  // unsigned num_instruction_parts{0};

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
  //
  //        NOTE(surovic): This assumption does not hold for certain combinations
  //        of instruction encodings, input register states and output register
  //        states. For example, subtraction and addition of 0 will both transfer
  //        any input register state to and equivalent output register state.
  //        In this case more than one instruction will be verified and thus
  //        XORing all inputs will produce `0` or `1` depending on the number of
  //        successful verifications.
  llvm::Function *one_of_func{nullptr};

  // Verify that an instruction completed a successful state transfer. This
  // has the equivalent meaning of an "all ones" function, except that the
  // first parameter is whether or not we verified the decoding for this
  // instruction's selection.
  llvm::Function *verify_inst_func{nullptr};

  // Top-level registers.
  std::vector<const remill::Register *> regs;
};

}  // namespace circuitous
