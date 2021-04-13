/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>

#include <circuitous/Lifter/BaseLifter.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/IntrinsicTable.h>
#include <remill/BC/Lifter.h>
#include <remill/BC/Optimizer.h>
#include <remill/BC/Util.h>

#include <unordered_map>
#include <vector>

namespace circuitous {
  static constexpr const uint32_t kMaxNumBytesRead = 16u;

  class CircuitBuilder;

  struct State {
    using reg_ptr_t = const remill::Register *;

    llvm::Value *state = nullptr;

    State(llvm::BasicBlock *where, llvm::Type *type)
        : state(llvm::IRBuilder<>(where).CreateAlloca(type))
    {}

    auto raw() { return state; }
    auto type() { return state->getType(); }

    void store(llvm::IRBuilder<> &ir, const reg_ptr_t where, llvm::Value *what);
    llvm::Value *load(llvm::IRBuilder<> &ir, const reg_ptr_t where);
  };

// Creates firs circuit, the really naive lift. Can be improved by other passes
struct Circuit0 {
    static constexpr const char *name = "circuit_0";

    Circuit0(CtxRef &ctx_) : ctx(ctx_) {}

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

    llvm::CallInst *CallSemantic(
        llvm::IRBuilder<> &ir, llvm::Function *fn,
        llvm::Value *state, llvm::Value *pc, llvm::Value *memory)
    {
      llvm::Value *inst_func_args[remill::kNumBlockArgs] = {};
      inst_func_args[remill::kPCArgNum] = pc;
      inst_func_args[remill::kMemoryPointerArgNum] = memory;
      inst_func_args[remill::kStatePointerArgNum] = state;
      return ir.CreateCall(fn, inst_func_args);
    }

    // For given ISEL return back list of byte checks that should be included in
    // the final verify.
    std::vector<llvm::Value *> ByteFragments(llvm::IRBuilder<> &ir, ISEL_view isel);

    // Lower `circuitous.dst.reg` into allocas.
    std::vector<llvm::Instruction *> LowerDstRegs(std::vector<llvm::Value *> &dsts);
    std::vector<llvm::Value *> HandleDstRegs(
        llvm::IRBuilder<> &ir,
        std::vector<llvm::Instruction *> &dst_regs,
        ISEL_view isel,
        State &state);

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
    CtxRef &ctx;
  };


class CircuitBuilder {
 public:
  using InstSelections = std::vector<InstructionSelection>;
  static constexpr const char *bytes_fragments_count_kind =
    "__circuitous.byte_fragment_count";

  explicit CircuitBuilder(CtxRef ctx_)
      : ctx(ctx_)
  {}

  llvm::Function *Build(llvm::StringRef buff);

 public:

  // Build the first level circuit. We will analyze this function later to
  // get an accurate picture of instruction dependencies.
  Circuit0 BuildCircuit0(std::vector<InstructionSelection> isels);

  // Build the second level circuit. Here we analyze how the instruction checkers
  // use registers and try to eliminate unneeded registers from the function's
  // argument list.
  llvm::Function *BuildCircuit1(Circuit0 circuit0);
 public:
  CtxRef ctx;

  // TODO(lukas): Currently turned off, later we want to hide this behind some
  //              cmd flag. I do not want to spaghet it in at this point.
  bool reduce_imms = false;

  // NOTE(lukas): I am keeping this block for doc purposes.
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
  //llvm::Function *one_of_func{nullptr};
  // Verify that an instruction completed a successful state transfer. This
  // has the equivalent meaning of an "all ones" function, except that the
  // first parameter is whether or not we verified the decoding for this
  // instruction's selection.
  //llvm::Function *verify_inst_func{nullptr};
};

}  // namespace circuitous
