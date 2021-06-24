/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Lifter/BaseLifter.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/IntrinsicTable.h>
#include <remill/BC/Lifter.h>
#include <remill/BC/Optimizer.h>
#include <remill/BC/Util.h>

#include <unordered_map>
#include <vector>

namespace circ {
  static constexpr const uint32_t kMaxNumBytesRead = 16u;

  struct Surface : Names {
    struct ErrorBit { static constexpr uint8_t size = 1; } ebit;
    struct InstructionBits { static constexpr uint8_t size = kMaxNumInstBits; } inst_bits;
    struct Timestamp { static constexpr uint8_t size = 64; } timestamp;

    using types_t = std::vector<llvm::Type *>;

    CtxRef &ctx;

    using remill_reg = const remill::Register *;
    // register - input - output
    using Arg = std::tuple<remill_reg, llvm::Argument *, llvm::Argument *>;
    // TODO(lukas): For compatibility with history, this must be
    //              ordered hence vector and not map.
    std::vector<Arg> reg_to_args;
    std::vector<llvm::Argument *> inst_bytes;
    llvm::Value *pc = nullptr;
    llvm::Function *circuit_fn = nullptr;

    std::tuple<llvm::Value *, llvm::Value *> ebits;
    std::tuple<llvm::Value *, llvm::Value *> timestamps;
    uint8_t aliens_size = 5;

    Surface(CtxRef &ctx_) : ctx(ctx_) {}

    template<typename T>
    auto type(T &&t) const {
      return llvm::IRBuilder<>(*ctx.llvm_ctx()).getIntNTy(t.size);
    }

    types_t Aliens() const {
      return { type(inst_bits), type(ebit), type(ebit), type(timestamp), type(timestamp) };
    }

    void BindRegArgs(const std::string &base, llvm::Function *fn, uint32_t a, uint32_t b) {
      return BindRegArgs(base, remill::NthArgument(fn, a), remill::NthArgument(fn, b));
    }

    void BindRegArgs(const std::string &base, llvm::Value *in, llvm::Value *out) {
      in->setName(as_in_reg(base));
      out->setName(as_out_reg(base));
    }

    types_t Regs() const;

    types_t FullType() const {
      auto aliens = Aliens();
      auto regs = Regs();
      aliens.insert(aliens.end(), regs.begin(), regs.end());
      return aliens;
    }

    llvm::FunctionType *FnT(const types_t &params_types) {
      llvm::IRBuilder<> ir{ *ctx.llvm_ctx() };
      return llvm::FunctionType::get(ir.getInt1Ty(), params_types, false);
    }

    uint64_t copy_aliens(std::vector<llvm::Type *> &types) {
      for (std::size_t i = 0; i < aliens_size; ++i) {
        types.push_back(remill::NthArgument(circuit_fn, i)->getType());
      }
      return aliens_size;
    }

    uint8_t create_aliens(llvm::Function *fn) {
      remill::NthArgument(circuit_fn, 0)->setName(instruction_bits());
      inst_bytes.push_back(remill::NthArgument(circuit_fn, 0));

      auto ebit_name = [&](auto &mut) { return this->build(this->flag, "ebit", mut); };
      auto t_name = [&](auto &mut) { return this->build(timestamp_type, "timestamp", mut); };

      auto name_pair = [&](std::size_t idx, auto namer) {
        remill::NthArgument(fn, idx)->setName(namer(this->in));
        remill::NthArgument(fn, idx + 1)->setName(namer(this->out));
        return std::make_tuple(remill::NthArgument(circuit_fn, idx),
                               remill::NthArgument(circuit_fn, idx + 1));
      };

      ebits = name_pair(1, ebit_name);
      timestamps = name_pair(3, t_name);

      return aliens_size;
    }

    llvm::Function *Create(const std::string &name) {
      auto linkage = llvm::GlobalValue::ExternalLinkage;
      circuit_fn = llvm::Function::Create(FnT(FullType()), linkage, name, ctx.module());
      circuit_fn->addFnAttr(llvm::Attribute::ReadNone);

      auto alien_size = create_aliens(circuit_fn);

      // The rest of arguments should be the registers in/out pairs
      CHECK((circuit_fn->arg_size() - alien_size) % 2 == 0);
      for (std::size_t i = 0; i < ctx.regs().size(); ++i) {
        uint32_t arg_idx = static_cast<uint32_t>(i * 2) + alien_size;
        BindRegArgs(ctx.regs()[i]->name, circuit_fn, arg_idx, arg_idx + 1);

        reg_to_args.emplace_back(ctx.regs()[i],
                                 remill::NthArgument(circuit_fn, arg_idx),
                                 remill::NthArgument(circuit_fn, arg_idx + 1));
        if (ctx.regs()[i]->name == ctx.arch()->ProgramCounterRegisterName()) {
          pc = remill::NthArgument(circuit_fn, arg_idx);
        }
      }
      return circuit_fn;
    }

    template<typename Str>
    auto fetch(Str &&name) {
      for (auto &arg : circuit_fn->args()) {
        if (arg.getName() == name) {
          return &arg;
        }
      }
      return nullptr;
    }

    auto fetch_ebits() { return ebits; }
    auto fetch_timestamps() { return timestamps; }

    // Checks that once ebit is set it is not lowered
    // `ebit_it -> ebit_out` which can be rewritten
    // `!ebit_in || ebit_out`
    auto saturation_property(llvm::IRBuilder<> &ir) {
      auto [ebit_in, ebit_out] = fetch_ebits();
      return ir.CreateOr(ir.CreateNot(ebit_in), ebit_out);
    }

    // Checks that input.timestamp + 1 == output.timestamp
    auto timestamp_property(llvm::IRBuilder<> &ir) {
      auto [ts_in, ts_out] = fetch_timestamps();
      auto inc =  ir.CreateAdd(ts_in, ir.getInt64(1));
      return intrinsics::make_outcheck(ir, {inc, ts_out});
    }
  };

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

    Circuit0(CtxRef &ctx_) : ctx(ctx_), surface(ctx) {}

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
        llvm::Value *current_ebit,
        std::vector<llvm::Instruction *> &dst_regs,
        ISEL_view isel,
        State &state);

    // Vector of return values, one for each result of doing a
    // `__circuitous_verify_decode`.
    std::vector<llvm::Value *> verified_insts;

    std::map<llvm::Value *, std::vector<llvm::CallInst *>> used_selects;

    llvm::Function *circuit_fn = nullptr;
    CtxRef &ctx;
    Surface surface;
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

  // NOTE(lukas): I think this is 99% redundant and not needed but since
  //              I am not sure I am keeping it this way.
  void FoldOutputChecks();
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

}  // namespace circ
