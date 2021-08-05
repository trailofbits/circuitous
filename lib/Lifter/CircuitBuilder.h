/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Lifter/BaseLifter.hpp>
#include <circuitous/Lifter/Component.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/IntrinsicTable.h>
#include <remill/BC/Lifter.h>
#include <remill/BC/Optimizer.h>
#include <remill/BC/Util.h>

#include <iomanip>
#include <unordered_map>
#include <vector>

namespace circ {

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


struct CircuitBuilder {
  using InstSelections = std::vector<InstructionSelection>;
  static constexpr const char *bytes_fragments_count_kind =
    "__circuitous.byte_fragment_count";

  explicit CircuitBuilder(CtxRef ctx_) : ctx(ctx_) {}

  llvm::Function *Build(llvm::StringRef buff);

  CtxRef ctx;
};

llvm::Function *make_circuit_fn(CtxRef ctx, const std::string &name);

struct CircuitFunction : has_ctx_ref {
  llvm::Function *circuit_fn = nullptr;

  llvm::BasicBlock *entry = nullptr;
  llvm::BasicBlock *start = nullptr;
  llvm::BasicBlock *head = nullptr;
  llvm::BasicBlock *exit = nullptr;

  using cr_reg = const remill::Register *;
  using arg_map_t = std::vector< std::tuple< cr_reg, llvm::Argument *, llvm::Argument * > >;

  arg_map_t arg_map;

  CircuitFunction(CtxRef ctx_, const std::string &name) : has_ctx_ref(ctx_) {
    circuit_fn = _make_fn(name);
    _make_body();
  }

  llvm::Function *_make_fn(const std::string &name);

  void _make_body() {
    LOG(INFO) << "CircuitFunction::_make_body";
    CHECK(circuit_fn && circuit_fn->isDeclaration());

    entry = llvm::BasicBlock::Create(*ctx.llvm_ctx(), "entry", circuit_fn);
    head = llvm::BasicBlock::Create(*ctx.llvm_ctx(), "", circuit_fn);
    start = head;
    exit = llvm::BasicBlock::Create(*ctx.llvm_ctx(), "exit", circuit_fn);
    LOG(INFO) << "\tBasic blocks created.";
  }

  void move_head() {
    auto next = llvm::BasicBlock::Create(*ctx.llvm_ctx(), "", circuit_fn);
    llvm::IRBuilder<>(head).CreateBr(next);
    head = next;
  }

  void tie_head() { llvm::IRBuilder<>(head).CreateBr(exit); }
  void tie_entry() { llvm::IRBuilder<>(entry).CreateBr(start); }
  void tie_exit(llvm::Value *v) {
    llvm::IRBuilder<>(exit).CreateRet(v);
  }

  auto pc() {
    for (auto &[reg, arg, _] : arg_map)
      if (reg->name == ctx.arch()->ProgramCounterRegisterName())
        return arg;
    LOG(FATAL) << "Could not locate input pc register.";
  }

  void inspect_corpse() {
    circuit_fn->print(llvm::errs());
    llvm::errs().flush();
    LOG(FATAL) << "Corpse inspection";
  }

  using maybe_str = std::optional< std::string >;
  static maybe_str is_output_reg(llvm::Argument *arg);
  static maybe_str is_input_reg(llvm::Argument *arg);
};

struct decoder : has_ctx_ref {
  using has_ctx_ref::has_ctx_ref;
  using values_t = std::vector< llvm::Value * >;

  llvm::IRBuilder<> &ir;
  ISEL_view isel;

  decoder(CtxRef ctx_, llvm::IRBuilder<> &ir_, ISEL_view &isel_)
      : has_ctx_ref(ctx_), ir(ir_), isel(isel_)
  {}

  values_t byte_fragments();
  std::string generate_raw_bytes(const std::string &str, uint64_t form, uint64_t to);
  llvm::Value *create_bit_check(uint64_t from, uint64_t to);


  auto rinst_size() { return isel.instruction.bytes.size() * 8; }

  std::string convert_encoding(const auto &encoding) {
    std::string full_inst;
    // Encoding check needed since `x` is unsigned.
    for (auto x = rinst_size() - 1; x >= 0 && x < encoding.size(); --x) {
      full_inst += (encoding[x]) ? '1' : '0';
    }
    std::reverse(full_inst.begin(), full_inst.end());
    return full_inst;
  }
};

struct circuit_builder : CircuitFunction {
  using parent_t = CircuitFunction;

  using isels_t  = std::vector< InstructionSelection >;
  using values_t = std::vector< llvm::Value * >;
  using instructions_t = std::vector< llvm::Instruction * >;

  using parent_t::parent_t;

  llvm::Value *saturation_prop;
  llvm::Value *timestamp_prop;

  using context_t = component::Context;
  std::vector< context_t > ctxs;

  std::unordered_map< std::string, llvm::Value * > default_rcs;

  circuit_builder(CtxRef ctx_, const std::string &name) : CircuitFunction(ctx_, name)
  {
    build_general_props();
    build_default_rcs();
  }

  void build_general_props() {
    std::tie(saturation_prop, timestamp_prop) =
        component::construct< component::SaturationProp, component::TimestampProp >(this->entry);
  }

  void build_default_rcs() {
    llvm::IRBuilder<> irb(this->entry);
    for (auto &[reg, in, out] : this->arg_map) {
      default_rcs[reg->name] = irops::make< irops::OutputCheck >(irb, {in, out});
    }
  }

  void inject_isels(const isels_t& isels) {
    for (const auto &isel : isels) {
      inject_isel(isel);
    }
  }

  void inject_isel(const InstructionSelection &isel) {
    for (auto i = 0u; i < isel.instructions.size(); ++i) {
      inject_semantic(ISEL_view(isel, i));
      this->move_head();
    }
  }

  void inject_semantic(ISEL_view isel);
  // TODO(lukas): Fix and replace the old way of lifting.
  void inject_semantic_modular(ISEL_view isel);
  llvm::Function *finish();

  std::tuple< values_t, llvm::Value * > handle_errors(llvm::Value *begin, llvm::Value *end);
  llvm::Value *emit_error_transitions(llvm::Value *current_ebit);
  instructions_t lower_dst_regs(const values_t & dtst);

  values_t handle_dst_regs(llvm::Value *c_ebit, instructions_t &dst_regs, ISEL_view isel, State &state);
  llvm::Value *handle_dst_regs_(
      std::vector< llvm::Instruction * > &dst_regs, ISEL_view isel, State &state);

  llvm::Value *emit_preserved_checks(instructions_t &dst_regs, ISEL_view &isel, State &state);

  void add_isel_metadata(llvm::Instruction *call, ISEL_view isel) {
    std::stringstream ss;
    for (auto byte : isel.instruction.bytes) {
      ss << " " << std::setw(2) << std::setfill('0') << std::hex
        << static_cast<unsigned>(static_cast<uint8_t>(byte));
    }
    annotate_llvm(call, circir_llvm_meta::lifted_bytes, ss.str());
  }


  using vp = llvm::Value *;
  llvm::CallInst *call_semantic(llvm::IRBuilder<> &ir, llvm::Function *fn, vp s, vp pc, vp m)
  {
    llvm::Value *inst_func_args[remill::kNumBlockArgs] = {};
    inst_func_args[remill::kPCArgNum] = pc;
    inst_func_args[remill::kMemoryPointerArgNum] = m;
    inst_func_args[remill::kStatePointerArgNum] = s;
    return ir.CreateCall(fn, inst_func_args);
  }
};

}  // namespace circ
