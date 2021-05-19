/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

// NOTE(lukas): `#pragma once` is path related and since this is being
//              included "directly" in `Run.cpp` it causes a lot of troubles.
#ifndef circuitous_Interpreter_file
#define circuitous_Interpreter_file

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Verify.hpp>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <gflags/gflags.h>
#include <glog/logging.h>

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/ADT/APInt.h>
#pragma clang diagnostic pop

#include <deque>
#include <optional>
#include <unordered_map>

namespace circuitous {

  template<typename Self>
  struct Interpreter : public Visitor<Self> {
    using raw_value_type = llvm::APInt;
    // If no value is held <=> value is undefined
    using value_type = std::optional<raw_value_type>;
    using parent_t = Visitor<Interpreter>;

    Circuit *circuit{nullptr};
    bool changed{false};

    std::unordered_map<Operation *, value_type> node_values;

    value_type Undef() { return {}; }
    llvm::APInt TrueVal() { return llvm::APInt(1, 1); }
    llvm::APInt FalseVal() { return llvm::APInt(1, 0); }

    template<typename ...Args>
    bool ValidVals(Args &&... args) {
      return (GetNodeVal(args).has_value() && ...);
    }

    bool ValidChildren(Operation *op) {
      for (std::size_t i = 0; i < op->operands.size(); ++i) {
        if (!GetNodeVal(op->operands[i])) {
          return false;
        }
      }
      return true;
    }

    std::unordered_map<Operation *, llvm::APInt> values() const {
      std::unordered_map<Operation *, llvm::APInt> out;
      for (auto &[op, val] : node_values) {
        if (val) {
          out[op] = *val;
        }
      }
      return out;
    }

    Operation *GetReg(const std::string &name) {
      for (auto reg : circuit->Attr<InputRegister>()) {
        if (reg->reg_name == name) {
          return reg;
        }
      }
      return nullptr;
    }

    auto &self() { return static_cast<Self &>(*this); }

    Interpreter(Circuit *circuit_) : circuit(circuit_) {}

    void SetNodeVal(Operation *op, const raw_value_type &val);
    void SetNodeVal(Operation *op, const value_type &val);
    value_type GetNodeVal(Operation *op);

    void SetInstructionBitsValue(const std::string &bits);
    void SetInputRegisterValue(const std::string &name, uint64_t bits);
    void SetInputEbit(bool value);

    std::optional<uint64_t> GetOutputRegisterValue(const std::string &name);
    std::optional<bool> GetOutputErrorFlagValue();

    // Default
    void VisitOperation(Operation *op);

    // Operands
    void VisitConstant(Constant *op);
    void VisitInputRegister(InputRegister *op);
    void VisitInputImmediate(InputImmediate *op);
    void VisitOutputRegister(OutputRegister *op);
    void VisitInputErrorFlag(InputErrorFlag *op);
    void VisitOutputErrorFlag(OutputErrorFlag *op);
    void VisitInputInstructionBits(InputInstructionBits *op);
    void VisitHint(Hint *op);
    void VisitUndefined(Undefined *op);

    // Operations
    void VisitConcat(Concat *op);
    void VisitExtract(Extract *op);
    void VisitNot(Not *op);
    void VisitLLVMOperation(LLVMOperation *op);
    void VisitSelect(Select *op);
    void VisitParity(Parity *op);
    void VisitPopulationCount(PopulationCount *op);

    // Conditions
    void VisitDecodeCondition(DecodeCondition *op);
    void VisitRegisterCondition(RegisterCondition *op);
    void VisitPreservedCondition(PreservedCondition *op);
    void VisitCopyCondition(CopyCondition *op);
    void VisitHintCondition(HintCondition *op);
    void VisitVerifyInstruction(VerifyInstruction *op);
    void VisitOnlyOneCondition(OnlyOneCondition *op);

    // Circuit
    void VisitCircuit(Circuit *op);

  };

  #include "Interpreter.tpp"

  struct BasicInterpreter : Interpreter<BasicInterpreter> {
    using parent_t = Interpreter<BasicInterpreter>;

    using parent_t::parent_t;

    void Visit(Operation *op) {
      op->Traverse(*this);
      if (node_values.count(op)) {
        // Remember previous node value
        auto prev_val{GetNodeVal(op)};
        // Compute new node value
        parent_t::Visit(op);
        // Was there a change?
        changed |= prev_val != GetNodeVal(op);
      } else {
        // We have no value. Just do it!
        parent_t::Visit(op);
        changed = true;
      }
    }

    bool Run() {
      // Save initial node values
      auto node_values_init{node_values};
      // Run verification nodes until one succeeds
      for (auto op : circuit->Attr<VerifyInstruction>()) {
        // Re-initialize node values
        node_values = node_values_init;
        // Reset
        changed = true;
        // Evaluate verification node until fixpoint
        while (changed) {
          changed = false;
          Visit(op);
        }
        // Verification successful
        CHECK(GetNodeVal(op));
        if (GetNodeVal(op)->getBoolValue()) {
          return true;
        }
      }
      // All verifications failed
      return false;
    }
  };

  struct QueueInterpreter : Interpreter<QueueInterpreter> {

    struct State {
      std::deque<Operation *> todo;
      std::unordered_map<Operation *, uint64_t> blocked;

      CtxCollector &collector;

      State(CtxCollector &collector_) : collector(collector_) {}
      State(const State &) = default;
      State(State &&) = delete;

      auto Pop() {
        auto x = todo.front();
        todo.pop_front();
        return x;
      }

      void Push(Operation *op) {
        todo.push_back(op);
      }

      void Notify(Operation *op) {
        auto [it, inserted] = blocked.emplace(op, op->operands.size());
        if (it->second != 0) {
          --it->second;
        }
        if (it->second == 0) {
          Push(it->first);
        }
      }

      void SetNodeVal(Operation *op) {
        for (auto user : op->users) {
          Notify(user);
        }
      }
    };

    struct Spawn : Interpreter<Spawn> {
      using parent_t = Interpreter<Spawn>;

      VerifyInstruction *current;
      State state;

      Spawn(Circuit *circuit_, VerifyInstruction *current_,
            const State &state_, const decltype(node_values)& nvs_ )
          : parent_t(circuit_), current(current_), state(state_)
      {
        node_values = nvs_;
        InitChecks();
      }

      Spawn(const Spawn &) = default;
      Spawn(Spawn &&) = delete;

      void SetNodeVal(Operation *op, const value_type &val) {
        if (node_values.count(op)) {
          CHECK(node_values[op] == val);
          return;
        }
        this->parent_t::SetNodeVal(op, val);
        state.SetNodeVal(op);
      }

      void SetNodeVal(Operation *op, const raw_value_type &val) {
        if (node_values.count(op)) {
          CHECK(*node_values[op] == val);
          return;
        }
        this->parent_t::SetNodeVal(op, val);
        state.SetNodeVal(op);
      }


      void Visit(Operation *op) {
        if (!state.collector.op_to_ctxs[op].count(current)) {
          return;
        }
        parent_t::Visit(op);
      }

      void InitChecks() {

        for (auto hc : circuit->Attr<HintCondition>()) {
          // TODO(lukas): I am not sure if the following scenario propagates properly
          //              `HintCondition(H1, A)`
          //              `HintCondition(H1, H2)`
          //               I am not sure that after visting the first condition the value
          //               of H1 will propagate to H2.
          if (state.collector.op_to_ctxs[hc].count(current)) {
            state.blocked[hc] = 1;
          }
        }

        for (auto oreg : circuit->Attr<OutputRegister>()) {
          if (state.collector.op_to_ctxs[oreg].count(current)) {
            for (auto user : oreg->users) {
              state.Notify(user);
            }
          }
        }
        for (auto oreg : circuit->Attr<OutputErrorFlag>()) {
          if (state.collector.op_to_ctxs[oreg].count(current)) {
            for (auto user : oreg->users) {
              if (user->op_code == Operation::kRegisterCondition) {
                state.Notify(user);
              }
            }
          }
        }
      }

      void VisitRegisterCondition(RegisterCondition *op) {
        DLOG(INFO) << "VisitRegisterCondition: " << op->Name() << " " << op->id();
        auto val{op->operands[0]};
        auto reg{op->operands[1]};
        self().SetNodeVal(reg, self().GetNodeVal(val));
        self().SetNodeVal(op, TrueVal());
      }

      void VisitPreservedCondition(PreservedCondition *op) {
        DLOG(INFO) << "VisitPreservedCondition: " << op->Name() << " " << op->id();
        auto ireg{op->operands[0]};
        auto oreg{op->operands[1]};
        self().SetNodeVal(oreg, self().GetNodeVal(ireg));
        self().SetNodeVal(op, TrueVal());
      }

      void VisitCopyCondition(CopyCondition *op) {
        DLOG(INFO) << "VisitPreservedCondition: " << op->Name() << " " << op->id();
        auto ireg{op->operands[0]};
        auto oreg{op->operands[1]};
        self().SetNodeVal(oreg, self().GetNodeVal(ireg));
        self().SetNodeVal(op, TrueVal());
      }

      void VisitHintCondition(HintCondition *op) {
        DLOG(INFO) << "VisitHintCondition: " << op->Name() << " " << op->id();
        auto real{op->operands[0]};
        auto hint{op->operands[1]};
        CHECK(real->op_code != Operation::kHint);
        CHECK(hint->op_code == Operation::kHint);
        self().SetNodeVal(hint, self().GetNodeVal(real));
        self().SetNodeVal(op, TrueVal());
      }

      bool Run() {
        while (!state.todo.empty()) {
          auto x = state.Pop();
          Visit(x);
        }
        return GetNodeVal(current) == TrueVal();
      }
    };

    using parent_t = Interpreter<QueueInterpreter>;

    CtxCollector collector;
    State init_state{ collector };
    std::unordered_map<VerifyInstruction *, Spawn> runners;

    QueueInterpreter(Circuit *circuit_) : parent_t(circuit_) {
      collector.Run(circuit);

      for (auto vi : circuit->Attr<VerifyInstruction>()) {
        runners.insert( std::make_pair( vi, Spawn(circuit, vi, init_state, node_values) ) );
      }

      Init();
    }

    void SetNodeVal(Operation *op, const value_type &val) {
      this->parent_t::SetNodeVal(op, val);
      init_state.SetNodeVal(op);
    }

    void SetNodeVal(Operation *op, const raw_value_type &val) {
      this->parent_t::SetNodeVal(op, val);
      init_state.SetNodeVal(op);
    }

    void Init() {
      for (auto c : circuit->Attr<Constant>()) {
        for (auto &[_, runner] : runners) {
          runner.Visit(c);
        }
      }

      for (auto u : circuit->Attr<Undefined>()) {
        for (auto &[_, runner] : runners) {
          runner.Visit(u);
        }
      }
    }

    void SetInputEbit(bool val) {
      for (auto &[_, runner] : runners) {
        runner.SetInputEbit(val);
      }
    }

    void SetInstructionBitsValue(const std::string &bits) {
      for (auto &[_, runner] : runners) {
        runner.SetInstructionBitsValue(bits);
      }
    }

    void SetInputRegisterValue(const std::string &name,
                               uint64_t bits)
    {
      for (auto &[_, runner] : runners) {
        runner.SetInputRegisterValue(name, bits);
      }
    }

    bool Run() {
      std::unordered_set<Spawn *> successes;
      for (auto &[_, runner] : runners) {
        if (runner.Run()) {
          successes.emplace(&runner);
        }
      }
      if (successes.size() == 1) {
        node_values = std::move((*successes.begin())->node_values);
      } else {
        node_values = std::move(runners.begin()->second.node_values);
      }
      if (successes.size() > 1) {
        DLOG(FATAL) << "Multiple contexts satisfied." << successes.size();
      }
      return successes.size() == 1;
    }

  };

}  // namespace circuitous

#endif //include guard