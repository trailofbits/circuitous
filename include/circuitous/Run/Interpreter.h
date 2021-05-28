/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Run/Base.hpp>
#include <circuitous/Run/Spawn.hpp>

namespace circuitous::run {

  struct BasicInterpreter : DBase<BasicInterpreter> {
    using parent_t = DBase<BasicInterpreter>;

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

  template<typename Spawn>
  struct QueueInterpreter {
    using value_type = typename Spawn::value_type;

    Circuit *circuit;

    CtxCollector collector;
    State init_state{ collector };
    std::unordered_map<VerifyInstruction *, Spawn> runners;

    Spawn *acceptor = nullptr;

    QueueInterpreter(Circuit *circuit_) : circuit(circuit_) {
      collector.Run(circuit);

      for (auto vi : circuit->Attr<VerifyInstruction>()) {
        runners.insert( std::make_pair( vi, Spawn(circuit, vi, init_state) ) );
      }

      init();
    }

    void SetNodeVal(Operation *op, const value_type &val) {
      //this->parent_t::SetNodeVal(op, val);
      init_state.SetNodeVal(op);
      for (auto &[_, runner] : runners) {
        runner.SetNodeVal(op, val);
      }
    }

    void set_input_state(const trace::Entry &in) {
      for (auto &[_, runner] : runners) {
        runner.set_input_state(in);
      }
    }

    void set_output_state(const trace::Entry &out) {
      for (auto &[_, runner] : runners) {
        runner.set_output_state(out);
      }
    }

    template<typename T, typename ...Ts>
    void init() {
      for (auto c : circuit->Attr<T>()) {
        for (auto &[_, runner] : runners) {
          runner.Visit(c);
        }
      }

      if constexpr (sizeof...(Ts) != 0) {
        init<Ts...>();
      }
    }

    auto values() const {
      CHECK(acceptor);
      return acceptor->values();
    }

    void init() { init<Undefined, Constant>(); }

    bool Run() {
      std::unordered_set<Spawn *> successes;
      for (auto &[_, runner] : runners) {
        if (runner.Run()) {
          successes.emplace(&runner);
        }
      }
      if (successes.size() == 1) {
        acceptor = *successes.begin();
      }
      if (successes.size() > 1) {
        LOG(FATAL) << "Multiple contexts satisfied." << successes.size();
      }
      return successes.size() == 1;
    }
  };

  using DQueueInterpreter = QueueInterpreter<DSpawn>;
  using VQueueInterpreter =  QueueInterpreter<VSpawn>;

}  // namespace circuitous::run