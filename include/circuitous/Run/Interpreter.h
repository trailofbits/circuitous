/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Run/Base.hpp>
#include <circuitous/Run/Spawn.hpp>

namespace circ::run {

  struct BasicInterpreter : DBase<BasicInterpreter> {
    using parent_t = DBase<BasicInterpreter>;

    using parent_t::parent_t;

    void Dispatch(Operation *op) {
      op->Traverse(*this);
      if (node_values.count(op)) {
        // Remember previous node value
        auto prev_val{GetNodeVal(op)};
        // Compute new node value
        parent_t::Dispatch(op);
        // Was there a change?
        changed |= prev_val != GetNodeVal(op);
      } else {
        // We have no value. Just do it!
        parent_t::Dispatch(op);
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
          Dispatch(op);
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
    using spawn_t = Spawn;

    Circuit *circuit;

    CtxCollector collector;
    std::vector< std::pair< VerifyInstruction *, Spawn > > runners;

    Spawn *acceptor = nullptr;

    QueueInterpreter(Circuit *circuit_) : circuit(circuit_) {
      collector.Run(circuit);

      for (auto vi : circuit->Attr<VerifyInstruction>()) {
        Spawn spawn{ circuit, vi, &collector };
        runners.emplace_back( vi, std::move(spawn) );
      }
    }

    template<typename F>
    void runners_do(F &&f) {
      for (auto &[_, runner] : runners) {
        f(runner);
      }
    }

    void set_input_state(const trace::Entry &in) {
      return runners_do([ & ]( auto &runner ){ runner.set_input_state(in); } );
    }

    void set_output_state(const trace::Entry &out) {
      return runners_do([ & ]( auto &runner ){ runner.set_output_state(out); } );
    }

    void set_memory(uint64_t addr, const std::string &val) {
      return runners_do([ & ]( auto &runner ){ runner.set_memory(addr, val); } );
    }

    template<typename T, typename ...Ts>
    void init() {
      for (auto c : circuit->Attr<T>()) {
        runners_do([ & ]( auto &runner ){ runner.Visit(c); } );
      }

      if constexpr (sizeof...(Ts) != 0) {
        init<Ts...>();
      }
    }

    auto values() const {
      // TODO(lukas): This is dubious once we consider verify mode
      CHECK(acceptor);
      return acceptor->values();
    }

    void init() { init<Undefined, Constant>(); }

    bool result() {
      uint32_t acceptors = 0;
      runners_do([ & ](auto &runner ){ if ( *runner.result ) ++acceptors; });
      return acceptors == 1;
    }

    std::string dump_spawn(VerifyInstruction *v, Spawn &runner) {
      std::stringstream ss;
      ss << runner.GetNodeVal(v)->toString(16, false) << std::endl;;
      ss << pretty_print(v) << std::endl;
      for (auto op : v->operands) {
        ss << "\t" << to_string(op->op_code) << " " << op->id() << " "
           << runner.GetNodeVal(op)->toString(16, false) << std::endl;
      }
      return ss.str();
    }

    // TODO(lukas): If runners do is `const` this can be as well.
    std::string dump_runners() {
      std::stringstream ss;
      runners_do([ & ](auto &runner){
          using dbg_printer = typename Spawn::template DBGPrint< Spawn >;
          ss << dbg_printer{&runner}.gather(false).get();
      });
      return ss.str();
    }

    bool Run() {
      init();

      std::unordered_set<Spawn *> successes;
      for (auto &[_, runner] : runners) {
        if (runner.Run()) {
          successes.emplace(&runner);
        }
      }
      if (successes.size() == 1) {
        acceptor = *successes.begin();
      }
      CHECK(!(successes.size() > 1)) << "Multiple contexts satisfied." << successes.size();
      return successes.size() == 1;
    }
  };

  using DQueueInterpreter = QueueInterpreter<DSpawn>;
  using VQueueInterpreter =  QueueInterpreter<VSpawn>;

}  // namespace circ::run
