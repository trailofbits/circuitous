/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Run/Base.hpp>

#include <deque>
#include <unordered_map>

namespace circuitous::run {

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
      if (it->second <= 1) {
        Push(it->first);
      }
      --it->second;
    }

    void SetNodeVal(Operation *op) {
      for (auto user : op->users) {
        Notify(user);
      }
    }
  };

  template<typename Base>
  struct Spawn : Base {
    using value_type = typename Base::value_type;
    using Base::node_values;
    using Base::circuit;

    using parent_t = Base;

    VerifyInstruction *current;
    State state;

    Spawn(Circuit *circuit_, VerifyInstruction *current_,
          const State &state_)
        : parent_t(circuit_), current(current_), state(state_)
    {
      init();
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

    void Visit(Operation *op) {
      if (state.collector.op_to_ctxs[op].count(current)) {
        parent_t::Visit(op);
      }
    }

    void init() {
      parent_t::init();

      for (auto hc : circuit->template Attr<HintCondition>()) {
        // TODO(lukas): I am not sure if the following scenario propagates properly
        //              `HintCondition(H1, A)`
        //              `HintCondition(H1, H2)`
        //               I am not sure that after visting the first condition the value
        //               of H1 will propagate to H2.
        if (state.collector.op_to_ctxs[hc].count(current)) {
          state.blocked[hc] = 1;
        }
      }

      for (auto oreg : circuit->template Attr<OutputRegister>()) {
        if (this->node_values.count(oreg)) {
          continue;
        }
        if (state.collector.op_to_ctxs[oreg].count(current)) {
          for (auto user : oreg->users) {
            state.Notify(user);
          }
        }
      }
      for (auto oreg : circuit->template Attr<OutputErrorFlag>()) {
        if (this->node_values.count(oreg)) {
          continue;
        }
        if (state.collector.op_to_ctxs[oreg].count(current)) {
          for (auto user : oreg->users) {
            if (user->op_code == Operation::kRegisterCondition) {
              state.Notify(user);
            }
          }
        }
      }
    }

    bool Run() {
      while (!state.todo.empty()) {
        auto x = state.Pop();
        Visit(x);
      }
      for (auto x : current->operands) {
        LOG(INFO) << to_string(x->op_code) << " " << x->id()
                  << " " << this->node_values.count(x);
      }
      if (auto res = this->GetNodeVal(current)) {
        return *res == this->TrueVal();
      }
      return false;
    }
  };

  struct DSpawn : Spawn<DBase<DSpawn>> {
    using parent_t = Spawn<DBase<DSpawn>>;
    using parent_t::parent_t;
  };

  struct VSpawn : Spawn<VBase<VSpawn>> {
    using parent_t = Spawn<VBase<VSpawn>>;
    using parent_t::parent_t;
  };

  static_assert(valid_interpreter<DSpawn>());
  static_assert(valid_interpreter<VSpawn>());

} // namespace circuitous::run