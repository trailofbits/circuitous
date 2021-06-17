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

    State& operator=(State) = delete;

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
    {}

    Spawn(const Spawn &) = default;
    Spawn(Spawn &&) = delete;

    Spawn &operator=(Spawn) = delete;

    void SetNodeVal(Operation *op, const value_type &val) {
      if (node_values.count(op) && node_values[op].has_value()) {
        CHECK(node_values[op] == val);
        return;
      }
      this->parent_t::SetNodeVal(op, val);
      state.SetNodeVal(op);
    }

    void Dispatch(Operation *op) {
      if (state.collector.op_to_ctxs[op].count(current)) {
        parent_t::Dispatch(op);
      }
    }

    void set_memory(uint64_t addr, const std::string &data) {
      CHECK(data.size() % 2 == 0);
      uint64_t offset = 0;
      for (std::size_t i = 0; i < data.size(); i += 2) {
        auto str = data.substr(i, 2);
        auto val = llvm::APInt(8, str, 16);
        this->store(addr + offset, val);
        ++offset;
      }
    }

    void init() {
      parent_t::init();

      for (auto hint : circuit->template Attr<Advice>()) {
        if (this->node_values.count(hint)) {
          continue;
        }
        if (state.collector.op_to_ctxs[hint].count(current)) {
          for (auto user : hint->users) {
            if (user->op_code == AdviceConstraint::kind) {
              state.Notify(user);
            }
          }
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
            if (user->op_code == RegConstraint::kind) {
              state.Notify(user);
            }
          }
        }
      }
      for (auto oreg : circuit->template Attr<OutputTimestamp>()) {
        if (this->node_values.count(oreg)) {
          continue;
        }
        if (state.collector.op_to_ctxs[oreg].count(current)) {
          for (auto user : oreg->users) {
            state.Notify(user);
          }
        }
      }
      for (auto oreg : circuit->template Attr<Memory>()) {
        if (this->node_values.count(oreg)) {
          continue;
        }
        if (state.collector.op_to_ctxs[oreg].count(current)) {
          for (auto user : oreg->users) {
            if (is_one_of<ReadConstraint, WriteConstraint>(user)) {
              state.Notify(user);
            }
          }
        }
      }
    }

    bool Run() {
      init();
      while (!state.todo.empty()) {
        auto x = state.Pop();
        Dispatch(x);
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