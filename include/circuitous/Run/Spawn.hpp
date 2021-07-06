/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Run/Base.hpp>

#include <deque>
#include <unordered_map>

namespace circ::run {

  struct MemoryOrdering {
    using mem_ops_t = std::unordered_set< Operation * >;
    using level_t = std::tuple< uint32_t, mem_ops_t >;
    using constraints_t = std::vector< level_t >;

    Circuit *circuit;
    CtxCollector *collector;
    VerifyInstruction *current;

    constraints_t constraints;
    uint32_t allowed = 0;

    void extend(uint32_t desired) {
      if (desired < constraints.size()) {
        return;
      }
      constraints.resize(desired + 1);
    }

    template< typename MO >
    void init() {
      for (auto op : circuit->Attr< MO >()) {
        if (collector->op_to_ctxs[op].count(current)) {
          auto idx = op->mem_idx();
          extend(idx);
          auto &[count, ops] = constraints[idx];
          ++count;
          ops.insert(op);
        }

      }
    }

    bool raise_level() {
      ++allowed;
      LOG(INFO) << "Raising level";
      return true;
    }

    bool do_enable(Operation *op, uint64_t mem_idx) {
      CHECK_EQ(mem_idx, allowed);
      auto &[count, ops] = constraints[mem_idx];
      if (!ops.count(op)) {
        return false;
      }
      --count;
      ops.erase(op);
      if (count == 0) {
        return raise_level();
      }
      return false;
    }

    std::optional< uint64_t > mem_idx(Operation *op) {
      if (!is_one_of<ReadConstraint, WriteConstraint>(op)) {
        return {};
      }
      if (auto x = dynamic_cast<WriteConstraint *>(op)) { return x->mem_idx(); }
      if (auto x = dynamic_cast<ReadConstraint *>(op)) { return x->mem_idx(); }
      LOG(FATAL) << "Unreachable";
    }

    bool enable(Operation *op) {
      if (auto mi = mem_idx(op)) {
        return do_enable(op, *mi);
      }
      return false;
    }

    MemoryOrdering(Circuit *circuit_, CtxCollector *collector_, VerifyInstruction *c_)
        : circuit(circuit_), collector(collector_), current(c_)
    {
      init<WriteConstraint>();
      init<ReadConstraint>();
    }

  };

  struct State {
    std::deque< Operation * > todo;
    std::unordered_map< uint64_t, std::vector< Operation * > > waiting;
    std::unordered_map< Operation *, uint64_t > blocked;

    MemoryOrdering mem_order;

    State(MemoryOrdering mem_order_)
      : mem_order(std::move(mem_order_))
    {}
    State(const State &) = default;
    State(State &&) = default;

    State& operator=(State) = delete;

    auto Pop() {
      auto x = todo.front();
      todo.pop_front();
      return x;
    }

    void Push(Operation *op) {
      if (!is_one_of<ReadConstraint, WriteConstraint>(op)) {
        return todo.push_back(op);
      }
      auto mem_idx = mem_order.mem_idx(op);
      if (*mem_idx == mem_order.allowed) {
        return todo.push_back(op);
      }
      waiting[*mem_idx].push_back(op);
    }

    void Notify(Operation *op) {
      auto [it, inserted] = blocked.emplace(op, op->operands.size());
      if (it->second <= 1) {
        Push(it->first);
        it->second = 0;
        return;
      }
      --it->second;
    }

    void notify_mem(Operation *op) {
      if (!mem_order.enable(op)) {
        return;
      }

      for (auto x : waiting[mem_order.allowed]) {
        Push(x);
      }
      waiting[mem_order.allowed].clear();
    }

    void SetNodeVal(Operation *op) {
      for (auto user : op->users) {
        Notify(user);
      }
      notify_mem(op);
    }
  };

  template<typename Base>
  struct Spawn : Base {
    using value_type = typename Base::value_type;
    using Base::node_values;
    using Base::circuit;

    using parent_t = Base;

    VerifyInstruction *current;
    CtxCollector *collector;
    State state;
    std::optional<bool> result;

    Spawn(Circuit *circuit_, VerifyInstruction *current_,
          CtxCollector *collector_)
        : parent_t(circuit_), current(current_),
          collector(collector_),
          state(MemoryOrdering(circuit_, collector_, current_))
    {}

    Spawn(const Spawn &) = delete;
    Spawn(Spawn &&) = default;

    Spawn &operator=(Spawn) = delete;

    void SetNodeVal(Operation *op, const value_type &val) {
      if (node_values.count(op)) {
        CHECK(node_values[op] == val)
            << op->op_code_str() << " already has value "
            << node_values[op]->toString(16, false) << " "
            << node_values[op]->getBitWidth()
            << " yet we try to set "
            << val->toString(16, false) << " " << val->getBitWidth();
        return;
      }
      // This node is not used in current context, just skip.
      if (!collector->op_to_ctxs[op].count(current)) {
        return;
      }
      this->parent_t::SetNodeVal(op, val);
      state.SetNodeVal(op);
    }

    void Dispatch(Operation *op) {
      if (collector->op_to_ctxs[op].count(current)) {
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

    template<typename T>
    void init_notify_() {
      for (auto node : circuit->template Attr<T>()) {
        if (this->node_values.count(node)) {
          continue;
        }

        if (!collector->op_to_ctxs[node].count(current)) {
          continue;
        }
        for (auto user : node->users) {
          if (constrained_by(node, user)) {
            state.Notify(user);
          }
        }
      }
    }

    template<typename ...Ts>
    void init_notify() {
      return (init_notify_<Ts>(), ...);
    }

    void init() {
      parent_t::init();
      init_notify<Advice, OutputRegister, OutputErrorFlag, OutputTimestamp, Memory>();
    }

    bool Run() {
      init();
      while (!state.todo.empty()) {
        auto x = state.Pop();
        Dispatch(x);
      }
      result = [&](){
        if (auto res = this->GetNodeVal(current)) {
          return *res == this->TrueVal();
        }
        return false;
      }();
      return *result;
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

} // namespace circ::run