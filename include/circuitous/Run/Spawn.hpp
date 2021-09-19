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

    std::string status(Operation *op) {
      std::stringstream ss;
      ss << "[ " << blocked[op] << " / " << op->operands.size() << "]";
      return ss.str();
    }

    auto Pop() {
      auto x = todo.front();
      todo.pop_front();
      return x;
    }

    auto push_todo(Operation *op)
    {
      if (is_one_of< InputInstructionBits, Extract, Concat, DecodeCondition >(op))
        return todo.push_back(op);
      return todo.push_back(op);
    }

    void Push(Operation *op) {
      if (!is_one_of<ReadConstraint, WriteConstraint>(op)) {
        return push_todo(op);
      }
      auto mem_idx = mem_order.mem_idx(op);
      if (*mem_idx == mem_order.allowed) {
        return push_todo(op);
      }
      waiting[*mem_idx].push_back(op);
    }

    // Generic notify call, allows for callback - either extra preprocessing or dbg info
    template<typename F>
    void notify(Operation *from, Operation *to, F &&fn) {
      fn(from, to);
      _notify(to);
    }

    // Verbose notification for debug purposes
    void notify_verbose(Operation *from, Operation *to) {
      auto dbg_info = [](auto from, auto to) {
        LOG(INFO) << pretty_print< false >(from) << " -- notifies -> " << pretty_print(to);
      };
      return notify(from, to, dbg_info);
    }

    // General notify that does no extra work
    void notify(Operation *from, Operation *to) {
      return notify(from, to, [](auto, auto){});
    }

    // Implementation
    void _notify(Operation *op) {
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
        notify(op, user);
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

    std::stringstream _dbg;

    Spawn(Circuit *circuit_, VerifyInstruction *current_,
          CtxCollector *collector_)
        : parent_t(circuit_), current(current_),
          collector(collector_),
          state(MemoryOrdering(circuit_, collector_, current_))
    {}

    Spawn(const Spawn &) = delete;
    Spawn(Spawn &&) = default;

    Spawn &operator=(Spawn) = delete;

    using Base::SetNodeVal;

    void SetNodeVal(Operation *op, const value_type &val) {
      if (node_values.count(op)) {
        CHECK(node_values[op] == val)
            << pretty_print(op) << " already has value "
            << node_values[op]->toString(16, false) << " "
            << node_values[op]->getBitWidth()
            << " yet we try to set "
            << val->toString(16, false) << " " << val->getBitWidth();
        return;
      }
      // This node is not used in current context, just skip.
      if (!collector->op_to_ctxs[op].count(current) && !is_of< LeafValue >(op)) {
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
            state.notify(node, user);
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

    template< typename S >
    struct DBGPrint {
      S *parent;

      std::unordered_set< Operation * > seen;
      std::stringstream ss;

      DBGPrint(S *parent_) : parent(parent_) { ss << parent->_dbg.str(); }

      void dispatch(Operation *op, bool skip_unset) {
        if (seen.count(op)) return;
        seen.insert(op);

        auto next = [&]() {
          for (auto o : op->operands)
            this->dispatch(o, skip_unset);
        };

        ss << " [" << op->id() << "] " << op->Name() << " ";
        if (!parent->node_values.count(op)) {
          ss << "(no value set)";
          if (skip_unset) {
            ss << std::endl;
            return;
          }
        } else {
          auto val = parent->GetNodeVal(op);
          ss << (val ? val->toString(16, false) : "(undef)");
        }

        if (op->operands.size() == 0) {
          ss << std::endl;
          return;
        }

        ss << " ->\n";

        auto fmt_node_value = [&](auto o) {
          ss << o->id() << " ";
          if (!parent->node_values.count(op))
            ss << " (no value set)";
          else {
            auto val = parent->GetNodeVal(o);
            ss << (val ? val->toString(16, false) : "(undef)");
          }
        };

        if (op->op_code == VerifyInstruction::kind) {
          for (auto o : op->operands) {
            if (o->op_code == DecodeCondition::kind) {
              ss << " ~~~> decode: ";
              fmt_node_value(o);
              ss << std::endl;
            }
          }
        }

        for (auto o : op->operands) {
          ss << "\t - ";
          fmt_node_value(o);
          ss << std::endl;
        }
        next();
      }

      DBGPrint &gather(bool skip_unset = true) {
        dispatch(parent->current, skip_unset);
        return *this;
      }

      std::string get() { return ss.str(); }
    };

    std::string dbg_context_dump() {
      std::stringstream ss;
      if (!this->has_value(current)) {
        ss << state.status(current) << std::endl;
      }
      for (auto x : current->operands) {
        ss << state.status(x) << " " << this->has_value(x)
           << " " << pretty_print< false >(x) << " " << this->val_as_str(x) << std::endl;
        for (auto y : x->operands) {
          ss << "\t" << state.status(y) << " " << this->has_value(y)
             << " " << pretty_print< false >(y) << " " << this->val_as_str(y) << std::endl;
        }
      }
      return ss.str();
    }

    std::optional< bool > short_circuit(Operation *op) {
      // This can happen if `op` is not in `current` context.
      if (!this->has_value(op))
        return {};
      switch (op->op_code) {
        case DecodeCondition::kind: {
            if (this->GetNodeVal(op) == this->FalseVal())
              return std::make_optional(false);
            return {};
        }
        default: return {};
      }
    }

    bool Run() {
      init();
      while (!state.todo.empty()) {
        auto x = state.Pop();
        Dispatch(x);
        if (auto r = short_circuit(x)) {
          result = *r;
          return *result;
        }
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
