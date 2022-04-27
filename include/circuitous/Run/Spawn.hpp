/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Trace.hpp>
#include <circuitous/Run/Base.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <deque>
#include <unordered_map>

namespace circ::run
{

    struct MemoryOrdering
    {
        using mem_ops_t = std::unordered_set< Operation * >;
        using level_t = std::tuple< uint32_t, mem_ops_t >;
        using constraints_t = std::vector< level_t >;

        Circuit *circuit;
        CtxCollector *collector;
        VerifyInstruction *current;

        constraints_t constraints;
        uint32_t allowed = 0;

        void extend(uint32_t desired)
        {
            if (desired < constraints.size())
                return;
            constraints.resize(desired + 1);
        }

        template< typename MO >
        void init()
        {
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

        bool raise_level()
        {
            ++allowed;
            return true;
        }

        bool do_enable(Operation *op, uint64_t mem_idx)
        {
            check(mem_idx == allowed);
            auto &[count, ops] = constraints[mem_idx];
            if (!ops.count(op))
                return false;

            --count;
            ops.erase(op);
            if (count == 0)
                return raise_level();

            return false;
        }

        std::optional< uint64_t > mem_idx(Operation *op)
        {
            if (!is_one_of<ReadConstraint, WriteConstraint>(op))
                return {};

            if (auto x = dynamic_cast<WriteConstraint *>(op)) return x->mem_idx();
            if (auto x = dynamic_cast<ReadConstraint *>(op)) return x->mem_idx();
            unreachable() << "Unreachable";
        }

        bool enable(Operation *op)
        {
            if (auto mi = mem_idx(op))
                return do_enable(op, *mi);
            return false;
        }

        MemoryOrdering(Circuit *circuit_, CtxCollector *collector_, VerifyInstruction *c_)
            : circuit(circuit_), collector(collector_), current(c_)
        {
            init<WriteConstraint>();
            init<ReadConstraint>();
        }

    };

    struct State
    {
        std::deque< Operation * > todo;
        std::unordered_map< uint64_t, std::vector< Operation * > > waiting;
        std::unordered_map< Operation *, uint64_t > blocked;

        MemoryOrdering mem_order;

        State(MemoryOrdering mem_order_) : mem_order(std::move(mem_order_)) {
        }
        State(const State &) = default;
        State(State &&) = default;

        State& operator=(State) = delete;

        std::string status(Operation *op)
        {
            std::stringstream ss;
            ss << "[ " << blocked[op] << " / " << op->operands.size() << "]";
            return ss.str();
        }

        auto Pop()
        {
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

        void Push(Operation *op)
        {
            if (!is_one_of<ReadConstraint, WriteConstraint>(op))
                return push_todo(op);
            auto mem_idx = mem_order.mem_idx(op);
            if (*mem_idx == mem_order.allowed)
                return push_todo(op);
            waiting[*mem_idx].push_back(op);
        }

        // Generic notify call, allows for callback - either extra preprocessing or dbg info
        template<typename F>
        void notify(Operation *from, Operation *to, F &&fn)
        {
            fn(from, to);
            _notify(to);
        }

        // Verbose notification for debug purposes
        void notify_verbose(Operation *from, Operation *to)
        {
            auto dbg_info = [&](auto from, auto to) {
                auto tail = [&]() -> std::string
                {
                    if (blocked.count(to))
                        return std::to_string(blocked[to]);
                    return "(unknown)";
                }();

                log_dbg() << pretty_print< false >(from) << " -- notifies -> "
                          << pretty_print< false >(to)
                          << "which is blocked by:" << tail;
            };
            return notify(from, to, dbg_info);
        }

        // General notify that does no extra work
        void notify(Operation *from, Operation *to)
        {
            return notify_verbose(from, to);
        }

        // Implementation
        void _notify(Operation *op)
        {
            auto [it, inserted] = blocked.emplace(op, op->operands.size());
            if (it->second <= 1) {
                Push(it->first);
                it->second = 0;
                return;
            }
            --it->second;
        }

        void notify_mem(Operation *op)
        {
            if (!mem_order.enable(op))
                return;

            for (auto x : waiting[mem_order.allowed])
                Push(x);
            waiting[mem_order.allowed].clear();
        }

        void SetNodeVal(Operation *op)
        {
            for (auto user : op->users)
                notify(op, user);
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

        void SetNodeVal(Operation *op, const value_type &val)
        {
            if (node_values.count(op))
            {
                auto fmt = [](auto what) -> std::string
                {
                    if (!what)
                        return "( no value )";
                    std::stringstream ss;
                    ss << "[ "<< what->getBitWidth() << "b: " << llvm::toString(*what, 16, false)
                       << " ]";
                    return ss.str();
                };
                check(this->get(op) == val, [&](){
                    std::stringstream ss;
                    ss << pretty_print(op) << " already has value "
                       << fmt(this->get(op))
                       << " yet we try to set "
                       << fmt(val);
                       return ss.str();
                });
                log_dbg() << "Assign:" << pretty_print< false >(op)
                          << "value was already set.";
                return;
            }
            // This node is not used in current context, just skip.
            if (!collector->op_to_ctxs[op].count(current) && !is_of< LeafValue >(op))
            {
                log_dbg() << "Assign:" << pretty_print< false >(op)
                          << "node not in the current context, do not set value.";
                return;
            }
            this->parent_t::SetNodeVal(op, val);
            state.SetNodeVal(op);
        }

        void Dispatch(Operation *op)
        {
            log_dbg() << "Dispatching: " << pretty_print(op);
            if (collector->op_to_ctxs[op].count(current))
                parent_t::Dispatch(op);
        }

        void set_memory(uint64_t addr, const std::string &data)
        {
            check(data.size() % 2 == 0);
            uint64_t offset = 0;
            for (std::size_t i = 0; i < data.size(); i += 2)
            {
                auto str = data.substr(i, 2);
                auto val = llvm::APInt(8, str, 16);
                this->store(addr + offset, val);
                ++offset;
            }
        }

        template<typename T>
        void init_notify_()
        {
            auto check_position = [&](auto node, auto user)
            {
                if constexpr (!std::is_same_v< T, Advice >) return true;
                else {
                    for (std::size_t i = 0; i < user->operands.size(); ++i)
                        if (node == user->operands[i])
                            return i == 1;
                    unreachable();
                }
            };

            for (auto node : circuit->template Attr<T>())
            {
                if (this->node_values.count(node))
                    continue;

                if (!collector->op_to_ctxs[node].count(current))
                    continue;

                for (auto user : node->users)
                {
                    if (constrained_by(node, user) && check_position(node, user))
                        state.notify(node, user);
                }
            }
        }

        template<typename ...Ts>
        void init_notify()
        {
            return (init_notify_<Ts>(), ...);
        }

        void init()
        {
            parent_t::init();
            init_notify<Advice, OutputRegister, OutputErrorFlag, OutputTimestamp, Memory>();
        }


        // `[ current, next ]`.
        // In `next` only values "visible" by this transition are updated.
        std::tuple< std::string, std::string > to_traces(Circuit *circuit)
        {
            auto trace = Trace::make(circuit);
            std::string current_trace(trace.total_size, '0');
            std::string next_trace(trace.total_size, '0');

            auto inject = [&](auto &where, auto op, auto &field) {
                const auto &[start, size, _] = field;
                auto maybe_value = this->GetNodeVal(op);
                check(maybe_value);

                auto val_as_str = maybe_value->toString(2, false);
                if (val_as_str.size() < size)
                {
                    auto diff = size - val_as_str.size();
                    val_as_str.insert(0, std::string(diff, '0'));
                }

                where.replace(start, size, val_as_str);
            };

            for (auto &[op, field] : trace.parse_map)
            {
                if (is_one_of(op, input_leaves_ts{}))
                    inject(current_trace, op, *field);
                else if (is_one_of(op, output_leaves_ts{}))
                    inject(next_trace, op, *field);
                else
                    unreachable() << "Unreachable.";
            }
            check(current_trace.size() == trace.total_size);
            check(next_trace.size() == trace.total_size);
            return { current_trace, next_trace };
        }

        template< typename S >
        struct DBGPrint
        {
            S *parent;

            std::unordered_set< Operation * > seen;
            std::stringstream ss;

            DBGPrint(S *parent_) : parent(parent_) { ss << parent->_dbg.str(); }

            void dispatch(Operation *op, bool skip_unset)
            {
                if (seen.count(op)) return;
                seen.insert(op);

                auto next = [&]() {
                    for (auto o : op->operands)
                        this->dispatch(o, skip_unset);
                };

                ss << " [" << op->id() << "] " << op->Name() << " ";
                if (!parent->node_values.count(op))
                {
                      ss << "(no value set)";
                      if (skip_unset) {
                          ss << std::endl;
                          return;
                      }
                } else {
                    auto val = parent->GetNodeVal(op);
                    ss << (val ? llvm::toString(*val, 16, false) : "(undef)");
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
                        ss << (val ? llvm::toString(*val, 16, false) : "(undef)");
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

            DBGPrint &gather(bool skip_unset = true)
            {
                dispatch(parent->current, skip_unset);
                return *this;
            }

            std::string get() { return ss.str(); }
        };

        std::string dbg_context_dump()
        {
            std::stringstream ss;
            if (!this->has_value(current))
                ss << state.status(current) << std::endl;
            for (auto x : current->operands)
            {
                ss << state.status(x) << " " << this->has_value(x)
                   << " " << pretty_print< false >(x) << " "
                   << this->val_as_str(x) << std::endl;

                for (auto y : x->operands)
                  ss << "\t" << state.status(y) << " " << this->has_value(y)
                     << " " << pretty_print< false >(y) << " "
                     << this->val_as_str(y) << std::endl;
            }
            return ss.str();
        }

        std::optional< bool > short_circuit(Operation *op)
        {
            // This can happen if `op` is not in `current` context.
            if (!this->has_value(op))
                return {};
            switch (op->op_code) {
                case DecoderResult::kind:
                {
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
                    log_dbg() << "Short circuiting to result, as decode was not satisfied.";
                    result = *r;
                    return *result;
                }
            }
            log_dbg() << "Run done, fetching result.";
            result = [&](){
                if (auto res = this->GetNodeVal(current)) {
                    return *res == this->TrueVal();
                }
                return false;
            }();
            return *result;
        }
    };

    struct DSpawn : Spawn<DBase<DSpawn>>
    {
        using parent_t = Spawn<DBase<DSpawn>>;
        using parent_t::parent_t;
    };

    struct VSpawn : Spawn<VBase<VSpawn>>
    {
        using parent_t = Spawn<VBase<VSpawn>>;
        using parent_t::parent_t;
    };

    static_assert(valid_interpreter<DSpawn>());
    static_assert(valid_interpreter<VSpawn>());

} // namespace circ::run
