/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Trace.hpp>

#include <circuitous/Run/Base.hpp>
#include <circuitous/Run/Queue.hpp>
#include <circuitous/Run/State.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <deque>
#include <unordered_map>

namespace circ::run
{
    // Class that glues together all pieces needed to interpret a context in the circuit.
    // NOTE(lukas): Implementation of `StateOwner` interface is a way to reduce the template
    //              load of this class - another approach is to let `Semantics` be a CRTP
    //              class instead and inject `Spawn` into it.
    template< typename Semantics >
    struct Spawn : StateOwner
    {
        using semantics_t = Semantics;

        Circuit *circuit;
        VerifyInstruction *current;
        CtxCollector *collector;
        // Queue of Operations to be dispatched.
        TodoQueue todo;

        // Class that tells what are Operation doing with their values.
        Semantics semantics;
        // Memory state - in strict verify mode this is not needed (as memory operations
        // are provided by memory hints).
        Memory memory;
        // Mapping of `Operation * -> value`
        NodeState node_state;

        Spawn(Circuit *circuit, VerifyInstruction *current,
              CtxCollector *collector, const NodeState &node_state, const Memory &memory)
        : circuit(circuit),
          current(current),
          collector(collector),
          todo(MemoryOrdering(circuit, collector, current)),
          semantics(this, circuit),
          memory(memory),
          node_state(node_state)
        {}

        // NOTE(lukas): `semantics` are holding a pointer to `this` -> therefore if it is
        //              decided that move/copy ctor is needed, keep that in mind.
        Spawn(const Spawn &) = delete;
        Spawn(Spawn &&) = delete;

        Spawn &operator=(Spawn) = delete;

        // If `op` already has a value different than `val` hard error is hit -> this most
        // likely means a bug occurred.
        // NOTE(lukas): The best option would be if we never try to set *any* value to `op`
        //              that already has some. Unfortunately, this would require some hack
        //              elsewhere, as when deriving a value -> parent sets value to child ->
        //              notification is fired again and adds parent to queue. One option
        //              is to never add again things into queue that were processes once,
        //              but I think it exposes a bigger surface for bugs than current approach.
        void set_node_val(Operation *op, const value_type &val) override
        {
            if (node_state.has_value(op))
            {
                // Helpful formatter to report error.
                auto fmt = [](auto what) -> std::string
                {
                    if (!what)
                        return "( no value )";
                    std::stringstream ss;
                    ss << "[ "<< what->getBitWidth()
                       << "b: " << llvm::toString(*what, 16, false)
                       << " ]";
                    return ss.str();
                };
                check(node_state.get(op) == val, [&](){
                    std::stringstream ss;
                    ss << pretty_print(op) << " already has value "
                       << fmt(node_state.get(op))
                       << " yet we try to set "
                       << fmt(val);
                       return ss.str();
                });
                log_dbg() << "Assign:" << pretty_print< false >(op)
                          << "value was already set.";
                return;
            }
            // This node is not used in current context, just skip.
            if (!collector->op_to_ctxs[op].count(current) && !isa< leaf_values_ts >(op))
            {
                log_dbg() << "Assign:" << pretty_print< false >(op)
                          << "node not in the current context, do not set value.";
                return;
            }
            this->node_state.set(op, val);
            notify_from(op);
        }

        /* StateOwner interface */

        value_type get_node_val(Operation *op) const override { return node_state.get(op); }
        bool has_value(Operation *op) const override { return node_state.has_value(op); }

        void store(uint64_t addr, const raw_value_type &data) override
        {
            memory.store(addr, data);
        }

        value_type load(uint64_t addr, std::size_t size) const override
        {
            return memory.load(addr, size);
        }

        bool defined(uint64_t addr, std::size_t size) const override
        {
            return memory.defined(addr, size);
        }

        void visit(Operation *op)
        {
            semantics.dispatch(op);
        }

        void dispatch(Operation *op)
        {
            if (collector->op_to_ctxs[op].count(current))
                semantics.dispatch(op);
        }

        auto is_in_current_ctx()
        {
            return [&](const auto &op)
            {
                return collector->op_to_ctxs[op].count(current);
            };
        }

        void notify_advice_constraints()
        {
            for (auto undef : circuit->attr< Undefined >())
                notify_from(undef);
        }

        void notify_from(Operation *op)
        {
            todo.notify_from(op, is_in_current_ctx());
        }

        void derive(const std::unordered_set< Operation * > &ops)
        {
            for (auto op : ops)
            {
                if (is_in_current_ctx()(op))
                {
                    semantics.to_derive(op->operands[1], op);
                    todo.notify_self(op);
                }
            }
        }

        void init()
        {
            semantics.init();
            for (const auto &[op, _] : node_state.node_values)
                notify_from(op);
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
                auto maybe_value = node_state.get(op);
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

        std::optional< bool > short_circuit(Operation *op)
        {
            // This can happen if `op` is not in `current` context.
            if (!node_state.has_value(op))
                return {};
            switch (op->op_code) {
                case DecoderResult::kind:
                {
                    if (node_state.get(op) == semantics.false_val())
                        return std::make_optional(false);
                    return {};
                }
                default: return {};
            }
        }

        bool run() {
            init();
            // Set constants first, as they are never blocked by anything.
            for (const auto &constant : circuit->attr< Constant >())
                dispatch(constant);

            while (!todo.empty()) {
                auto x = todo.pop();
                log_dbg() << "Dispatching" << pretty_print< true >(x);
                dispatch(x);
                // If decoder result failed, we can just return a false as the result will
                // be false.
                if (auto r = short_circuit(x)) {
                    log_dbg() << "Short circuiting to result, as decode was not satisfied.";
                    return *r;
                }
            }
            log_dbg() << "Run done, fetching result.";
            if (!node_state.has_value(current))
            {
                no_value_reached();
                return false;
            }
            if (auto res = node_state.get(current)) {
                return *res == semantics.true_val();
            }
            return false;
        }

        // If context does not have a value -> something is blocking it.
        // Report a trace that highlights which nodes are not interpreted and what is blocking
        // them.
        void no_value_reached()
        {
            std::stringstream ss;

            auto fmt = [&](auto what, auto &prefix)
            {
                ss << prefix << pretty_print< false >(what)
                   << " : " << todo.status(what) << "\n";
            };

            auto rec_print = [&](auto what, std::size_t indent, auto &rec) -> void
            {
                std::string prefix(indent * 2, ' ');
                if (has_value(what))
                {
                    ss << prefix << " * \n";
                    return;
                }

                fmt(what, prefix);
                for (auto op : what->operands)
                    rec(op, indent + 1, rec);
            };

            rec_print(current, 0, rec_print);
            log_dbg() << ss.str();
        }

        trace::Entry get_output_state() const
        {
            trace::Entry out;

            for (auto op : circuit->attr< OutputRegister >())
                out.regs[op->reg_name] = node_state.get(op)->getLimitedValue();

            out.ebit = node_state.get(circuit->output_ebit()) == semantics.true_val();
            out.timestamp = node_state.get(circuit->output_timestamp())->getLimitedValue();

            return out;
        }

        template< typename T >
        auto get_derived() const { return semantics.template get_derived< T >(); }

        auto get_derived_mem()
        {
            std::vector< Memory::Parsed > out;
            for (auto op : circuit->attr< circ::Memory >())
            {
                if (node_state.has_value(op))
                    out.push_back(memory.deconstruct(*node_state.get(op)));
            }
            return out;
        }
    };

    static_assert(valid_interpreter< typename Spawn< Base >::semantics_t >());

} // namespace circ::run
