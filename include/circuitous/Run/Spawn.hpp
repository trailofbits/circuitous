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
    template< typename Semantics >
    struct Spawn : StateOwner
    {
        using semantics_t = Semantics;

        Circuit *circuit;
        VerifyInstruction *current;
        CtxCollector *collector;
        State state;
        std::optional<bool> result;

        Semantics semantics;
        NodeState node_state;
        Memory memory;

        std::stringstream _dbg;

        Spawn(Circuit *circuit, VerifyInstruction *current_,
              CtxCollector *collector_)
        : circuit(circuit),
          current(current_),
          collector(collector_),
          state(MemoryOrdering(circuit, collector_, current_)),
          semantics(this, circuit),
          memory(circuit)
        {}

        Spawn(const Spawn &) = delete;
        Spawn(Spawn &&) = delete;

        Spawn &operator=(Spawn) = delete;

        void set_node_val(Operation *op, const value_type &val) override
        {
            if (node_state.has_value(op))
            {
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
            state.notify_from(op);
        }

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

        void set_memory(uint64_t addr, const std::string &data)
        {
            check(data.size() % 2 == 0);
            uint64_t offset = 0;
            for (std::size_t i = 0; i < data.size(); i += 2)
            {
                auto str = data.substr(i, 2);
                auto val = llvm::APInt(8, str, 16);
                this->memory.store(addr + offset, val);
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

            for (auto node : circuit->template attr< T >())
            {
                if (this->node_state.has_value(node))
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
            semantics.init();
            init_notify< Advice, OutputRegister, OutputErrorFlag, OutputTimestamp,
                         circ::Memory >();
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

        bool Run() {
            init();
            while (!state.empty()) {
                auto x = state.pop();
                log_dbg() << "Dispatching" << pretty_print< true >(x);
                dispatch(x);
                if (auto r = short_circuit(x)) {
                    log_dbg() << "Short circuiting to result, as decode was not satisfied.";
                    result = *r;
                    return *result;
                }
            }
            log_dbg() << "Run done, fetching result.";
            result = [&](){
                if (auto res = node_state.get(current)) {
                    return *res == semantics.true_val();
                }
                return false;
            }();
            return *result;
        }

        void set_input_state(const trace::Entry &in)
        {
            auto inst_bits = circuit->input_inst_bits();
            this->set_node_val(inst_bits, in.get_inst_bits(inst_bits->size));

            this->set_node_val(circuit->input_ebit(), in.get_ebit());
            this->set_node_val(circuit->input_timestamp(), in.get_timestamp());
            for (auto &[name, val] : in.regs) {
                if (auto reg = circuit->input_reg(name)) {
                    this->set_node_val(reg, llvm::APInt(reg->size, val));
                }
            }

            for (auto hint : circuit->attr<circ::Memory>()) {
                if (auto val = in.get_mem_hint(std::to_string(hint->mem_idx))) {
                    this->set_node_val(hint, *val);
                }
            }
        }

        void set_output_state(const trace::Entry &out)
        {
            this->set_node_val(circuit->output_ebit(), out.get_ebit());
            this->set_node_val(circuit->output_timestamp(), out.get_timestamp());
            for (auto &[name, val] : out.regs) {
                if (auto reg = circuit->output_reg(name)) {
                    this->set_node_val(reg, llvm::APInt(reg->size, val));
                }
            }
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

    using DSpawn = Spawn< DBase >;
    using VSpawn = Spawn< VBase >;

    static_assert(valid_interpreter< typename DSpawn::semantics_t >());
    static_assert(valid_interpreter< typename VSpawn::semantics_t >());

} // namespace circ::run
