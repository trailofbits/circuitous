/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Run/Interpreter.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/ADT/APInt.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/Run/Trace.hpp>
#include <circuitous/Run/State.hpp>

namespace circ::run
{

    auto MemoryBuilder::set(std::size_t addr, const std::string &data) -> self_t &
    {
        check(data.size() % 2 == 0);
        uint64_t offset = 0;
        for (std::size_t i = 0; i < data.size(); i += 2)
        {
            auto str = data.substr(i, 2);
            auto val = llvm::APInt(8, str, 16);
            memory.store(addr + offset, val);
            ++offset;
        }
        return *this;
    }

    auto MemoryBuilder::set(const trace::native::Entry &trace) -> self_t &
    {
        // REFACTOR(lukas): Consider how to change this API/keep it at all.
        return *this;
    }

    void set_if(auto &state, Operation *op, const value_type &val)
    {
        if (!val)
            return;

        state.set(op, val);
    }

    auto NodeStateBuilder::output_trace(const trace::native::Entry &out) -> self_t &
    {
        set_if(node_state, circuit->output_ebit(), out.ebit);
        set_if(node_state, circuit->output_timestamp(), out.timestamp);

        for (auto &[name, val] : out.regs)
            if (auto reg = circuit->output_reg(name))
                set_if(node_state, reg, val);
        return *this;
    }

    auto NodeStateBuilder::input_trace(const trace::native::Entry &in) -> self_t &
    {
        NodeState out;

        // Set singletons.
        set_if(node_state, circuit->input_inst_bits(), in.inst_bits);
        set_if(node_state, circuit->input_ebit(), in.ebit);
        set_if(node_state, circuit->input_timestamp(), in.timestamp);

        // Set regs
        for (auto &[name, val] : in.regs)
            if (auto reg = circuit->input_reg(name))
                set_if(node_state, reg, val);

        for (auto hint : circuit->attr<circ::Memory>())
            if (auto val = in.get_mem_hint(std::to_string(hint->mem_idx)))
                set_if(node_state, hint, val);

        return *this;
    }
} // namespace circ::run
