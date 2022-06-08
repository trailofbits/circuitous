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

    auto MemoryBuilder::set(const trace::Entry &trace) -> self_t &
    {
        for (auto [addr, data] : trace.initial_memory)
            set(addr, data);
        return *this;
    }

    auto NodeStateBuilder::output_trace(const trace::Entry &out) -> self_t &
    {
        node_state.set(circuit->output_ebit(), out.get_ebit());
        node_state.set(circuit->output_timestamp(), out.get_timestamp());
        for (auto &[name, val] : out.regs) {
            if (auto reg = circuit->output_reg(name)) {
                node_state.set(reg, llvm::APInt(reg->size, val));
            }
        }
        return *this;
    }

    auto NodeStateBuilder::input_trace(const trace::Entry &in) -> self_t &
    {
        NodeState out;

        // Set singletons.
        auto inst_bits = circuit->input_inst_bits();
        node_state.set(circuit->input_inst_bits(), in.get_inst_bits(inst_bits->size));
        node_state.set(circuit->input_ebit(), in.get_ebit());
        node_state.set(circuit->input_timestamp(), in.get_timestamp());

        // Set regs
        for (auto &[name, val] : in.regs)
            if (auto reg = circuit->input_reg(name))
                node_state.set(reg, llvm::APInt(reg->size, val));

        for (auto hint : circuit->attr<circ::Memory>())
            if (auto val = in.get_mem_hint(std::to_string(hint->mem_idx)))
                node_state.set(hint, *val);
        return *this;
    }
} // namespace circ::run
