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

    auto MemoryBuilder::set(std::size_t addr, const value_type &value) -> self_t &
    {
        check(value) << "TODO(lukas): Memory cannot have undefined values.";
        memory.store(addr, *value);
        return *this;
    }

    void set_if(auto &state, Operation *op, const value_type &val)
    {
        if (!val)
            return;

        state.set(op, val);
    }
} // namespace circ::run
