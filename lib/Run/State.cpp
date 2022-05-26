/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Run/State.hpp>

#include <circuitous/IR/Circuit.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/ADT/APInt.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ::run
{
    Memory::Memory(Circuit *circuit) : hint_size(circuit->ptr_size) {}

    bool Memory::defined(uint64_t addr, std::size_t size)
    {
        for (auto i = 0u; i < size; ++i)
            if (!memory.count(addr + i))
                return false;
        return true;
    }

    auto Memory::load(uint64_t addr, std::size_t size) -> value_type
    {
        if (!defined(addr, size))
            return {};

        llvm::APInt build{ static_cast< uint32_t >(size * 8), 0, false };
        for (auto i = 0u; i < size; ++i)
            build.insertBits( memory[addr + i], i * 8 );

        return build;
    }

    void Memory::store(uint64_t addr, raw_value_type val)
    {
        check( val.getBitWidth() % 8 == 0 )
            << "Cannot store val that has unalinged bw such as " << val.getBitWidth();

        for (auto i = 0u; i < val.getBitWidth(); i += 8)
            memory[addr + i] = val.extractBits(8, i);
    }
} // namespace circ::run
