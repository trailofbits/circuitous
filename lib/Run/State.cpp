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

    bool Memory::defined(uint64_t addr, std::size_t size) const
    {
        for (auto i = 0u; i < size; ++i)
            if (!memory.count(addr + i))
                return false;
        return true;
    }

    auto Memory::load(uint64_t addr, std::size_t size) const -> value_type
    {
        if (!defined(addr, size))
            return {};

        llvm::APInt build{ static_cast< uint32_t >(size * 8), 0, false };
        for (auto i = 0u; i < size; ++i)
            build.insertBits( memory.find(addr + i)->second, i * 8 );

        return build;
    }

    void Memory::store(uint64_t addr, raw_value_type val)
    {
        check( val.getBitWidth() % 8 == 0 )
            << "Cannot store val that has unalinged bw such as " << val.getBitWidth();

        for (auto i = 0u; i < val.getBitWidth(); i += 8)
            memory[addr + i] = val.extractBits(8, i);
    }

    auto Memory::deconstruct(const llvm::APInt &value) -> Parsed
    {
        return deconstruct(value, hint_size);
    }

    llvm::APInt Memory::construct(const Parsed &parsed)
    {
        return construct(parsed, hint_size);
    }


    auto Memory::deconstruct(const llvm::APInt &value, std::size_t hint_size) -> Parsed
    {
        uint32_t casted_hint_size = static_cast< uint32_t >(hint_size);
        auto extractor = [](auto thing, auto from, auto size) -> llvm::APInt {
            return thing.extractBits(size, from);
        };
        check(value.getBitWidth() == casted_hint_size);
        return irops::memory::parse< llvm::APInt >(value, extractor, hint_size);
    }

    llvm::APInt Memory::construct(const Parsed &parsed, std::size_t hint_size)
    {
        auto mem_size = irops::memory::size(static_cast< uint32_t >(hint_size));
        llvm::APInt out { mem_size, 0, false };
        auto inserter_ = [&](auto thing, auto from, auto size) {
            check(size == thing.getBitWidth());
            out.insertBits(thing, from);
        };
        irops::memory::construct(parsed, inserter_);
        return out;
    }
} // namespace circ::run
