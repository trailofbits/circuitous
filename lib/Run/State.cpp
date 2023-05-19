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


        for (auto i = 0u; i < val.getBitWidth() / 8; ++i)
            memory[addr + i] = val.extractBits(8, i * 8);
    }

    auto Memory::deconstruct(const llvm::APInt &value) const -> Parsed
    {
        return deconstruct(value, hint_size);
    }

    llvm::APInt Memory::construct(const Parsed &parsed) const
    {
        return construct(parsed, hint_size);
    }


    auto Memory::deconstruct(const llvm::APInt &value, std::size_t hint_size) -> Parsed
    {
        uint32_t casted_hint_size = static_cast< uint32_t >(hint_size);
        auto extractor = [](auto thing, auto from, auto size) -> llvm::APInt {
            return thing.extractBits(size, from);
        };
        check(value.getBitWidth() == irops::memory::size(casted_hint_size));
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

    std::string Memory::to_string() const
    {
        std::stringstream ss;
        ss << std::hex;

        ss << "Memory: [ addr ] := byte\n";
        for ( const auto &[ addr, val ] : memory )
            ss << "\t[ " << addr << "] := " << llvm::toString( val, 16, false ) << "\n";

        return ss.str();
    }

    bool NodeState::set(Operation *op, value_type value)
    {
        if (has_value(op))
            return false;
        node_values[op] = value;
        return true;
    }

    auto NodeState::get(Operation *op) const -> value_type
    {
        // TODO(lukas): Yield error instead.
        check(has_value(op), [&](){
            return pretty_print(op) + " does not have value.";
        });
        return node_values.find(op)->second;
    }

    auto NodeState::permutate_memory( circuit_ref_t circuit ) -> gap::generator< NodeState >
    {
        std::vector< raw_value_type > pool;

        for ( auto memory_op : circuit->attr< ::circ::Memory >() )
            pool.emplace_back( *this->get( memory_op ) );


        log_dbg() << "[run:NodeState]:" << "Permutating" << pool.size() << "memory hints";
        auto cmp = []( const auto &a, const auto &b )
        {
            return a.ult( b );
        };

        std::sort( pool.begin(), pool.end(), cmp );
        do
        {
            auto out = *this;

            std::size_t idx = 0;
            for ( auto memory_op : circuit->attr< ::circ::Memory >() )
            {
                auto val = pool[ idx++ ];
                // TODO( run ): Extract to some helper fn.
                val.insertBits( llvm::APInt( 4, memory_op->mem_idx, false ), 8 );

                out.set( memory_op, val );
            }
            co_yield std::move( out );
        } while ( std::next_permutation( pool.begin(), pool.end(), cmp ) );

    }

    std::string NodeState::to_string() const
    {
        std::stringstream ss;

        auto id = []( auto o )
        {
            return "[" + std::to_string( o->id() ) + "] " + "{ " + op_code_str( o->op_code ) + " }";
        };
        auto fmt = []( auto v ) -> std::string
        {
            if ( !v ) return "[ NOT SET ]";
            return llvm::toString( *v, 16, false );
        };

        for ( auto &[ op, val ] : node_values )
        {
            ss << "Value: " << pretty_print( op ) << std::endl
               << id( op ) << " := " << fmt( val ) << std::endl;
            for ( auto o : op->operands() )
            {
                ss << "\t" << id( o ) << " <- " << fmt( get( o ) ) << std::endl;
            }
        }
        return ss.str();
    }

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
} // namespace circ::run
