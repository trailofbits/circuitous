/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Memory.hpp>
#include <circuitous/IR/Circuit.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/ADT/APInt.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <cstdint>
#include <optional>
#include <map>
#include <unordered_map>

namespace circ::run
{
    // NOTE(lukas): We could template all the classes, but compile time.
    using raw_value_type = llvm::APInt;
    using value_type = std::optional< llvm::APInt >;

    static inline std::string to_string( value_type v )
    {
        if ( v )
            return llvm::toString( *v, 16, false );
        return "{}";
    }

    struct Memory
    {
        using memory_map_t = std::map< uint64_t, raw_value_type >;

        uint32_t hint_size;
        memory_map_t memory;

        Memory(Circuit *circuit);
        Memory(const Memory &) = default;
        Memory(Memory &&) = default;

        Memory &operator=(const Memory &) = default;
        Memory &operator=(Memory &&) = default;

        memory_map_t take_memory() { return std::move(memory); }

        bool defined(uint64_t addr, std::size_t size) const;

        value_type load(uint64_t addr, std::size_t size_) const;
        void store(uint64_t addr, raw_value_type val);

        using Parsed = irops::memory::Parsed< llvm::APInt >;

        Parsed deconstruct(const llvm::APInt &value) const;
        llvm::APInt construct(const Parsed &parsed) const;

        static Parsed deconstruct(const llvm::APInt &value, std::size_t hint_size);
        static llvm::APInt construct(const Parsed &parsed, std::size_t hint_size);

        std::string to_string() const;
    };

    struct NodeState
    {
        using node_values_t = std::unordered_map< Operation *, value_type >;
        node_values_t node_values;

        bool set(Operation *op, value_type val);
        value_type get(Operation *op) const;

        bool has_value(Operation *op) const
        {
            return node_values.count(op);
        }

        auto take() { return std::move(node_values); }

        std::string to_string() const;

        gap::generator< NodeState > permutate_memory( circuit_ref_t circuit );
    };

    struct StateOwner
    {
        virtual ~StateOwner() = default;

        virtual void set_node_val(Operation *, const value_type &) = 0;
        virtual value_type get_node_val(Operation *) const = 0;
        virtual bool has_value(Operation *) const = 0;

        virtual void store(uint64_t addr, const raw_value_type &data) = 0;
        virtual value_type load(uint64_t addr, std::size_t size) const = 0;
        virtual bool defined(uint64_t addr, std::size_t size) const = 0;
    };

    // TODO(lukas): May be too simple now, bordering useless bolierplate.
    struct NodeStateBuilder
    {
        using self_t = NodeStateBuilder;

      private:
        NodeState node_state;
        Circuit *circuit;

      public:
        NodeStateBuilder(Circuit *circuit) : circuit(circuit) {}

        auto take() { return std::move(node_state); }

        template< typename MapLike >
        self_t &set(const MapLike &mapping)
        {
            for (const auto &[op, val]: mapping)
                node_state.set(op, val);
            return *this;
        }

        // Set all operations of type `T` to value `v` (can be empty value - for example to
        // model undefined values).
        template< typename T >
        self_t &all(const value_type &v)
        {
            for (auto op : circuit->attr< T >())
                node_state.set(op, v);
            return *this;
        }

        template< typename T >
        self_t &all( uint64_t val )
        {
            // We have no guarantees? that they are all the same size.
            for ( auto op : circuit->attr< T >() )
            {
                auto cooked = llvm::APInt( op->size, val, false );
                node_state.set( op, cooked );
            }
            return *this;
        }

        self_t &fill_memory()
        {
            for ( auto memory_op : circuit->attr< ::circ::Memory >() )
            {
                if ( node_state.has_value( memory_op ) )
                    continue;

               llvm::APInt val { irops::memory::size( this->circuit->ptr_size ), 0, false };
               // TODO( run ): Extract to some helper fn.
               val.insertBits( llvm::APInt( 4, memory_op->mem_idx, false ), 8 );
               node_state.set( memory_op, val );
            }
            return *this;
        }
    };

    // TODO(lukas): May be too simple now, bordering useless bolierplate.
    struct MemoryBuilder
    {
        using self_t = MemoryBuilder;
      private:
        Memory memory;

      public:
        MemoryBuilder(Circuit *circuit) : memory(circuit) {}

        auto take() { return std::move(memory); }
        self_t &set(std::size_t addr, const std::string &val);
        self_t &set(std::size_t addr, const value_type &value);
    };


} // namespace circ::run
