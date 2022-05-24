/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <deque>
#include <map>

#include <circuitous/IR/Visitors.hpp>

#include <circuitous/Support/Check.hpp>

namespace circ
{
    struct Trace
    {
        static inline auto parse_map_comparator = [](const auto &lhs, const auto &rhs)
        {
            return std::less< std::string >{}(lhs->name(), rhs->name());
        };

        // [ from, size, raw_name ]
        using field_t = std::tuple< uint32_t, uint32_t, std::string >;
        // We need persistent storage, because we are going to take pointers to it
        using storage_t = std::deque< field_t >;
        using parse_map_t = std::map< Operation *, field_t *, decltype(parse_map_comparator) >;

        storage_t storage;
        parse_map_t parse_map;


        uint32_t total_size = 0;

        Trace(std::map< std::string, std::vector< Operation * > > &&from, uint32_t total_size_)
            : total_size(total_size_)
        {
            uint32_t current = 0;
            for (auto &[name, ops] : from)
            {
                auto size = ops[0]->size;
                storage.emplace_back(current, size, std::move(name));
                for (auto op : ops)
                {
                    check(size == op->size);
                    parse_map[op] = &storage.back();
                }
                current += size;
            }
        }

        template< typename Derived >
        struct BuilderBase : Visitor< Derived >
        {
            std::map< std::string, std::vector< Operation * > > fields;
            uint32_t total_size;

            Derived &add(Operation *op) { this->dispatch(op); return *this; }

            void visit(Operation *op) { unreachable() << "..."; }

            Trace take() { return Trace(std::move(fields), std::move(total_size)); }
        };

        struct Builder : BuilderBase< Builder >
        {
            using parent_t = BuilderBase< Builder >;
            using parent_t::visit;

            // TODO(lukas): This can be synthetized, see how Serializer does that,
            //              but not sure if it is worth.
            void visit(InputRegister *op)        { add_entry(op->parent_t::name(), op); }
            void visit(InputErrorFlag *op)       { add_entry(op->parent_t::name(), op); }
            void visit(InputTimestamp *op)       { add_entry(op->parent_t::name(), op); }

            void visit(OutputRegister *op)        { add_entry(op->parent_t::name(), op); }
            void visit(OutputErrorFlag *op)       { add_entry(op->parent_t::name(), op); }
            void visit(OutputTimestamp *op)       { add_entry(op->parent_t::name(), op); }

            void visit(InputInstructionBits *op) { add_entry(op->name(), op); }
            void visit(Advice *op)               { add_entry(op->name(), op); }
            void visit(Memory *op)               { add_entry(op->name(), op); }

            void add_entry(const std::string &name, Operation *op)
            {
                this->fields[name].push_back(op);
                this->total_size += op->size;
            }

            template< typename Op >
            void _add_all(Circuit *circuit)
            {
                for (auto op : circuit->attr< Op >())
                    this->dispatch(op);
            }

            template< typename T, typename ... Ts >
            Builder &add_all(Circuit *circuit)
            {
                _add_all< T >(circuit);
                if constexpr (sizeof ... (Ts) != 0)
                    return add_all< Ts ... >(circuit);
                return *this;
            }

            template< typename ... Ts >
            Builder &add_all(Circuit *circuit, tl::TL< Ts ... >)
            {
                return add_all< Ts ... >(circuit);
            }
        };

        static Trace make(Circuit *circuit)
        {
            return Builder().add_all(circuit, input_leaves_ts{})
                            .add_all(circuit, output_leaves_ts{})
                            .take();
        }
    };
}// namespace circ
