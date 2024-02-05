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
    // Provides information about how the blob is parsed to circuit operations.
    // Each instance is tied to a specific instance of `Circuit` objects both in lifetime
    // and correctness -> two different circuits can have different traces; consider a case
    // of circuits with different number of `Advice` nodes.
    // This is a representation of one state during program execution.
    struct Trace
    {
        static inline auto parse_map_comparator = [](const auto &lhs, const auto &rhs)
        {
            return std::less< std::string >{}(lhs->name(), rhs->name());
        };

        static inline auto field_ptr_comparator = [](const auto &lhs, const auto &rhs)
        {
            check(lhs && rhs);
            return std::less< uint32_t >{}(std::get< 0 >(*lhs), std::get< 0 >(*rhs));
        };

        // [ from, size, raw_name ]
        using field_t = std::tuple< uint32_t, uint32_t, std::string >;
        // We need persistent storage, because we are going to take pointers to it
        using storage_t = std::deque< field_t >;
        // Multiple operations can point to same field - nodes that have both input and output
        // variants will be such case (`OutputRegister`, `InputRegister` for example.
        using parse_map_t = std::map< Operation *, field_t *, decltype(parse_map_comparator) >;

        using field_map_t = std::unordered_map< field_t *, std::unordered_set< Operation * > >;

        storage_t storage;
        parse_map_t parse_map;
        field_map_t field_map;

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
                    field_map[&storage.back()].insert(op);
                }
                current += size;
            }
        }

        // TODO(lukas): Because internally raw pointers are used, this class cannot be copied.
        //              Rework so it can, as there will most likely be use cases for it.
        Trace(const Trace &) = delete;
        Trace(Trace &&) = default;

        Trace &operator=(const Trace &) = delete;
        Trace &operator=(Trace &&) = default;

        std::string to_string() const
        {
            // Order fields by index
            std::map< uint32_t, std::tuple< uint32_t, std::string > > sorted;
            // Group all operations that point to same index (therefore the same field)
            std::map< uint32_t, std::vector< Operation * > > op_to_fields;
            for (const auto &[x, y, z] : storage)
                sorted[x] = std::make_tuple(y, z);
            for (const auto &[op, field] : parse_map)
                op_to_fields[std::get< 0 >(*field)].push_back(op);

            std::stringstream ss;
            for (const auto &[from, tail] : sorted)
            {
                const auto &[size, raw_name] = tail;
                ss << "[ " << from << ", " << size << " ] " << raw_name << std::endl;
                for (auto op : op_to_fields[from])
                    ss << "\t| " << pretty_print< false >(op) << std::endl;
            }
            return ss.str();
        }

        std::optional< field_t * > fetch_field(auto &&match)
        {
            for (auto &[op, field] : parse_map)
                if (match(op->name()))
                    return { field };
            return {};
        }


        template< typename Derived >
        struct BuilderBase : Visitor< Derived >
        {
            std::map< std::string, std::vector< Operation * > > fields;
            uint32_t total_size;

            void visit(Operation *op) { unreachable() << "..."; }

            Trace take() { return Trace(std::move(fields), std::move(total_size)); }

            template< typename Op >
            void _add_all(Circuit *circuit)
            {
                for (auto op : circuit->attr< Op >())
                    this->dispatch(op);
            }

            template< typename T, typename ... Ts >
            Derived &add_all(Circuit *circuit)
            {
                _add_all< T >(circuit);
                if constexpr (sizeof ... (Ts) != 0)
                    return add_all< Ts ... >(circuit);
                else
                    return static_cast< Derived & >(*this);
            }

            template< typename ... Ts >
            Derived &add_all(Circuit *circuit, tl::TL< Ts ... >)
            {
                return add_all< Ts ... >(circuit);
            }
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
            void visit(InputSyscallReg *op)      { add_entry(op->parent_t::name(), op); }
            void visit(InputSyscallState *op)    { add_entry(op->parent_t::name(), op); }

            void visit(OutputRegister *op)        { add_entry(op->parent_t::name(), op); }
            void visit(OutputErrorFlag *op)       { add_entry(op->parent_t::name(), op); }
            void visit(OutputTimestamp *op)       { add_entry(op->parent_t::name(), op); }
            void visit(OutputSyscallReg *op)      { add_entry(op->parent_t::name(), op); }
            void visit(OutputSyscallState *op)    { add_entry(op->parent_t::name(), op); }

            void visit(InputInstructionBits *op) { add_entry(op->name(), op); }
            void visit(Advice *op)               { add_entry(op->name(), op); }
            void visit(Memory *op)               { add_entry(op->name(), op); }

            void add_entry(const std::string &name, Operation *op)
            {
                this->fields[name].push_back(op);
                this->total_size += op->size;
            }

        };

        static Trace make(Circuit *circuit)
        {
            return Builder().add_all(circuit, input_leaves_ts{})
                            .add_all(circuit, output_leaves_ts{})
                            .take();
        }
    };

    // Each field can have associated value of type `V`.
    template< typename V >
    struct ValuedTrace : Trace
    {
        using parent_t = Trace;
        using self_t = ValuedTrace< V >;

        using field_t = parent_t::field_t;

        using value_type = V;
        using mapping_t = std::map< field_t *, V, decltype(parent_t::field_ptr_comparator) >;

        mapping_t field_state;

        // NOTE(lukas): Match is done by looking at suffixes - ie `RAX` will match both
        //              `OutputRegister` and `InputRegister`. For now this is fine, but it
        //              can happen later that this will require some rework.
        ValuedTrace(Trace &&trace,
                    const std::map< std::string, V > &value_mapping)
            : parent_t(std::move(trace))
        {
            for (const auto &[name, v] : value_mapping)
            {
                auto field = fetch_field(suffix_match(name));

                check( field ) << "Could not fetch field:" << name;
                check(!field_state.count(*field)) << name;

                field_state[*field] = v;
            }
        }

        static auto suffix_match(std::string to_match)
        {
            return [str = std::move(to_match)](const std::string &other)
            {
                return llvm::StringRef(other).consume_back_insensitive(str);
            };
        }

        std::string to_string() const
        {
            std::stringstream ss;
            ss << parent_t::to_string();

            for (const auto &[field, val] : field_state)
            {
                // TODO(lukas): Get rid of llvm::APInt assumption.
                ss << std::get< 0 >(field) << ": " << toString(val, 16, false) << std::endl;
            }
        }

        // For given `circuit` and operation types `Ts ...` return back mapping of those
        // operation to values of fields they are pointing at.
        // In case it is attempted to use one field twice, hard error is emitted.
        template< typename ... Ts >
        std::unordered_map< Operation *, V > specialize(Circuit *circuit, tl::TL< Ts ... >)
        {
            std::unordered_map< Operation *, V > out;
            std::unordered_set< field_t * > seen;
            fixate_fields< Ts ... >(circuit, out, seen);
            return out;
        }

        template< typename H, typename ... Tail >
        void fixate_fields(Circuit *circuit,
                           std::unordered_map< Operation *, V > &fixated,
                           std::unordered_set< field_t * > &seen)
        {
            for (auto op : circuit->template attr< H >())
            {
                auto field = parse_map[op];
                check(!seen.count(field));
                seen.insert(field);
                if (auto it = field_state.find(field); it != field_state.end())
                    fixated[op] = it->second;
            }

            if constexpr (sizeof ... (Tail) != 0)
                return fixate_fields< Tail ... >(circuit, fixated, seen);
        }
    };

}// namespace circ
