/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Util/TypeList.hpp>
#include <circuitous/IR/IR.hpp>
#include <circuitous/Support/Check.hpp>

namespace circ
{

    template< typename OP >
    struct MaterializedDefList
    {
        DefList< OP > data;

        template< typename Predicate >
        std::size_t remove_if( Predicate &&should_be_removed )
        {
            auto notify_operands = []( auto &&x )
            {
                x->destroy();
            };
            return data.remove_if( std::forward< Predicate >( should_be_removed ),
                                   notify_operands );
        }

        template< typename CB >
        void for_each_operation(CB &&cb)
        {
            for (auto op : this->data)
                cb(op);
        }

        template< typename CB >
        void apply(CB &&cb) { cb(data); }

        auto &attr() { return data; }

        template< typename CB >
        auto match(Operation *op, CB cb)
        {
            if (op->op_code == OP::kind)
                cb(dynamic_cast<OP *>(op));
        }

        template< typename CB >
        auto match_d(uint32_t kind, CB cb) -> decltype( cb( std::declval< OP * >() ) )
        {
            if (kind == OP::kind)
                cb.template operator()< OP >();
        }
    };


    template< typename L > struct Attributes {};

    template< typename ... Ops >
    struct Attributes< tl::TL< Ops ... > > : Ops ...
    {
        template< typename T >
        using parent = MaterializedDefList< T >;

        template< typename T >
        auto &attr() { return this->parent< T >::attr(); }

        template< typename CB >
        void for_each_operation(CB cb) { (this->Ops::for_each_operation(cb), ...); }

        void clear_without_erasure()
        {
            auto clear = [](auto &field)
            {
                for (auto op : field) {
                    op->operands().clear_without_erasure();
                }
            };
            (this->Ops::apply(clear), ...);
        }

        template< typename Predicate >
        std::size_t remove_if( Predicate &&should_be_removed )
        {
            return ( this->Ops::remove_if( should_be_removed ) + ... );
        }

        template< typename CB > auto match(Operation *op, CB cb)
        {
            (this->Ops::match(op, cb), ...);
        }
        template< typename CB > auto match_d(uint32_t k, CB cb)
        {
            (this->Ops::match_d(k, cb), ...);
        }
    };

    struct to_mat_def_list
    {
        template< typename T >
        using type = MaterializedDefList< T >;
    };
    using m_def_lists = tl::apply< all_nodes_list_t, to_mat_def_list >;
    using AllAttributes = Attributes< m_def_lists >;

    // NOTE(lukas): This is not templated - it is not like we are going
    //              to have more in near future and it would just pollute
    //              error messages even further.
    struct CircuitStorage : Attributes< m_def_lists >
    {
        using attrs_t = Attributes< m_def_lists >;
        using attrs_t::for_each_operation;

        uint64_t ids = 0;
        static constexpr inline uint64_t max_id = (1ull >> 60);

        using attrs_t::remove_if;

        template< typename Op, typename Predicate >
        std::size_t remove_if( Predicate &&should_be_removed )
        {
            return attr< Op >().remove_if( std::forward< Predicate >( should_be_removed ) );
        }

        template< typename T >
        auto &attr()
        {
            static_assert(std::is_base_of_v< Operation, T >);
            return this->AllAttributes::attr< T >();
        }

        template< typename T, typename ...Args >
        auto create(Args &&...args)
        {
            auto op = attr< T >().create(std::forward< Args >(args)...);
            op->_id = ++ids;
            return op;
        }

        template< typename ...Args >
        Operation *create(uint32_t kind, Args &&...args)
        {
            Operation *out = nullptr;

            auto create = [&]< typename T >() {
                // Down the line we will invoke `T( std::forward< Args >(args) ... )`
                // and since ctors are not uniform, the invocation may not be well-formed
                // which leads to compile error.
                if constexpr ( std::is_constructible_v< T, Args ... > ) {
                    out = this->create< T >( std::forward< Args >( args ) ... );
                }
            };

            this->match_d( kind, create );
            dcheck( out, [&](){ return "Failed to created Operation!"; });
            return out;
        }

        template< typename T, typename ...Args >
        T* adopt(uint64_t id, Args &&...args)
        {
            auto op = attr< T >().create(std::forward< Args >(args)...);
            op->_id = id;
            ids = std::max(ids, id);
            return op;
        }

        // TODO(lukas) : Return optional to signal failure.
        template< typename What >
        auto fetch_singular()
        {
            auto &all = this->attr< What >();
            check(all.size() == 1) << "Fetch singular did not return 1 element.";
            return all[0];
        }

        template< typename What, bool allow_failure = true >
        auto fetch_reg(const std::string &name) -> What *
        {
            for (auto reg : this->attr<What>())
                if (reg->reg_name == name)
                    return reg;

            if constexpr (!allow_failure) {
                check(false) << "Register " << name << " not present";
            }
            return nullptr;
        }

        auto input_inst_bits() { return fetch_singular< InputInstructionBits >(); }
        auto input_ebit() { return fetch_singular< InputErrorFlag >(); }
        auto output_ebit() { return fetch_singular< OutputErrorFlag >(); }

        auto input_timestamp() { return fetch_singular< InputTimestamp >(); }
        auto output_timestamp() { return fetch_singular< OutputTimestamp >(); }

        using cstr_ref = const std::string &;
        auto input_reg(cstr_ref name) { return fetch_reg< InputRegister >(name); }
        auto output_reg(cstr_ref name) { return fetch_reg< OutputRegister >(name); }
    };
} // namespace circ
