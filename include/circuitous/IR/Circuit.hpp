/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Storage.hpp>

namespace circ
{
    struct Circuit : CircuitStorage
    {
        using ptr_size_t = uint32_t;

        // TODO(lukas): Will be deprecated in the future.
        explicit Circuit( ptr_size_t ptr_size=64 ) : ptr_size( ptr_size ) {}

        Operation *root = nullptr;
        ptr_size_t ptr_size;
    };

    // Owner of Circuit, in case non-owning reference is desired, use raw pointer.
    using circuit_owner_t = std::unique_ptr< Circuit >;
    using circuit_ref_t = Circuit *;

    // TODO(lukas): Generalise comparator and move to `tl::`.
    template< typename F, typename H, typename ... Tail >
    auto dispatch_on_kind( Operation::kind_t rkind, F &&f )
    {
        if ( H::kind == rkind )
            return f.template operator()< H >();

        if constexpr ( sizeof ... ( Tail ) != 0 )
            return dispatch_on_kind< F, Tail ... >( rkind, f );
        else
            unreachable() << "runtime find on: "
                          << std::to_string( util::to_underlying( rkind ) )
                          << " failed";
    }

    template< typename F, typename ... Ts >
    auto dispatch_on_kind( tl::TL< Ts ... >, Operation::kind_t rkind, F &&f )
    {
        return dispatch_on_kind< F, Ts ... >( rkind, std::forward< F >( f ) );
    }

    template< typename F >
    auto dispatch_on_kind_to_all( Operation::kind_t kind, F &&f )
    {
        return dispatch_on_kind( all_nodes_list_t{}, kind, std::forward< F >( f ) );
    }

    static inline std::string op_code_str(Operation::kind_t rkind)
    {
        auto get_name = []< typename  Op >() {
            return Op::op_code_str();
        };

        static std::unordered_map< Operation::kind_t, std::string > cache;
        if (!cache.count(rkind))
            cache[rkind] = dispatch_on_kind_to_all(rkind, get_name);

        return cache[rkind];
    }

    template< bool with_meta = true >
    static inline std::string pretty_print(const Operation *op)
    {
        std::stringstream ss;
        ss << op->id() << ": " << op_code_str(op->op_code);
        if constexpr (with_meta)
        {
            if (op->meta_size())
                ss << std::endl << op->dump_meta();
        }
        return ss.str();
    }

} // namespace circ
