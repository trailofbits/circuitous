/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Lifter/Context.hpp>

#include <circuitous/Exalt/Value.hpp>
#include <circuitous/Exalt/States.hpp>
#include <circuitous/Exalt/Components.hpp>
#include <circuitous/Exalt/ComponentStorage.hpp>
#include <circuitous/Exalt/OperandSelection.hpp>
#include <circuitous/Exalt/UnitComponents.hpp>
#include <circuitous/Exalt/Syscall.hpp>

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

namespace circ::exalt
{
    struct unit_lifter
    {
        builder_context &b_ctx;
        component_storage local_components;

        unit_lifter( builder_context &b_ctx, component_storage &pcs )
            : b_ctx( b_ctx ),
              local_components( component_storage::make_default( b_ctx ) )
        {
            local_components.copy_persistent_components( pcs );
            for ( const auto &c : local_components )
                for ( auto spawn : c->spawn_on_unit( b_ctx ) )
                    local_components.add( std::move( spawn ) );
        }

        // TODO( exalt ): Change to only work on `&&` of `unit_lifter` if an internal
        //                state is introduced.
        // TODO( exalt ): Figure out return type.
        auto exalt( unit_t &unit ) -> exalted_value_buckets;

        // TODO( exalt ): Is this worth making hookable?
        value_t make_context( value_set_t vs );

      protected:

        /* Some random helpers to avoid accessing internals of things. */

        auto llvm_module() const { return b_ctx.ctx.module(); }
        auto &arch_state() const { return b_ctx.sub_mods.arch_state; }
        auto &irb() const { return b_ctx.fn_ctx.irb(); }

        auto &l_ctx() const { return b_ctx.ctx; }
        auto bw( auto v ) { return l_ctx().bw( v ); }

        static std::string log_prefix() { return "[exalt:unit-lifter]:"; }
    };

    // Owns `builder_context`
    struct circuit_producer : has_ctx_ref
    {
        using base = has_ctx_ref;
        using unit_t = Unit< Atom >;

        builder_context b_ctx;

        // persistent unit components
        component_storage pcs;

        // exalted values gathered by subsequent runs of components/lifters
        exalted_value_buckets exalted_buckets;

        circuit_producer( CtxRef ctx_ref )
            : base( ctx_ref ),
              b_ctx( ctx_ref ),
              pcs( b_ctx )
        {
            init_pcs();
        }

        // TODO( next ): Should be part of ctor?
        template< typename T >
        void add_isem_lifter()
        {
            pcs.emplace< T >().init();
        }

        void add_operand_selector( const auto &worklist )
        {
            TM_cache cache( ctx );
            cache.build_from( worklist );
            component_t allocator = std::make_shared< TM_allocator >( ctx, cache );
            pcs.add( std::move( allocator ) );
        }

        void add_syscalls()
        {
            auto sc = std::make_shared< syscall_component >( b_ctx );
            sc->init();
            pcs.add( std::move( sc ) );
            // To check that syscall registers and state got injected properly.
            log_dbg() << "[exalt]:" << b_ctx.arch_state().to_string();
        }

        void exalt( unit_t &unit );
        void finalize();

        auto take_fn() &&
        {
            return std::move( b_ctx ).take_fn();
        }

      protected:
        void init_pcs();
    };
}  // namespace circ::exalt
