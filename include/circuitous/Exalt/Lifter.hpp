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
#include <circuitous/Exalt/UnitComponents.hpp>

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

namespace circ
{
    struct unit_lifter
    {
        builder_context &b_ctx;
        unit_components local_components;

        unit_lifter( builder_context &b_ctx, unit_components &pucs )
            : b_ctx( b_ctx ),
              local_components( unit_components::make_default( b_ctx ) )
        {
            local_components.copy_persistent_components( pucs );
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
        unit_components pucs;

        // exalted values gathered by subsequent runs of components/lifters
        exalted_value_buckets exalted_buckets;

        circuit_producer( CtxRef ctx_ref )
            : base( ctx_ref ),
              b_ctx( ctx_ref ),
              pucs( b_ctx )
        {
            init_pucs();
        }

        void exalt( unit_t &unit );
        void finalize();

        auto take_fn() &&
        {
            return std::move( b_ctx ).take_fn();
        }

      protected:
        void init_pucs();
    };
}  // namespace circ
