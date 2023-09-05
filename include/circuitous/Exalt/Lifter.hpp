/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Lifter/Context.hpp>

#include <circuitous/Exalt/States.hpp>
#include <circuitous/Exalt/Components.hpp>
#include <circuitous/Exalt/UnitComponents.hpp>

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

namespace circ
{
    template< typename U, typename BuilderCtx >
    struct unit_lifter
    {
        using unit_t = U;
        using builder_ctx_t = BuilderCtx;

        builder_context &b_ctx;

        // TODO( exalt ): Change to only work on `&&` of `unit_lifter` if an internal
        //                state is introduced.
        // TODO( exalt ): Figure out return type.
        void exalt( unit_t unit );
    };

    // Owns `builder_context`
    struct circuit_producer : has_ctx_ref
    {
        using base = has_ctx_ref;
        using unit_t = Unit< Atom >;

        builder_context b_ctx;

        circuit_producer( CtxRef ctx_ref )
            : base( ctx_ref ),
              b_ctx( ctx_ref )
        {}

        void exalt( unit_t &unit );
        void finalize();

        auto take_fn() &&
        {
            return std::move( b_ctx ).take_fn();
        }
    };
}  // namespace circ
