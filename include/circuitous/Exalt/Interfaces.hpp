/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Exalt/Common.hpp>
#include <circuitous/Exalt/Value.hpp>

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <type_traits>

namespace circ::exalt
{
    // Universal tag for storage purposes.
    struct component_base
    {
        virtual ~component_base() = default;

        // Is this per function or per unit only?
        // Allows to have state in the function or caching (and initializes only once?)
        virtual bool is_persistent() const { return false; }

        // Global init, so far happens only for `persistent` components.
        // TODO( exalt ): Pull out into separate interface?
        virtual void init() {}

        // Local init per `unit`.
        virtual exalted_value_buckets init( unit_t & ) { return {}; }
    };

    // Provides hooks to interpose on lifting process.
    struct unit_component_base : component_base
    {
        virtual exalted_values_t after_isem( unit_t &unit, isem_range_t ) { return {}; };

        exalted_values_t wrap_as_exalted( auto &&range, place p )
        {
            exalted_values_t out;
            for ( auto v : range )
                out.emplace_back( p, v );
            return out;
        }
    };

    template< typename T >
    concept is_unit_component = std::is_base_of_v< unit_component_base, T >;

    // To ease usage w.r.t. persistent components. There is nothing preventing us from
    // having this as `unique_ptr` and passing them around.
    using component_t = std::shared_ptr< component_base >;

    // Forward declare to avoid include hell.
    struct builder_context;

    struct uc_with_b_ctx : unit_component_base
    {
        builder_context &b_ctx;

        uc_with_b_ctx( builder_context &b_ctx )
            : b_ctx( b_ctx )
        {}
    };

    struct decoder_base : uc_with_b_ctx
    {
        using base = uc_with_b_ctx;
        using base::base;

        // TODO( exalt ): Do we need more?
        virtual const values_t &atom_decoders() const = 0;
        auto begin() const { return atom_decoders().begin(); }
        auto end()   const { return atom_decoders().end(); }

        virtual value_t unit_decoder() const = 0;
    };

    struct component_storage;

    // Responsible for lifting semantic function in a "circuit-like" manner.
    struct isem_lifter_base : uc_with_b_ctx
    {
        using base = uc_with_b_ctx;
        using base::base;

        // TODO( next ): Will most likely need some state to store info discovered during
        //               `make_semantic_call`.
        // TODO( exalt ): Should `requested` be part of this api? If not should it be
        //                a member?
        virtual isem_range_t make_semantic_call( unit_t &unit,
                                                 component_storage &ucs,
                                                 semantic_fn_t isem ) = 0;

        // Return the final result.
        // This allows more freedom on how things will be wired and connected.
        virtual value_t finalize_circuit(exalted_value_buckets) = 0;

        // TODO( exalt ): Is this required?
        bool is_persistent() const override { return true; }
    };

}  // namespace circ::exalt
