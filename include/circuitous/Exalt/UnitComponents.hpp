/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Exalt/Components.hpp>

#include <circuitous/Lifter/Instruction.hpp>
#include <circuitous/Lifter/Components/Decoder.hpp>

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>


#include <tuple>
#include <type_traits>

namespace circ
{
    // Can we use this as virtual base or just a tag?
    struct unit_component_base
    {
        builder_context &b_ctx;

        unit_component_base( builder_context &b_ctx )
            : b_ctx( b_ctx )
        {}

        virtual ~unit_component_base() = default;

        virtual void init( unit_t & ) {}
        virtual exalted_value_t on_isem( isem_range_t ) { return {}; };
    };

    template< typename T >
    concept is_unit_component = std::is_base_of_v< T, unit_component_base >;

    using unit_component_t = std::unique_ptr< unit_component_base >;

    template< typename U >
    struct unit_decoder : unit_component_base
    {
        using base = unit_component_base;
        using unit_t = U;

        values_t atom_decoders;

        using base::base;

        void init( unit_t &unit ) override
        {
            auto &irb = b_ctx.irb();
            for ( auto &atom : unit )
                atom_decoders.push_back( build::AtomDecoder( irb, atom ).get_decoder_tree() );
        }
    };

    // Replaces the remill memory intrinsics with appropriate circ intrinsics.
    struct memory_checks : unit_component_base
    {
        using base = unit_component_base;
        using base::base;

    };

    // Replace all undefs with values or die trying.
    struct undef_resolver : unit_component_base
    {
        using base = unit_component_base;
        using base::base;
    };

    struct unit_components
    {
        builder_context &b_ctx;
        std::vector< unit_component_t > components;

        template< typename ... Ts >
        unit_components( builder_context &b_ctx )
            : b_ctx( b_ctx )
        {
            ( emplace< Ts >(), ... );
        }

        template< is_unit_component T >
        unit_component_base &emplace()
        {
            return components.emplace_back( std::make_unique< T >( b_ctx ) );
        }
    };

}  // namespace circ
