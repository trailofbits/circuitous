/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Exalt/Components.hpp>
#include <circuitous/Exalt/Interfaces.hpp>

#include <circuitous/Lifter/Instruction.hpp>
#include <circuitous/Lifter/Components/Decoder.hpp>

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>


#include <tuple>
#include <type_traits>

namespace circ
{
    struct isem_lifter_base;

    struct simple_unit_decoder : decoder_base
    {
        using base = decoder_base;

        values_t _decoder_checks;
        value_t _unit_decoder = nullptr;

        using base::base;

        /* component_base */

        void init( unit_t &unit ) override
        {
            auto &irb = b_ctx.irb();
            for ( auto &atom : unit )
                _decoder_checks.push_back( build::AtomDecoder( irb, atom ).get_decoder_tree() );

            auto combined = irops::Or::make( irb, _decoder_checks );
            _unit_decoder = irops::DecoderResult::make( irb, combined );
        }

        /* decoder_base */
        const values_t &atom_decoders() const override
        {
            return _decoder_checks;
        }

        value_t unit_decoder() const override
        {
            return _unit_decoder;
        }
    };

    // Replaces the remill memory intrinsics with appropriate circ intrinsics.
    struct memory_checks : uc_with_b_ctx
    {
        using base = uc_with_b_ctx;
        using base::base;

        exalted_values_t after_isem( unit_t &unit, isem_range_t isem_range ) override;
    };

    struct error_bit : uc_with_b_ctx
    {
        using base = uc_with_b_ctx;
        using base::base;

        exalted_values_t after_isem( unit_t &unit, isem_range_t isem_range ) override;
    };

    struct timestamp : uc_with_b_ctx
    {
        using base = uc_with_b_ctx;
        using base::base;

        // For now holds only one check that the timestamp is bumped properly.
        exalted_values_t checks;

        exalted_values_t after_isem( unit_t &unit, isem_range_t isem_range ) override;
        void init() override;

        bool is_persistent() const override { return true; }
    };

    struct unit_components
    {
        using components_t = std::vector< unit_component_t >;

        builder_context &b_ctx;
        components_t components;

        unit_components( builder_context &b_ctx )
            : b_ctx( b_ctx )
        {}

        unit_components( builder_context &b_ctx, components_t &&components )
            : b_ctx( b_ctx ), components( std::move( components ) )
        {}

        unit_components( builder_context &b_ctx, const unit_components &other )
            : b_ctx( b_ctx )
        {
            copy_persistent_components( other );
        }

        void copy_persistent_components( const unit_components &other )
        {
            for ( auto &c : other.components )
                if ( c->is_persistent() )
                    // We are using `shared_ptr` so we can copy and
                    // *not* destroy `other`.
                    components.push_back( c );
        }

        /* Access components */

        template< typename T >
        T &fetch_or_die()
        {
            for ( auto &c : components )
                if ( auto casted = dynamic_cast< T * >( c.get() ) )
                    return *casted;
            log_kill() << "Could not fetch desired interface from unit_components.";
        }

        auto get_decoder() -> decoder_base &
        {
            return fetch_or_die< decoder_base >();
        }

        auto get_isem_lifter() -> isem_lifter_base &
        {
            return fetch_or_die< isem_lifter_base >();
        }


        /* Construction */

        template< is_unit_component T >
        unit_component_base &emplace()
        {
            components.push_back( std::make_shared< T >( b_ctx ) );
            return *components.back();
        }

        template< typename ... Ts >
        static auto make( builder_context &b_ctx )
        {
            auto self = unit_components( b_ctx );
            ( self.emplace< Ts >(), ... );
            return self;
        }

        static auto make_default( builder_context &b_ctx )
        {
            return make< simple_unit_decoder, memory_checks, error_bit >( b_ctx );
        }

        exalted_values_gen_t after_isem( unit_t &unit, isem_range_t isem_range )
        {
            for ( auto &c : components )
                for ( auto v : c->after_isem( unit, isem_range ) )
                    co_yield v;
        }
    };


}  // namespace circ
