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

namespace circ::exalt
{
    struct isem_lifter_base;

    struct simple_unit_decoder : decoder_base
    {
        using base = decoder_base;

        values_t _decoder_checks;
        value_t _unit_decoder = nullptr;

        using base::base;

        /* component_base */

        auto init( unit_t &unit ) -> exalted_value_buckets override
        {
            auto &irb = b_ctx.irb();
            for ( auto &atom : unit )
                _decoder_checks.push_back( build::AtomDecoder( irb, atom ).get_decoder_tree() );

            auto combined = irops::Or::make( irb, _decoder_checks );
            _unit_decoder = irops::DecoderResult::make( irb, combined );

            // We need decoder infomarion in the context.
            exalted_value_buckets out;
            out[ place::ctx ].emplace( _unit_decoder );
            return out;
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
        exalted_value_buckets init( unit_t & ) override;

        bool is_persistent() const override { return true; }
    };
}  // namespace circ
