/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Exalt/Interfaces.hpp>
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
    struct mux_heavy_lifter : isem_lifter_base
    {
        using base = isem_lifter_base;
        using base::base;

        // TODO( exalt ): This is temporary glue to keep old version working.
        //                Figure out how to deal with it.
        using stores_t = std::vector< llvm::StoreInst * >;
        using reg_to_vals = std::unordered_map< std::string, values_t >;
        using parsed_writes_t = std::map< std::size_t, std::tuple< stores_t, reg_to_vals > >;

        using cond_to_value_t = std::tuple< llvm::Value *, llvm::Value * >;
        // { reg, [ ( condition, value of that reg if condition holds ) ] }
        using reg_final_values_t = std::map< reg_ptr_t, std::vector< cond_to_value_t > >;

        parsed_writes_t parsed_writes;
        reg_final_values_t final_values;

        using lifted_operands_t = values_t;
        using writes_t = std::vector< std::tuple< llvm::Instruction *, std::size_t > >;

        /* `isem_lifter_base` interface */

        auto make_semantic_call( unit_t &unit, decoder_base &decoder,
                                 semantic_fn_t isem )
            -> isem_range_t override;

        auto finalize_circuit() -> exalted_value_buckets override;

        /* Local logic */

        void bump_pc( unit_t &unit, decoder_base &decoder );

        // TODO( exalt ): Have `unit` as attribute?
        auto get_operands( unit_t &unit, decoder_base &decoder, semantic_fn_t isem )
            -> std::tuple< lifted_operands_t, writes_t >;

        auto get_make_breakpoint()
        {
            return []( auto irb )
            {
                return irops::Breakpoint::make( irb, irb.getTrue() );
            };
        }

        // TODO( exalt ): Should these be part of a component?
        void parse_writes( unit_t &unit, decoder_base &decoder, writes_t );
        reg_to_vals write_conditions( unit_t &unit, decoder_base &decoder, std::size_t idx );
        stores_t stores_to( llvm::Instruction *v );

        void gather_final_values( unit_t &unit,
                                  decoder_base &decoder,
                                  const parsed_writes_t &writes );

        value_t reg_check( reg_ptr_t reg );

        /* Random helpers */

        auto &irb() { return b_ctx.irb(); }
        auto &arch_state() { return b_ctx.arch_state(); }
        auto &l_ctx() { return b_ctx.ctx; }
        auto bw( auto v ) { return l_ctx().bw( v ); }

        auto log_prefix() { return "[exalt:mux_heavy_lifter]:"; }
    };
}  // namespace circ
