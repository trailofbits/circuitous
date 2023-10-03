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

namespace circ::exalt
{
    /* A bunch of shared helper functionality to be used by isem_lifters.
     * TODO( exalt ): Add as base class? Hide in `.cpp`?
     */

    // For now it is a separate class so we can hide some internal type
    // aliases.
    // Not opting for CTRP as I want implementation in the `.cpp`.
    struct isem_lifter_utilities
    {
        virtual ~isem_lifter_utilities() = default;

      protected:
        /* Lifting helpers */

        // Due to how remill semantics work, this is supposed to happen *before*
        // actual call to semantics.
        void bump_pc( unit_t &, decoder_base &);

        // `[ ( irops alloca, idx of the operand in instruction ) ]`
        using writes_t = std::vector< std::tuple< llvm::Instruction *, std::size_t > >;
        using lifted_operands_t = values_t;

        // Lift operand using stateless requester.
        auto get_operands( unit_t &, decoder_base &, semantic_fn_t )
            -> std::tuple< lifted_operands_t, writes_t >;

        using stores_t = std::vector< llvm::StoreInst * >;

        // Collect all stores to particular pointer.
        // TODO( exalt ): Pass in `irops::Instance` to be sure this is a destination?
        stores_t stores_to( llvm::Instruction *v );

        // Create dummy breakpoint llvm instruction to serve as boundary of inlined
        // LLVM semantic.
        auto get_make_breakpoint()
        {
            return []( auto irb )
            {
                return irops::Breakpoint::make( irb, irb.getTrue() );
            };
        }

        // Register name to vector of condition under which they are written to.
        using reg_to_vals = std::unordered_map< std::string, values_t >;

        // Collect for given `unit.slice( idx )` what register can be written and
        // if they are, what is the condition that needs to be satisfied.
        reg_to_vals write_conditions( unit_t &unit, decoder_base &decoder, std::size_t idx );

        using parsed_writes_t = std::map< std::size_t, std::tuple< stores_t, reg_to_vals > >;
        parsed_writes_t parse_writes( unit_t &unit, decoder_base &decoder, writes_t );

        using cond_to_value_t = std::tuple< llvm::Value *, llvm::Value * >;
        // { reg, [ ( condition, value of that reg if condition holds ) ] }
        using reg_final_values_t = std::map< reg_ptr_t, std::vector< cond_to_value_t > >;

        auto gather_final_values( unit_t &unit,
                                  decoder_base &decoder,
                                  const parsed_writes_t &writes ) -> reg_final_values_t;

        auto reg_check( reg_ptr_t reg, const reg_final_values_t & ) -> value_t;
        auto reg_check( reg_ptr_t reg, const std::vector< cond_to_value_t > & ) -> value_t;

        /* Accessor shortcuts */

        auto &irb() { return get_b_ctx().irb(); }
        auto &arch_state() { return get_b_ctx().arch_state(); }
        auto &l_ctx() { return get_b_ctx().ctx; }
        auto bw( auto v ) { return l_ctx().bw( v ); }
        auto mem_ptr() { return get_b_ctx().mem_ptr(); }

      protected:
        virtual builder_context &get_b_ctx() = 0;
    };

    struct mux_heavy_lifter : isem_lifter_base, isem_lifter_utilities
    {
        using base = isem_lifter_base;
        using base::base;

        using reg_final_values_t = isem_lifter_utilities::reg_final_values_t;
        reg_final_values_t final_values;

       protected:
        builder_context &get_b_ctx() override { return b_ctx; }

       public:

        /* `isem_lifter_base` interface */

        auto make_semantic_call( unit_t &unit, decoder_base &decoder,
                                 semantic_fn_t isem )
            -> isem_range_t override;

        auto finalize_circuit( exalted_value_buckets ) -> value_t override;

        /* Local logic */
        void account( const reg_final_values_t &other );

        /* Random helpers */
        auto log_prefix() { return "[exalt:mux_heavy_lifter]:"; }
    };

    struct disjunctions_lifter : isem_lifter_base, isem_lifter_utilities
    {
        using base = isem_lifter_base;
        using base::base;

       private:
        // TODO( next ): Dirty trick to prototype faster. No need for this to be
        //               an attribute.
        exalted_values_t ctx_ops;

       protected:
        builder_context &get_b_ctx() override { return b_ctx; }

       public:
        /* `isem_lifter_base` interface. */
        auto make_semantic_call( unit_t &, decoder_base &, semantic_fn_t )
            -> isem_range_t override;

        auto finalize_circuit( exalted_value_buckets ) -> value_t override;

        /* `unit_component_base` */
        auto after_isem( unit_t &unit, isem_range_t isem ) -> exalted_values_t override;
    };
}  // namespace circ::exalt
