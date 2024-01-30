/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/Util/Warnings.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <circuitous/IR/Circuit.hpp>

#include <circuitous/Exalt/Lifter.hpp>
#include <circuitous/Exalt/ISemLifters.hpp>

#include <circuitous/Lifter/BaseLifter.hpp>
#include <circuitous/Lifter/CircuitBuilder.hpp>
#include <circuitous/Lifter/CircuitSmithy.hpp>
#include <circuitous/Lifter/Lifter.hpp>
#include <circuitous/Lifter/LLVMToCircIR.hpp>

namespace circ
{
    auto CircuitSmithy::categorize( atoms_t atoms ) -> worklist_t
    {
        std::unordered_map< isel_t, atoms_t > groups;

        for ( auto &atom : std::move( atoms ) )
        {
            auto isel = atom.isel();
            groups[ isel ].push_back( std::move( atom ) );
        }

        worklist_t out;
        for ( auto &[ isel, atom ] : std::move( groups ) )
            out.emplace( isel, std::move( atom ) );

        return out;
    }

    auto CircuitSmithy::purify( const std::vector< InstBytes > &insts ) -> concretes_t
    {
        return freeze< std::vector >( decode_all( ctx, insts ) );
    }

    auto CircuitSmithy::purify( std::string_view raw_bytes ) -> concretes_t
    {
        return decode_all( ctx, raw_bytes );
    }

    auto CircuitSmithy::smelt( concretes_t &&concretes ) -> atoms_t
    {
        atoms_t out;
        for ( auto concrete : std::move( concretes ) )
        {
            auto abstract = fuzz_operands( *ctx.arch(), concrete );
            out.emplace_back( std::move( concrete ), std::move( abstract ) );
        }

        for ( auto &atom : out )
            atom.abstract.distribute_selectors();

        return out;
    }

    void CircuitSmithy::register_submodules( exalt::circuit_producer &producer ) const
    {
        auto add = [ & ]( auto cs )
        {
            switch ( cs )
            {
                case circuit_submodule::external_syscalls:
                    return producer.add_syscalls();
            }
        };

        for ( auto cs : submodules )
            add( cs );
    }

    auto CircuitSmithy::forge_common( exalt::circuit_producer &producer,
                                         atoms_t &&atoms )
        -> circuit_ptr_t
    {
        auto worklist = categorize( std::move( atoms ) );

        producer.add_operand_selector( worklist );
        register_submodules( producer );
        log_info() << "[smithy]:" << "Worklist contains:" << worklist.size() << "entries!";

        for ( auto &unit : worklist )
            producer.exalt( unit );
        producer.finalize();
        auto circuit_fn = std::move( producer ).take_fn();
        return lower_fn( &*circuit_fn, ctx.ptr_size );
    }


    auto CircuitSmithy::forge_disjunctions( concretes_t &&concrete ) -> circuit_ptr_t
    {
        auto producer = exalt::circuit_producer( ctx );
        producer.add_isem_lifter< exalt::disjunctions_lifter >();
        return forge_common( producer, smelt( std::move( concrete ) ) );
    }

    auto CircuitSmithy::forge_mux_heavy( concretes_t &&concrete ) -> circuit_ptr_t
    {
        auto producer = exalt::circuit_producer( ctx );
        producer.add_isem_lifter< exalt::mux_heavy_lifter >();
        return forge_common( producer, smelt( std::move( concrete ) ) );
    }

    auto CircuitSmithy::forge_v3( concretes_t &&concrete ) -> circuit_ptr_t
    {
        log_info() << "[smithy]: Running old v3 lifter.";
        auto worklist = categorize( smelt( std::move( concrete ) ) );

        auto circuit_fn = CircuitFunction_v2( ctx );
        auto exalt_context = ExaltationContext( ctx, circuit_fn );
        for ( auto &unit : worklist )
            exalt_context.exalt( unit );

        exalt_context.finalize();
        return lower_fn( &*circuit_fn, ctx.ptr_size );
    }
} // namespace circ
