/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#include <circuitous/Exalt/Lifter.hpp>

#include <circuitous/Exalt/CSE.hpp>
#include <circuitous/Exalt/ISemLifters.hpp>

#include <circuitous/Lifter/ISELBank.hpp>
#include <circuitous/Lifter/Components/OperandSelection.hpp>

#include <circuitous/Lifter/Undefs.hpp>
#include <circuitous/Lifter/BaseLifter.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Check.hpp>

CIRCUITOUS_RELAX_WARNINGS
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ
{
    namespace
    {
        value_t last_store( const auto &stores )
        {
            // NOTE( exalt ): It is expected that if there are multiple stores,
            //                Flattener component will make sure they are properly guarded
            //                wrt path condition.
            check( stores.size() >= 1, [ & ]{ return dbg_dump( stores ); } );

            // Next they are being ordered to determine which is last, therefore
            // they need to be in the same basic block
            auto bb = stores[ 0 ]->getParent();
            llvm::StoreInst *last = stores[ 0 ];

            for ( auto store : stores )
            {
                auto to_store = inst_distance( &*bb->begin(), store );
                auto current = inst_distance( &*bb->begin(), last );
                if ( to_store > current )
                    last = store;
            }
            return last->getOperand( 0 );
        }

        void remove_write( value_t dst )
        {
            auto p_type = llvm::dyn_cast< llvm::PointerType >( dst->getType() );
            check( p_type ) << "Dst reg type is not a pointer.";

            auto as_inst = llvm::dyn_cast< llvm::Instruction >( dst );
            check( as_inst );

            llvm::IRBuilder<> bld( as_inst );
            auto allocation = bld.CreateAlloca( p_type, nullptr, "DSTA_" );

            as_inst->replaceAllUsesWith( allocation );
            as_inst->eraseFromParent();
        }
    } // namespace

    auto unit_lifter::exalt( unit_t &unit ) -> exalted_value_buckets
    {
        exalted_value_buckets out;
        auto collect = [ & ]( auto w ) { merge_to( out, std::move( w ) ); };

        collect( local_components.init( unit ) );
        collect( b_ctx.exalt_prologue( unit ) );
        // TODO( next ): `before_isem( unit )`.

        // TODO( next ): Bump pc.
        auto semantic = isem::semantic_fn( unit.isel, *llvm_module() );
        check( semantic ) << log_prefix() << "Could not fetch semantic for unit!";
        post_lift( **semantic );

        // Make the actual call
        auto &decoder = local_components.get_decoder();
        auto &isem_lifter = local_components.get_isem_lifter();
        auto [ begin, end ] = isem_lifter.make_semantic_call( unit, decoder, *semantic );

        auto values = local_components.after_isem( unit, isem_range_t{ begin, end } );
        collect( to_buckets( freeze< std::vector >( values ) ) );

        // TODO( next ): Should something happen here?
        //               Maybe `b_ctx.exalt_epilogue( unit )`?
        auto ctx = make_context( extract( out, place::ctx ) );
        out[ place::ctx ].emplace( ctx );
        return out;
    }

    auto unit_lifter::make_context( value_set_t vs ) -> value_t
    {
        // Someone can go and rework/adapt `irops::` to work over generic ranges.
        auto args = values_t( vs.begin(), vs.end() );
        return irops::VerifyInst::make( irb(), args );

    }

    void circuit_producer::exalt( unit_t &unit )
    {
        auto u_lifter = unit_lifter( b_ctx, pucs );
        merge_to( exalted_buckets, u_lifter.exalt( unit ) );
    }

    void circuit_producer::make_ret()
    {
        check( exalted_buckets.size() == 1, [&](){ return to_string( exalted_buckets ); } );
        check( exalted_buckets.count( place::root ) );

        auto &bld = b_ctx.irb();

        auto roots = extract( exalted_buckets, place::root );
        auto as_args = values_t( roots.begin(), roots.end() );
        auto result = irops::And::make( bld, as_args );

        bld.CreateRet( result );

    }

    void circuit_producer::finalize()
    {
        auto ret_val = pucs.get_isem_lifter().finalize_circuit( exalted_buckets );
        b_ctx.irb().CreateRet( ret_val );

        auto fn = b_ctx.fn_ctx.fn.get();

        ctx.clean_module( { fn } );
        remill::VerifyModule( ctx.module() );

        // Cleanup
        auto exec = [ & ]( auto v ) { return remove_write( v ); };
        irops::AllocateDst::for_all_in( fn, exec );

        auto erase = [ & ]( auto v ) { v->eraseFromParent(); };
        irops::Breakpoint::for_all_in( fn, erase );

        irops::Error::for_all_in( fn, []( auto c ) { c->eraseFromParent(); } );

        // Undefs
        replace_remill_undefs( fn );

        // We firstly optimize, otherwise there would still be memory operations
        // and we would not be able to propagate undefs.
        optimize_silently( { fn } );
        propagate_remill_undefs( fn );

        cse( *fn );


        optimize_silently( { fn } );
    }

    void circuit_producer::init_pucs()
    {
        pucs.emplace< mux_heavy_lifter >().init();
        pucs.emplace< timestamp >().init();
    }

}  // namespace circ
