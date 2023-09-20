/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#include <circuitous/Exalt/ISemLifters.hpp>

#include <circuitous/Exalt/Components.hpp>
#include <circuitous/Exalt/Value.hpp>

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
    } // namespace

    /* Shared helpers */

    void isem_lifter_utilities::bump_pc( unit_t &unit, decoder_base &decoder )
    {
        log_dbg() << "Bumping pc";
        auto decoder_it = decoder.begin();
        auto &bld = get_b_ctx().irb();

        values_t options;
        for ( auto &atom : unit )
        {
            auto inst_size = llvm::ConstantInt::get( l_ctx().word_type(),
                                                     atom.encoding_size() );
            options.emplace_back( irops::Option::make( bld, { inst_size, *( decoder_it++ ) },
                                                       bw( inst_size ) ) );
        }
        auto offet = irops::Switch::make( bld, options );
        auto next_inst = bld.CreateAdd( arch_state().load( bld, l_ctx().pc_reg() ), offet );
        arch_state().store( bld, l_ctx().pc_reg(), next_inst );
    }


    auto isem_lifter_utilities::get_operands( unit_t &unit, decoder_base &decoder,
                                              semantic_fn_t isem )
        -> std::tuple< lifted_operands_t, writes_t >
    {
        // I have no idea if this should be done somewhere else.
        lifted_operands_t operands = { *mem_ptr(), *arch_state() };
        writes_t writes;
        auto requester = build::StatelessRequester( l_ctx(), arch_state() );

        auto handle_reg_write = [ & ]( auto arg, std::size_t i )
            -> std::optional< llvm::CallInst * >
        {
            if ( !unit.is_write( i ) )
                return {};

            // Memory writes do not require special handling.
            if ( unit.type( i ) != remill::Operand::kTypeRegister )
                return {};

            auto dst = irops::AllocateDst::make( irb(), *arch_state(), arg->getType() );
            writes.emplace_back( dst, i );
            return dst;

        };

        auto handle = [ & ]( auto view, auto &decoder_it, auto arg, std::size_t i )
        {
            check( decoder_it != decoder.end() );
            auto &bld = irb();
            auto lifter = build::OperandLifter( l_ctx(), bld, requester, unit.is_read( i ) );
            auto operand = lifter.lift( view );

            if ( bw( operand ) < bw( arg ) )
                operand = bld.CreateSExt( operand, arg->getType() );
            return irops::Option::make( bld,
                                        { operand, *( decoder_it++ ) },
                                        bw( operand ) );

        };

        for ( std::size_t i = 0; i < unit.operand_count(); ++i )
        {
            auto decoder_it = decoder.begin();

            auto operand = [ & ]() -> llvm::CallInst *
            {
                auto arg = remill::NthArgument( isem, 2 + i );
                if ( auto dst = handle_reg_write( arg, i ) )
                    return *dst;

                values_t options;
                for ( auto view : unit.slices( i ) )
                    options.push_back( handle( view, decoder_it, arg, i ) );

                auto &bld = irb();
                return irops::Switch::make( bld, options );
            }();
            operands.push_back( operand );

        }

        return { operands, writes };
    }

    auto isem_lifter_utilities::stores_to( llvm::Instruction *v ) -> stores_t
    {
        stores_t out;
        auto collect = [ & ]( auto src, auto next ) -> void
        {
            for ( auto user : src->users() )
            {
                if ( auto store = llvm::dyn_cast< llvm::StoreInst >( user ) )
                    out.push_back( store );
                if ( auto bc = llvm::dyn_cast< llvm::BitCastInst >( user ) )
                    next( bc, next );

                check( !llvm::isa< llvm::PtrToIntInst, llvm::GetElementPtrInst >( user ) );
            }

        };
        collect( v, collect );
        return out;
    }

    /* `mux_heavy_lifter` */

    auto mux_heavy_lifter::make_semantic_call( unit_t &unit, decoder_base &decoder,
                                               semantic_fn_t isem )
        -> isem_range_t
    {
        bump_pc( unit, decoder );
        auto [ operands, writes ] = get_operands( unit, decoder, isem );

        log_dbg() << log_prefix() << dbg_dump( operands ) << "into" << dbg_dump( isem );
        auto sem_call = irb().CreateCall( isem, operands );
        // TODO( exalt ): How is `inline_flattened` not breaking `irb`?
        auto [ begin, end ] = inline_flattened( sem_call, get_make_breakpoint() );

        auto parsed_writes = parse_writes( unit, decoder, writes );
        gather_final_values( unit, decoder, parsed_writes );

        return { begin, end };
    }

    auto mux_heavy_lifter::parse_writes( unit_t &unit, decoder_base &decoder,
                                         writes_t writes )
        -> parsed_writes_t
    {
        parsed_writes_t out;

        // TODO( exalt ): Implement/Port from `lifter_v2`.
        for ( auto [ dst, idx ] : writes )
        {
            auto conds = write_conditions( unit, decoder, idx );
            auto stores = stores_to( dst );
            out.emplace( idx, std::make_tuple( std::move( stores ),
                                                          std::move( conds ) ) );
        }

        return out;
    }

    auto mux_heavy_lifter::write_conditions( unit_t &unit, decoder_base &decoder,
                                             std::size_t idx ) -> reg_to_vals
    {
        reg_to_vals out;

        auto decoder_it = decoder.begin();
        auto &bld = irb();

        auto key_as_constant = [ & ]( const auto &key )
        {
            auto bstr = shadowinst::TM_t::make_bitstring( key );
            auto c = llvm::APInt( static_cast< uint32_t >( bstr.size() ), bstr, 2 );
            return bld.getInt( c );
        };

        auto handle = [ & ]( const auto &r_reg, const auto &s_reg )
        {
            if ( !s_reg )
                return;

            if ( s_reg->tm().empty() )
            {
                out[ r_reg->name ].emplace_back( *decoder_it );
                return;
            }

            auto selector = shadowinst::Materializer( bld, *s_reg ).region_selector();
            check( decoder_it != decoder.end() );

            for ( auto [ name, keys ] : s_reg->tm() )
            {
                values_t conds;
                for ( auto key : keys )
                    conds.emplace_back( irb().CreateICmpEQ( selector, key_as_constant( key ) ) );
                auto selectors = irops::Or::make( bld, conds );
                out[ name ].emplace_back( irops::And::make( bld, { *decoder_it, selectors } ) );
            }

        };

        for ( auto view : unit.slices( idx ) )
        {
            auto reg = std::get_if< typename decltype( view )::reg >( &view.raw );
            check( reg );
            auto [ r_reg, s_reg ] = *reg;
            handle( r_reg, s_reg );
            ++decoder_it;
        }

        return out;
    }


    // Computes for each reg what value it holds at the end. Condition that the register
    // is being written to is included.
    // If reg is not present, it means it is constant during this semantic.
    // TODO( exalt ): Top-level condition is always `unit_decoder`?
    void mux_heavy_lifter::gather_final_values( unit_t &unit,
                                                decoder_base &decoder,
                                                const parsed_writes_t &writes )
    {
        auto &bld = irb();
        auto unit_decoder = decoder.unit_decoder();

        for ( auto &reg : l_ctx().regs() )
        {
            auto normal_flow_value = arch_state().load( bld, reg );
            values_t options;
            for ( auto [ _, data ] : writes )
            {
                auto [ stores, conds ] = data;
                for ( auto &[ key, vals ] : conds )
                {
                    if ( enclosing_reg( l_ctx().arch(), key ) != reg )
                        continue;

                    auto runtime_value = last_store( stores );
                    arch_state().store( bld, l_ctx().reg( key ), runtime_value );

                    auto full_value = arch_state().load( bld, reg );
                    auto full_conds = irops::Or::make( bld, vals );

                    options.emplace_back( irops::Option::make( bld,
                                                               { full_value, full_conds },
                                                               bw( full_value ) ) );
                    // Because we are modifying value of state in the bitcode, we need to
                    // reset it now
                    arch_state().store( bld, reg, normal_flow_value );
                }
            }

            if ( options.empty() )
            {
                final_values[ reg ].emplace_back( unit_decoder, normal_flow_value );
            } else {
                // Base base in case nothing was written
                options.push_back( irops::Option::make( bld,
                                                        { normal_flow_value, bld.getTrue() },
                                                        bw( normal_flow_value ) ) );
                auto s = irops::Switch::make( bld, options );
                final_values[ reg ].emplace_back( unit_decoder, s );
            }
        }
    }

    auto mux_heavy_lifter::finalize_circuit( exalted_value_buckets buckets ) -> value_t
    {
        for ( auto reg : l_ctx().regs() )
            buckets[ place::root ].insert( reg_check( reg ) );

        auto &bld = irb();
        auto mk_args = [ & ]( auto roots )
        {
            return values_t( roots.begin(), roots.end() );
        };

        auto ctxs = extract( buckets, place::ctx );
        buckets[ place::root ].emplace( irops::Or::make( bld, mk_args( ctxs ) ) );

        return irops::And::make( bld, mk_args( extract( buckets, place::root ) ) );
    }

    auto mux_heavy_lifter::reg_check( reg_ptr_t reg ) -> value_t
    {
        auto &bld = irb();

        auto muxed_operands = [ & ]() -> gap::generator< value_t >
        {
            check( final_values.count( reg ) );
            const auto &partials = final_values[ reg ];

            if ( partials.empty() )
                co_yield irops::input_reg( bld, reg );
            else
                for ( auto [ unit_decoder, runtime ] : partials )
                    co_yield irops::Option::make( bld, { runtime, unit_decoder },
                                                       bw( runtime ) );
        };

        auto mux = irops::make< irops::Switch >( bld, muxed_operands() );
        auto out_reg = irops::output_reg( bld, reg );
        return irops::OutputCheck::make( bld, { mux, out_reg } );
    }

    auto disjunctions_lifter::make_semantic_call( unit_t &unit, decoder_base &decoder,
                                                 semantic_fn_t isem )
        -> isem_range_t
    {
        return {};
    }


    auto disjunctions_lifter::finalize_circuit( exalted_value_buckets buckets )
        -> value_t
    {
        return {};
    }

}  // namespace circ
