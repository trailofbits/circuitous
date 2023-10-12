/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#include <circuitous/Exalt/OperandSelection.hpp>

CIRCUITOUS_RELAX_WARNINGS
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ::exalt
{
    value_t make_mux( builder_t &irb, State &state, const shadowinst::TM_t &tm )
    {
        values_t args( ( 1 << tm.bitsize ) + 1, nullptr );
        auto default_value = irb.getIntN( static_cast< unsigned >( tm.bitsize ), 0 );
        args[ 0 ] = irops::delayed_value::make( irb, values_t{ default_value }, tm.bitsize );

        auto &ctx = state.ctx;
        for ( const auto &[ str, reg_name ] : tm.reverse_bitmap() )
        {
            if ( reg_name.starts_with( "__remill_zero" ) )
                continue;

            auto idx = llvm::APInt( static_cast< uint32_t >( tm.bitsize ), str, 2 )
                .getLimitedValue();

            check( args.size() > idx + 1 );

            // Pseudo regs *do not have* remill register objects.
            auto value = [ & ]()
            {
                auto parent_reg = enclosing_reg( ctx.arch(), reg_name );
                if ( parent_reg )
                    return state.load( irb, parent_reg );
                return state.load( irb, reg_name );
            }();

            auto coerced = [ & ]
            {
                if ( ctx.bw( value ) < ctx.ptr_size )
                    return irb.CreateZExt( value, ctx.word_type() );
                return value;
            }();

            args[ idx + 1 ] = coerced;
        }

        auto trg_type = [ & ]()
        {
            for ( std::size_t i = 1; i < args.size(); ++i)
                if ( args[ i ] )
                    return args[ i ]->getType();
            log_kill() << "Trying to create select without values";
        }();

        auto zero = llvm::ConstantInt::get( trg_type, 0, false );
        for ( std::size_t i = 0; i < args.size(); ++i )
            if ( !args[ i ] )
                args[ i ] = zero;

        return irops::Select::make( irb, args );
    }

    auto TM_cache::upcast( const translation_map_t &tm ) const
        -> translation_map_t
    {
        auto out = translation_map_t( tm.bitsize );

        auto add = [ & ]( auto &into, const auto &what )
        {
            into.insert( what.begin(), what.end() );
        };

        for ( const auto &[ key, vals ] : tm )
        {
            // TODO( exalt ): pseudo-regs.
            auto reg = enclosing_reg( ctx.arch(), key );
            if ( !reg )
            {
                add( out[ key ], vals );
            } else {
                add( out[ reg->name ], vals );
            }
        }
        return out;
    }

    void TM_cache::add_sat( const translation_map_t &tm )
    {
        auto generic = upcast( tm );
        if ( !is_contained( generic ) )
            storage.push_back( generic );
    }

    // TODO( next ): Why is this a separate method? Should we not pad?
    void TM_cache::add_unsat( const translation_map_t &tm )
    {
        auto generic = upcast( tm );
        if ( !is_contained( generic ) )
            storage.push_back( generic );
    }

    void TM_cache::build_sat( const shadowinst::Instruction &s_inst )
    {
        auto process = gap::overloaded
        {
            [ & ]( const shadowinst::Reg &s_reg )
            {
                if ( s_reg.translation_map.is_saturated() )
                    add_sat( s_reg.translation_map );
            },
            [ & ]( const auto & ) {} // ignore
        };
        s_inst.for_each_present( process );
    }

    void TM_cache::build_unsat( const shadowinst::Instruction &s_inst )
    {
        auto process = gap::overloaded
        {
            [ & ]( const shadowinst::Reg &s_reg )
            {
                if ( !s_reg.translation_map.is_saturated() )
                    add_unsat( s_reg.translation_map );
            },
            [ & ]( const auto & ) {} // ignore
        };
        s_inst.for_each_present( process );
    }

    std::string TM_cache::to_string() const
    {
        std::stringstream ss;
        ss << "TM_cache with " << storage.size() << " entries:\n";
        for ( const auto &tm : storage )
            ss << tm.to_string( 1 ) << "\n";
        return ss.str();
    }

    operand_selector::operand_selector( builder_t &irb, State &state,
                                        const shadowinst::TM_t &tm )
        : mux( make_mux( irb, state, tm ) )
    {}

    void operand_selector::add_user( builder_t &irb, value_t condition, value_t value )
    {
        selectors[ value ].emplace( condition );
        auto as_inst = llvm::dyn_cast< llvm::Instruction >( &*mux );
        irops::cond_bind_delayed_value::make( irb, { as_inst->getOperand( 0 ), value,
                                                     condition },
                                              bw( irb, mux ) );
    }

    value_t operand_selector::update_mux( builder_t & )
    {
        // For now do nothing as the values are binded using delayed mechanism.
        return mux;
    }

    /* `TM_allocator` */

    std::size_t TM_allocator::map_idx( const translation_map_t &tm )
    {
        auto maybe_idx = storage.map_idx( tm );
        check( maybe_idx ) << "Failed to map tm.";
        return *maybe_idx;
    }

    auto TM_allocator::allocate( builder_t &irb, const translation_map_t &tm,
                                 State &state,
                                 std::size_t idx )
        -> operand_selector &
    {
        auto storage_idx = map_idx( tm );

        auto &materalized = read_map[ storage_idx ];
        // We ran out of muxes.
        if ( materalized.size() <= idx )
        {
            // We only can increase size by one
            check( materalized.size() == idx ) << "Requesting allocation too far ahead, got;"
                                               << materalized.size() << "requested:" << idx;
            // Make new `operand_selector`.
            materalized.emplace_back( irb, state, tm );
        }

        return materalized[ idx ];
    }

    auto operand_allocator_base::get_requester( builder_t &irb, State &state, value_t ctx_cond )
        -> requester_ptr
    {
        return std::make_unique< requester_proxy >( ctx, state, irb, ctx_cond, *this );
    }

    /* `requester_proxy` */

    value_t requester_proxy::request( const shadow_reg_t &s_reg )
    {
        auto &tm = s_reg.translation_map;
        auto tm_idx = allocator.map_idx( tm );
        if ( !used.count( tm_idx ) )
            used[ tm_idx ] = 0;
        auto &current = used[ tm_idx ];
        auto selector = allocator.allocate( irb, tm, state, current++ );

        auto selector_value = pad_selector( *selector, s_reg );
        selector.add_user( irb, ctx_cond, selector_value );
        return *selector;
    }

    value_t requester_proxy::pad_selector( value_t mux, const shadow_reg_t &s_reg )
    {
        auto raw_selector = shadowinst::Materializer( irb, s_reg ).region_selector();
        auto as_inst = llvm::dyn_cast< llvm::Instruction >( &*mux );
        auto requested_size = bw( irb, as_inst->getOperand( 0 ) );
        auto real_size = bw( irb, raw_selector );

        check( requested_size >= real_size );

        if ( real_size == requested_size )
            return raw_selector;

        auto padding = irb.getIntN( static_cast< uint32_t >( requested_size - real_size ), 0 );
        return irops::Concat::make( irb, { padding, raw_selector });
    }

} // namespace circ::exalt
