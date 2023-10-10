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
        args[ 0 ] = irb.getIntN( static_cast< unsigned >( tm.bitsize ), 0 );

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

    auto TM_cache::upcast( const translation_map_t &tm )
        -> translation_map_t
    {
        auto out = translation_map_t( tm.bitsize );

        for ( const auto &[ key, vals ] : tm )
        {
            // TODO( exalt ): pseudo-regs.
            auto reg = enclosing_reg( ctx.arch(), key );
            if ( !reg )
            {
                out[ key ] = vals;
            } else {
                out[ reg->name ] = vals;
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

    void operand_selector::add_user( value_t condition, value_t value )
    {
        selectors[ value ].emplace( condition );
    }

    value_t operand_selector::update_mux( builder_t &irb )
    {
        auto selector = irops::Switch::make< irops::Option >( irb, selectors );
        auto as_inst = llvm::dyn_cast< llvm::Instruction >( mux );
        check( as_inst );
        as_inst->setOperand( 0, selector );
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
        auto maybe_storage_idx = map_idx( tm );
        // This should not fire as spec requires first calling `map_idx`.
        check( maybe_storage_idx );
        auto storage_idx = maybe_storage_idx;

        auto &materalized = read_map[ storage_idx ];
        // We ran out of muxes.
        if ( materalized.size() < idx )
        {
            check( materalized.size() == idx );
            // Make new `operand_selector`.
        }

        return materalized[ idx ];
    }

    auto operand_allocator_base::get_requester( builder_t &irb, State &state, value_t ctx_cond )
        -> requester_ptr
    {
        return std::make_unique< requester_proxy >( ctx, state, irb, ctx_cond, *this );
    }

    /* `requester_proxy` */

    value_t requester_proxy::request( const translation_map_t &tm, bool is_read )
    {
        auto tm_idx = allocator.map_idx( tm );
        if ( !used.count( tm_idx ) )
            tm_idx = 0;
        auto &current = used[ tm_idx ];
        auto selector = allocator.allocate( irb, tm, is_read, current++ );
        return *selector;
    }

    value_t requester_proxy::request( const shadow_reg_t &s_reg, bool is_read )
    {
        return request( s_reg.translation_map, is_read );
    }

} // namespace circ::exalt
