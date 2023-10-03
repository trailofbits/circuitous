/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#include <circuitous/Exalt/OperandSelection.hpp>

CIRCUITOUS_RELAX_WARNINGS
CIRCUITOUS_UNRELAX_WARNINGS


namespace circ::exalt
{
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
        ss << "TM_cache:\n";
        for ( const auto &tm : storage )
            ss << tm.to_string( 1 ) << "\n";
        return ss.str();
    }

    operand_selector::operand_selector( const shadowinst::TM_t &tm )
    {

    }

    void operand_selector::add_user( value_t condition, value_t selector )
    {

    }

    value_t operand_selector::build_mux()
    {
        return nullptr;
    }

} // namespace circ::exalt
