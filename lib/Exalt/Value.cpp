/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#include <circuitous/Exalt/Value.hpp>

#include <circuitous/Util/LLVMUtil.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Check.hpp>

CIRCUITOUS_RELAX_WARNINGS
CIRCUITOUS_UNRELAX_WARNINGS

#include <sstream>

namespace circ
{
    exalted_value_buckets to_buckets( const exalted_values_t &vs )
    {
        exalted_value_buckets out;
        for ( auto [ p, v ] : vs )
            out[ p ].insert( v );
        return out;
    }

    void merge_to( exalted_value_buckets &to, const exalted_values_t &other )
    {
        return merge_to( to, to_buckets( other ) );
    }

    void merge_to( exalted_value_buckets &to, const exalted_value_buckets &other )
    {
        for ( auto [ p, vs ] : other )
            to[ p ].insert( vs.begin(), vs.end() );
    }

    gap::generator< value_t > filter( const exalted_values_t &vs, place trg )
    {
        for ( auto [ p, v ]  : vs )
            if ( p == trg )
                co_yield v;
    }

    value_set_t extract( exalted_value_buckets &b, place trg )
    {
        auto out = b[ trg ];
        b.erase( trg );
        return out;
    }

    std::string to_string( const exalted_value_buckets &buckets )
    {
        std::ostringstream os;
        for ( auto [ p, vs ] : buckets )
        {
            os << to_string( p ) << " {\n";
            for ( auto v : vs )
               os << "\t\t" << dbg_dump( v ) << "\n";
            os << "}\n";

        }
        return os.str();
    }
} // namespace circ
