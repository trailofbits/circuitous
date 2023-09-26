/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Exalt/Common.hpp>

#include <gap/core/generator.hpp>

#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace circ::exalt
{
    // TODO( exalt ): For now these are standalone but we may want to wrap it as a separate
    //                class.
    // TODO( next ): Implement everything via buckets.
    using exalted_value_buckets = std::unordered_map< place, std::unordered_set< value_t > >;

    exalted_value_buckets to_buckets( const exalted_values_t &vs );
    void merge_to( exalted_value_buckets &to, const exalted_values_t &other );
    void merge_to( exalted_value_buckets &to, const exalted_value_buckets &other );

    gap::generator< value_t > filter( const exalted_values_t &vs, place p );

    value_set_t extract( exalted_value_buckets &b, place trg );

    std::string to_string( const exalted_value_buckets &v );

    template< typename OS >
    OS &operator<<( OS &os, const exalted_value_buckets &v )
    {
        return ( os << to_string( v ) );
    }

} // namespace circ::exalt
