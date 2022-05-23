/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <type_traits>

namespace circ::util
{
    namespace detail
    {
        template< uint64_t idx, typename H, typename ... Args >
        struct from_pack
        {
            static_assert( idx < sizeof ... (Args) );
            using type = typename from_pack< idx - 1, Args ... >::type;
        };

        template< typename H, typename ... Args >
        struct from_pack< 0, H, Args ... >
        {
            using type = H;
        };
    }

    template< uint64_t idx, typename ... Args >
    using from_pack = typename detail::from_pack< idx, Args ... >::type;

    // Unfortunately generic ctor of form:
    // `template< typename ... Args > X(Args && ...args )` will match on almost everything,
    // therefore to exclude copy ctor we need to constraint it.
    template< typename P, typename ... Args >
    concept is_copy_ctor_of = std::is_same_v < from_pack< 0, Args ... >, const P & > &&
                              sizeof ... (Args) == 1;


    // TODO(lukas): Remove once c++23 is available since it will be in `std::`.
    template< typename E > requires (std::is_enum_v< E >)
    auto to_underlying(E e) -> std::underlying_type_t< E >
    {
        return static_cast< std::underlying_type_t< E > >(e);
    }
} // namespace circ::util
