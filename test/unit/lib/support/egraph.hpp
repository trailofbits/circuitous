/*
 * Copyright (c) 2022, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <eqsat/core/egraph.hpp>

#include <charconv>
#include <string>

namespace eqsat::test {

    struct string_storage
    {
        explicit string_storage( std::string_view str ) : data( str ) { }

        bool operator==( const string_storage & ) const = default;

        std::string data;
    };

    static inline std::string node_name( const string_storage &node )
    {
        return node.data;
    }

    static inline std::optional< gap::bigint > extract_constant( std::string_view str ) {
        if (std::uint64_t value; std::from_chars(str.data(), str.data() + str.size(), value).ec == std::errc{})
            return gap::bigint(64, value);
        return std::nullopt;
    }

    static inline std::optional< gap::bigint > extract_constant( const string_storage &node ) {
        return extract_constant(node.data);
    }

    using test_node = eqsat::graph::node< string_storage >;
    static_assert( gap::graph::node_like< test_node > );

    using test_edge = eqsat::graph::edge< test_node >;
    static_assert( gap::graph::edge_like< test_edge > );

    using test_graph = eqsat::graph::egraph< test_node >;
    static_assert( gap::graph::graph_like< test_graph > );

    template < typename... children_t >
    auto make_node( test_graph &egraph, std::string_view name, children_t... children )
        -> graph::node_handle
    {
        auto node = egraph.add_node( string_storage( name ) );
        ( node->add_child( children ), ... );
        return egraph.find( node );
    }

} // namespace eqsat::test
