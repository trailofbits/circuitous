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
        auto split = str.find_first_of(':');
        if (split == std::string_view::npos) {
            return std::nullopt;
        }

        auto v = str.substr(0, split);
        auto b = str.substr(split + 1);

        auto parse_bitwidth = [] (auto str) -> std::optional< std::uint64_t > {
            if (std::uint64_t value; std::from_chars(str.data(), str.data() + str.size(), value).ec == std::errc{}) {
                return value;
            }

            return std::nullopt;
        };

        if (auto bitwidth = parse_bitwidth(b)) {
            return gap::bigint(bitwidth.value(), v, 10);
        }

        return std::nullopt;
    }

    static inline std::optional< gap::bigint > extract_constant( const string_storage &node ) {
        return extract_constant(node.data);
    }

    template< typename egraph >
    struct test_graph_from_pattern_builder {

        using storage_type = typename egraph::storage_type;

        static std::string constant_storage(gap::bigint value) {
            return std::to_string(value) + ":" + std::to_string(value.bits);
        }

        static storage_type make(const eqsat::constant_t &con) {
            return storage_type(constant_storage(con.ref()));
        }

        static storage_type make(const eqsat::atom_t &op) {
            return string_storage(to_string(op));
        }
    };

    using test_node = eqsat::graph::node< string_storage >;
    static_assert( gap::graph::node_like< test_node > );

    using test_edge = eqsat::graph::edge< test_node >;
    static_assert( gap::graph::edge_like< test_edge > );

    using test_graph = eqsat::graph::egraph_pattern_buildable<
        eqsat::graph::egraph< test_node >, test_graph_from_pattern_builder
    >;
    static_assert( gap::graph::graph_like< test_graph > );

    static inline auto make_node( test_graph &egraph, std::string_view name, std::vector< node_handle > children )
        -> graph::node_handle
    {
        auto node = egraph.insert( string_storage( name ), children );
        return egraph.find( node );
    }

    static inline auto make_node( test_graph &egraph, std::string_view name )
        -> graph::node_handle
    {
        return make_node(egraph, name, {});
    }

} // namespace eqsat::test
