/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/core/common.hpp>
#include <gap/core/generator.hpp>
#include <gap/core/graph.hpp>
#include <gap/core/hash.hpp>
#include <gap/core/union_find.hpp>
#include <unordered_map>
#include <vector>

namespace eqsat::graph
{
    struct node_handle {
        node_id_t id;
    };

    constexpr gap::hash_code hash_value(gap::hash_code code, const node_handle& val) {
        return gap::hash_combine( code,
            gap::hash_code( std::hash< node_id_t >{}(val.id) )
        );
    }

    using children_t = std::vector< node_handle >;

    //
    // enode
    //
    struct base {
        using child_type   = node_handle;

        gap::generator< node_handle > children() {
            for (auto ch : _children)
                co_yield ch;
        }

        void add_child(node_handle handle) {
            _children.push_back(handle);
        }

        void update_children(gap::invocable auto &&fn) { std::for_each(_children, fn); }

        children_t _children;
    };

    static_assert(gap::graph::node_like< base >);

    template< typename storage >
    struct storage_node : storage {
        explicit storage_node(storage && data) : storage(std::move(data)) {}
    };

    template< typename storage >
    static inline std::string node_name(const storage_node< storage > &n) {
        return node_name(static_cast< storage >(n));
    }

    struct bond_node {
        std::vector< std::size_t > children_parents;
    };

    static inline std::string node_name(const bond_node &n) { return "bond"; }

    template< typename storage >
    using node_variants = std::variant< storage_node< storage >, bond_node >;

    template< typename storage >
    struct node : base {
        using node_pointer = node *;
        using storage_type = storage;

        explicit node(node_variants< storage > &&n) : data(std::move(n)) {}

        explicit node(storage_node< storage > &&n)
            : node(node_variants< storage >(std::move(n)))
        {}

        explicit node(bond_node &&n)
            : node(node_variants< storage >(std::move(n)))
        {}

        explicit node(storage &&data)
            : node(storage_node< storage >{std::move(data)})
        {}


        decltype(auto) visit(gap::invocable auto &&fn) {
            return std::visit(std::forward< decltype(fn) >(fn), data);
        }

        decltype(auto) visit(gap::invocable auto &&fn) const {
            return std::visit(std::forward< decltype(fn) >(fn), data);
        }

        node_variants< storage > data;
    };

    template< typename storage >
    std::string node_name(const node< storage > &n) {
        return std::visit( [] (const auto &n) { return node_name(n); }, n.data);
    }

    //
    // eclass
    //
    template< typename enode_pointer >
    struct eclass {
        std::vector< enode_pointer > nodes;
    };

    //
    // egraph edge
    //
    template< typename enode >
    struct edge {
        using node_type    = enode;
        using node_pointer = typename node_type::node_pointer;
        using source_type  = const node_pointer;

        using eclass_type    = eclass< node_pointer >;
        using eclass_pointer = eclass_type *;
        using target_type    = const eclass_pointer;

        source_type source() const { return src; }
        target_type target() const { return tgt; }

        source_type src;
        target_type tgt;
    };

    //
    // egraph
    //
    template< typename enode >
    struct egraph {
        using node_type    = enode;
        using storage_type = typename node_type::storage_type;
        using node_pointer = typename node_type::node_pointer;

        using edge_type    = edge< node_type >;
        using eclass_type  = eclass< node_pointer >;

        egraph()           = default;

        egraph(egraph &&)            = default;
        egraph &operator=(egraph &&) = default;

        egraph(const egraph &)            = delete;
        egraph &operator=(const egraph &) = delete;

        gap::generator< const node_pointer > nodes() const {
            for (auto &node : _nodes)
                co_yield node.get();
        }

        gap::generator< node_pointer > nodes() {
            for (auto &node : _nodes)
                co_yield node.get();
        }

        gap::generator< edge_type > edges() {}

        gap::generator< const edge_type > edges() const {}

        node_handle find(node_pointer ptr) { return _ids[ptr]; }

        node_pointer add_node(storage_type &&data) {
            _nodes.emplace_back(
                std::make_unique< node_type >(std::move(data))
            );

            // TODO make singular eclass & canonicalize egraph

            return _nodes.back().get();
        }

      private:
        // stores heap allocated nodes of egraph
        std::vector< std::unique_ptr< node_type > > _nodes;

        // stores equivalence relation between equaltity classes
        gap::resizable_union_find _unions = gap::resizable_union_find(0);

        // all equavalent ids  map to the same class
        using handle_hash = gap::hash< node_handle >;
        std::unordered_map< node_handle, eclass_type, handle_hash > _classes;

        // stores equality ids of enodes
        std::unordered_map< node_pointer, node_handle > _ids;
    };

} // namespace eqsat::graph
