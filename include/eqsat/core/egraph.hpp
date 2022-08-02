/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/core/common.hpp>
#include <gap/core/generator.hpp>
#include <gap/core/graph.hpp>
#include <gap/core/union_find.hpp>
#include <unordered_map>
#include <vector>

namespace eqsat::graph
{

    struct node_handle {
        node_id_t id;
    };

    using children_t = std::vector< node_handle >;

    struct base {
        using node_pointer = std::shared_ptr< base >;
        using child_type   = node_handle;

        gap::generator< node_handle > children() {
            for (auto ch : _children)
                co_yield ch;
        }

        void update_children(gap::invocable auto &&fn) { std::for_each(_children, fn); }

        children_t _children;
    };

    static_assert(gap::graph::node_like< base >);

    template< typename storage_type >
    struct storage_node : storage_type {
        using storage_type::storage_type;
    };

    struct bond_node {
        std::vector< std::size_t > children_parents;
    };

    template< typename storage >
    using node_variants = std::variant< storage_node< storage >, bond_node >;

    template< typename storage >
    struct node : base {
        decltype(auto) visit(gap::invocable auto &&fn) {
            return std::visit(std::forward< decltype(fn) >(fn), data);
        }

        decltype(auto) visit(gap::invocable auto &&fn) const {
            return std::visit(std::forward< decltype(fn) >(fn), data);
        }

        node_variants< storage > data;
    };

    template< typename enode >
    struct eclass {
        std::vector< enode * > nodes;
    };

    template< typename enode >
    struct edge {
        using node_type   = enode;
        using source_type = const node_type *;

        using eclass_type = eclass< node_type >;
        using target_type = const eclass_type *;

        source_type source() const { return src; }
        target_type target() const { return tgt; }

        source_type src;
        target_type tgt;
    };

    template< typename enode >
    struct egraph {
        using node_type    = enode;
        using node_pointer = node_type *;
        using edge_type    = edge< node_type >;

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

      private:
        // stores heap allocated nodes of egraph
        std::vector< std::unique_ptr< node_type > > _nodes;

        //     // stores equivalence relation between equaltity classes
        //     UnionFind _unions;

        //     // all equavalent ids  map to the same class
        //     std::unordered_map< Id, EClass > _classes;

        //     // stores equality ids of enodes
        //     std::unordered_map< const ENode *, Id > _ids;

        //     // modified eclasses that needs to be rebuild
        //     std::vector< Id > _pending;
    };

} // namespace eqsat::graph
