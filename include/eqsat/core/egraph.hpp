/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/core/common.hpp>
#include <eqsat/pattern/pattern.hpp>

#include <gap/core/generator.hpp>
#include <gap/core/graph.hpp>
#include <gap/core/hash.hpp>
#include <gap/core/union_find.hpp>
#include <gap/core/bigint.hpp>

#include <unordered_map>
#include <vector>
#include <iostream>

namespace eqsat::graph
{
    struct node_handle {
        explicit node_handle(node_id_t id) : id(id) {}

        node_id_t id;

        constexpr bool operator==(const node_handle& other) const = default;
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

        gap::generator< node_handle > children() const {
            for (auto ch : _children)
                co_yield ch;
        }

        child_type child(std::size_t idx) const {
            return _children[idx];
        }

        std::size_t num_of_children() const { return _children.size(); }

        // TODO fix parents
        void add_child(node_handle handle) {
            _children.push_back(handle);
        }

        template< typename Fn >
        void update_children(Fn &&fn) {
            std::for_each(_children.begin(), _children.end(), std::forward< Fn >(fn));
        }

        children_t _children;
    };

    static_assert(gap::graph::node_like< base >);

    template< typename storage >
    struct storage_node : storage {
        explicit storage_node(storage && data) : storage(std::move(data)) {}
    };

    template< typename storage >
    std::string node_name(const storage_node< storage > &n) {
        return node_name(static_cast< storage >(n));
    }

    template< typename storage >
    std::optional< gap::bigint > extract_constant(const storage_node< storage > &n) {
        return extract_constant(static_cast< storage >(n));
    }

    struct bond_node {
        std::vector< std::size_t > children_parents;
    };

    static inline std::string node_name(const bond_node &n) { return "bond"; }

    static inline std::optional< gap::bigint > extract_constant(const bond_node &n) {
        return std::nullopt;
    }

    template< typename storage >
    using node_variants = std::variant< storage_node< storage >, bond_node >;

    template< typename storage >
    struct node : base {
        using node_pointer = node *;
        using const_node_pointer = node const*;
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

    template< typename storage >
    std::optional< gap::bigint > extract_constant(const node< storage > &n) {
        return std::visit( [] (const auto &n) { return extract_constant(n); }, n.data);
    }

    //
    // eclass
    //
    template< typename enode_pointer >
    struct eclass {
        void merge(eclass &&other) {
            std::move(other.nodes.begin(), other.nodes.end(), std::back_inserter(nodes));
            std::move(other.parents.begin(), other.parents.end(), std::back_inserter(parents));
        }

        bool operator==(const eclass&) const = default;

        std::vector< enode_pointer > nodes;
        std::vector< enode_pointer > parents;
    };

    template< typename enode_pointer >
    static eclass< enode_pointer > singleton_eclass(enode_pointer node) {
        return {{ node }, {}};
    }

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
        using const_node_pointer = typename node_type::const_node_pointer;

        using edge_type      = edge< node_type >;
        using eclass_type    = eclass< node_pointer >;
        using eclass_pointer = eclass_type *;

        using handle_hash  = gap::hash< node_handle >;
        using eclass_map   = std::unordered_map< node_handle, eclass_type, handle_hash >;

        egraph() = default;

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

        gap::generator< edge_type > edges() {
            for (auto &node : nodes()) {
                for (auto &ch : node->children()) {
                    auto cls = eclass(ch);
                    co_yield edge_type{node, &cls};
                }
            }
        }

        gap::generator< const edge_type > edges() const {
            for (const auto &node : nodes()) {
                for (const auto &ch : node->children()) {
                    const auto &cls = _classes.at(find(ch));
                    co_yield edge_type{node, const_cast< const eclass_pointer >(&cls)};
                }
            }
        }

        using eclass_pair = typename eclass_map::value_type;
        gap::generator< const eclass_pair & > eclasses() const {
            for (const auto &pair : _classes)
                co_yield pair;
        }

        std::size_t num_of_eclasses() const { return _classes.size(); }

        node_handle find(const_node_pointer ptr) const {
            return find(_ids.at(const_cast< node_pointer >(ptr)));
        }

        node_handle find(node_pointer ptr) { return find(_ids.at(ptr)); }

        node_handle find(node_handle node) const {
            return node_handle( _unions.find(node.id) );
        }

        node_handle find(node_handle node) {
            return node_handle( _unions.find(node.id) );
        }

        void canonicalize(node_type &node) {
            node.update_children([&](node_handle child) {
                return _unions.find(child.id); /* compresses paths */
            });
        }


        node_handle insert(storage_type &&data, std::span< node_handle > children) {
            auto node = add_node(std::move(data));
            for (auto ch : children) {
                add_child(node, ch);
            }
            return find(node);
        }

        const eclass_type &eclass(node_handle handle) const { return _classes.at(find(handle)); }
        eclass_type &eclass(node_handle handle) { return _classes.at(find(handle)); }

        const auto& parents(node_handle handle) const {
            return eclass(handle).parents;
        }

        void canonicalize(node_handle eclass) {
            for (auto node : _classes[eclass].nodes) {
                this->canonicalize(*node);
            }
        }

        // require canonicalized eclasses
        void repair(node_handle eclass) { repair(_classes[eclass]); }

        void repair(eclass_type &eclass) {
            // deduplicate the parents, noting that equal
            // parents get merged and put on the worklist
            // std::unordered_set< node_handle, handle_hash > seen;
            // std::vector< node_pointer > new_parents;
            // for (auto *node : eclass.parents) {
            //     if (auto id = _unions.find( _ids[node] ); !seen.count(id)) {
            //     new_parents.push_back(node);
            //     seen.insert(id);
            //     }
            // }

            // eclass.parents = std::move(new_parents);

            // obliterate empty classes
            remove_empty_eclasses();
        }

        void remove_empty_eclasses() {
            std::erase_if(_classes, [](const auto& eclass) {
                const auto &[handle, cls] = eclass;
                return cls.nodes.empty();
            });
        }

      protected:

        void add_parent(node_handle eclass, node_pointer parent) {
            _classes.at(eclass).parents.push_back(parent);
        }

        void add_child(node_pointer node, node_handle child) {
            node->add_child(child);
            add_parent(child, node);
        }

        node_pointer add_node(storage_type &&data) {
            auto node = _nodes.emplace_back(
                std::make_unique< node_type >(std::move(data))
            ).get();

            node_handle id{ _unions.make_new_set().parent };

            _classes.emplace(id, singleton_eclass(node));

            _ids.emplace(node, id);

            return node;
        }

        // stores heap allocated nodes of egraph
        std::vector< std::unique_ptr< node_type > > _nodes;

        // stores equivalence relation between equaltity classes
        mutable gap::resizable_union_find _unions = gap::resizable_union_find(0);

        // all equavalent ids  map to the same class
        eclass_map _classes;

        // stores equality ids of enodes
        std::unordered_map< node_pointer, node_handle > _ids;

        // modified eclasses that needs to be rebuild
        std::vector< node_id_t > _pending;

    };

    template< gap::graph::graph_like graph, template< typename > typename builder >
    struct egraph_pattern_buildable : graph {

        using builder_type = builder< egraph_pattern_buildable >;

        using graph::insert;

        node_handle insert(const constant_t &con) {
            return insert(builder_type::make(con), {});
        }

        node_handle insert(const operation_t &op, std::span< node_handle > children) {
            return insert(builder_type::make(op), children);
        }
    };

} // namespace eqsat::graph
