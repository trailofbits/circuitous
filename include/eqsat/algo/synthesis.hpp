/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/pattern/rewrite_rule.hpp>
#include <eqsat/core/egraph.hpp>

namespace eqsat {

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const constant_t &constant,
        const apply_pattern &pattern,
        const match_result &where,
        egraph &graph
    ) -> node_handle {
        return graph.make(constant);
    }

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const operation_t &operation,
        const apply_pattern &pattern,
        const places_t &places,
        const match_result &where,
        std::span< node_handle > children,
        egraph &graph
    ) -> node_handle {
        return graph.make(operation, children);
    }

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const place_t &place,
        const apply_pattern &pattern,
        const places_t &places,
        const match_result &where,
        std::span< node_handle > children,
        egraph &graph
    ) -> node_handle {
        return where.matched_places.find(
            std::uint32_t(place_index(place, places))
        )->second.handle();
    }

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const label_t &label,
        const apply_pattern &pattern,
        const places_t &places,
        const match_result &where,
        std::span< node_handle > children,
        egraph &graph
    ) -> node_handle {
        throw std::runtime_error("not implemented label synthesis");
    }

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const atom_t &atom,
        const apply_pattern &pattern,
        const places_t &places,
        const match_result &where,
        std::span< node_handle > children,
        egraph &graph
    ) -> node_handle {
        return std::visit([&] (const auto &a) {
            return synthesize(a, pattern, places, where, children, graph);
        }, atom);
    }

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const expr_list &list,
        const apply_pattern &pattern,
        const places_t &places,
        const match_result &where,
        std::span< node_handle > children,
        egraph &graph
    ) -> node_handle {
        if (is_nested_list(list)) {
            throw std::runtime_error("nested expression in rewrite pattern");
        }

        graph::children_t list_children;
        for (const auto &child : tail(list)) {
            list_children.push_back(
                synthesize_simple_expr(child, pattern, places, where, {}, graph)
            );
        }

        return synthesize_simple_expr(list.front(), pattern, places, where, list_children, graph);
    }

    template< gap::graph::graph_like egraph >
    auto synthesize_simple_expr(
        const simple_expr &expr,
        const apply_pattern &pattern,
        const places_t &places,
        const match_result &where,
        std::span< node_handle > children,
        egraph &graph
    ) -> node_handle {
        return std::visit([&] (const auto &e) {
            return synthesize(e, pattern, places, where, children, graph);
        }, expr);
    }

} // namespace eqsat
