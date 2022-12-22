/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/pattern/rewrite_rule.hpp>
#include <eqsat/core/egraph.hpp>
#include <eqsat/pattern/pattern.hpp>

namespace eqsat {

    using graph::node_handle;

    template< gap::graph::graph_like egraph >
    auto synthesize_simple_expr(
        const simple_expr &expr,
        const apply_pattern &pattern,
        const places_t &places,
        const match_result &where,
        std::span< node_handle > children,
        egraph &graph
    ) -> node_handle;

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const expr_with_context &expr,
        const apply_pattern &pattern,
        const places_t &places,
        const match_result &where,
        std::span< node_handle > children,
        egraph &graph
    ) -> node_handle {
        return synthesize_simple_expr(expr.expr, pattern, places, where, children, graph);
    }

    template< gap::graph::graph_like egraph >
    auto synthesize_atom(
        const constant_t &constant,
        const apply_pattern &pattern,
        const places_t &places,
        const single_match_result &where,
        std::span< node_handle > children,
        egraph &graph
    ) -> node_handle {
        return graph.insert(constant);
    }

    template< gap::graph::graph_like egraph >
    auto synthesize_atom(
        const operation_t &operation,
        const apply_pattern &pattern,
        const places_t &places,
        const single_match_result &where,
        std::span< node_handle > children,
        egraph &graph
    ) -> node_handle {
        return graph.insert(operation, children);
    }

    template< gap::graph::graph_like egraph >
    auto synthesize_atom(
        const place_t &place,
        const apply_pattern &pattern,
        const places_t &places,
        const single_match_result &where,
        std::span< node_handle > children,
        egraph &graph
    ) -> node_handle {
        return where.matched_places.find(
            std::uint32_t(place_index(place, places))
        )->second.handle();
    }

    template< gap::graph::graph_like egraph >
    auto synthesize_atom(
        const label_t &label,
        const apply_pattern &pattern,
        const places_t &places,
        const single_match_result &where,
        std::span< node_handle > children,
        egraph &graph
    ) -> node_handle {
        return synthesize(
            get_expr_with_name(label, pattern).expr, pattern, places, where, children, graph
        );
    }

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const atom_t &atom,
        const apply_pattern &pattern,
        const places_t &places,
        const single_match_result &where,
        std::span< node_handle > children,
        egraph &graph
    ) -> node_handle {
        return std::visit([&] (const auto &a) {
            return synthesize_atom(a, pattern, places, where, children, graph);
        }, atom);
    }

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const expr_list &list,
        const apply_pattern &pattern,
        const places_t &places,
        const single_match_result &where,
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
