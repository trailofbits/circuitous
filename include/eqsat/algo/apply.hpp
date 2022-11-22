/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/pattern/rewrite_rule.hpp>
#include <eqsat/algo/synthesis.hpp>
#include <eqsat/core/egraph.hpp>

#include <gap/core/generator.hpp>
#include <gap/core/dense_map.hpp>

namespace eqsat {

    template< gap::graph::graph_like egraph >
    node_handle apply(
        const simple_expr &expr,
        const apply_pattern &rule,
        const places_t &places,
        const match_result &where,
        saturable_egraph< egraph > &graph
    ) {
        auto patch = synthesize_simple_expr(expr, rule, places, where, {}, graph);
        return graph.merge(where.root, graph.find(patch));
    }

    template< gap::graph::graph_like egraph >
    node_handle apply(
        const union_expr &un,
        const apply_pattern &rule,
        const places_t &places,
        const match_result &where,
        saturable_egraph< egraph > &graph
    ) {
        throw std::runtime_error("not implemented");
    }

    template< gap::graph::graph_like egraph >
    node_handle apply(
        const bond_expr &bond,
        const apply_pattern &rule,
        const places_t &places,
        const match_result &where,
        saturable_egraph< egraph > &graph
    ) {
        throw std::runtime_error("not implemented");
    }

    template< gap::graph::graph_like egraph >
    node_handle apply(
        const apply_action &action,
        const apply_pattern &rule,
        const places_t &places,
        const match_result &where,
        saturable_egraph< egraph > &graph
    ) {
        return std::visit([&] (const auto &a) {
            return apply(a, rule, places, where, graph);
        }, action);
    }

    template< gap::graph::graph_like egraph >
    node_handle apply(
        const apply_pattern &pattern,
        const places_t &places,
        const match_result &where,
        saturable_egraph< egraph > &graph
    ) {
        return apply(pattern.action, pattern, places, where, graph);
    }

    template< gap::graph::graph_like egraph >
    node_handle apply(
        const rewrite_rule &rule,
        const match_result &where,
        saturable_egraph< egraph > &graph
    ) {
        auto places = gather_places(rule.lhs);
        return apply(rule.rhs, places, where, graph);
    }

} // namespace eqsat
