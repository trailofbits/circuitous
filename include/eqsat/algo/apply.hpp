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
    struct saturable_egraph;

    template< gap::graph::graph_like egraph >
    struct applier {

        node_handle apply(const simple_expr &expr) {
            auto patch = synthesize(expr, rule.rhs, places, where, graph);
            auto root = std::get< single_match_result >(where).root;
            return graph.merge(root, graph.find(patch));
        }

        node_handle apply(const union_expr &un) {
            throw std::runtime_error("not implemented union expr");
        }

        node_handle apply(const bond_expr &bond) {
            throw std::runtime_error("not implemented bond expr");
        }

        node_handle apply(const apply_action &action) {
            return std::visit([&] (const auto &a) { return apply(a); }, action);
        }

        node_handle apply() { return apply(rule.rhs.action); }

        applier(const rewrite_rule &rule, const match_result &where, saturable_egraph< egraph > &graph)
            : rule(rule), places(gather_places(rule.lhs)), where(where), graph(graph)
        {}

        const rewrite_rule &rule;
        places_t places;
        const match_result &where;
        saturable_egraph< egraph > &graph;
    };

    template< gap::graph::graph_like egraph >
    node_handle apply(
        const rewrite_rule &rule,
        const match_result &where,
        saturable_egraph< egraph > &graph
    ) {
        spdlog::debug("[eqsat] applying rule {} at {}", rule, where);
        return applier(rule, where, graph).apply();
    }

} // namespace eqsat
