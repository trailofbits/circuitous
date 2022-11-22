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
    ) -> typename egraph::node_pointer {
        return graph.make(constant);
    }

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const operation_t &operation,
        const apply_pattern &pattern,
        const match_result &where,
        egraph &graph
    ) -> typename egraph::node_pointer {
        return graph.make(operation);
    }

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const place_t &place,
        const apply_pattern &pattern,
        const match_result &where,
        egraph &graph
    ) -> typename egraph::node_pointer {
        throw std::runtime_error("not implemented place synthesis");
    }

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const label_t &label,
        const apply_pattern &pattern,
        const match_result &where,
        egraph &graph
    ) -> typename egraph::node_pointer {
        throw std::runtime_error("not implemented label synthesis");
    }

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const atom_t &atom,
        const apply_pattern &pattern,
        const match_result &where,
        egraph &graph
    ) -> typename egraph::node_pointer {
        return std::visit([&] (const auto &a) {
            return synthesize(a, pattern, where, graph);
        }, atom);
    }

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const expr_list &list,
        const apply_pattern &pattern,
        const match_result &where,
        egraph &graph
    ) -> typename egraph::node_pointer {
        if (is_nested_list(list)) {
            throw std::runtime_error("nested expression in rewrite pattern");
        }

        auto node = synthesize(list.front(), pattern, where, graph);

        for (const auto &child : tail(list)) {
            synthesize(child, pattern, where, graph);
            // TODO add children to node
        }

        return node;
    }

    template< gap::graph::graph_like egraph >
    auto synthesize(
        const simple_expr &expr,
        const apply_pattern &pattern,
        const match_result &where,
        egraph &graph
    ) -> typename egraph::node_pointer {
        return std::visit([&] (const auto &e) {
            return synthesize(e, pattern, where, graph);
        }, expr);
    }

} // namespace eqsat
