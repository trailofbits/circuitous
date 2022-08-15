/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/algo/saturation_graph.hpp>

#include <eqsat/core/egraph.hpp>
#include <eqsat/core/cost_graph.hpp>

#include <eqsat/pattern/rule_set.hpp>


namespace eqsat
{
    // return value of equality saturation
    enum class stop_reason
    {
        saturated, iteration_limit, node_limit, time_limit, unknown, none
    };

    template< gap::graph::graph_like egraph >
    using saturation_result = std::pair< saturable_egraph< egraph >, stop_reason >;

    //
    // step of equality saturation
    //

    template< gap::graph::graph_like egraph >
    saturable_egraph< egraph > match_and_apply(
        saturable_egraph< egraph > &&graph,
        const rewrite_rule &rule
    ) {
        // TODO(Heno) match & apply
        return graph;
    }

    template< gap::graph::graph_like egraph >
    saturation_result< egraph > make_step(
        saturable_egraph< egraph > &&graph,
        std::span< rule_set > sets
    ) {
        spdlog::debug("[eqsat] step");
        // TODO(Heno paralelize)

        for (const auto &set : sets) {
            for (const auto &rule : set.rules) {
                graph = match_and_apply(std::move(graph), rule);
            }
        }

        return { std::move(graph), stop_reason::unknown };
    }

    //
    // generic saturation algorithm
    //
    template< gap::graph::graph_like egraph >
    saturation_result< egraph > saturate(
        saturable_egraph< egraph > &&graph,
        std::span< rule_set > rules
    ) {
        // egraph.rebuild()

        stop_reason status = stop_reason::none;
        while (status != stop_reason::none) {
            auto [g, s] = make_step(std::move(graph), rules);
            graph = std::move(g);
            status = s;
        }

        return { std::move(graph), status };
    }

    template< gap::graph::graph_like egraph >
    saturation_result< egraph > saturate( egraph &&graph, std::span< rule_set > rules) {
        return saturate( saturable_egraph(std::forward< egraph >(graph)), rules);
    }

} // namespace eqsat
