/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Transforms/EGraph.hpp>
#include <circuitous/Transforms/EGraphBuilder.hpp>
#include <circuitous/Transforms/EqSatCost.hpp>
#include <circuitous/Transforms/EqualitySaturation.hpp>
#include <eqsat/algo/saturation.hpp>
#include <eqsat/core/egraph.hpp>

namespace circ
{
    EGraphBuilderState make_circuit_egraph(const CircuitPtr &circuit) {
        EGraphBuilder builder;
        return builder.build(circuit.get());
    };

    CircuitPtr run_equality_saturation(CircuitPtr &&circuit, std::span< RuleSet > rules) {
        spdlog::debug("[eqsat] start equality saturation");
        auto [egraph, nodes_map] = make_circuit_egraph(circuit);

        auto [saturation, status] = eqsat::saturate(std::move(egraph), rules);

        auto optimal = make_optimal_circuit_graph(std::move(saturation));
        spdlog::debug("[eqsat] stop equality saturation");
        return nullptr; // TODO circuit from optimal
    }

} // namespace circ
