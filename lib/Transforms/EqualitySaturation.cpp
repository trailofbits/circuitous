/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Transforms/EqualitySaturation.hpp>
#include <circuitous/Transforms/EqSatCost.hpp>
#include <circuitous/Transforms/EGraph.hpp>
#include <circuitous/Transforms/EGraphBuilder.hpp>

#include <eqsat/core/egraph.hpp>
#include <eqsat/algo/saturation.hpp>

namespace circ::eqsat
{
    EGraphBuilderState make_circuit_egraph(const CircuitPtr &circuit) {
        EGraphBuilder builder;
        return builder.build(circuit.get());
    };

    CircuitPtr run_equality_saturation(CircuitPtr &&circuit, std::span< RuleSet > rules) {
        log_info() << "Start equality saturation";
        auto [egraph, nodes_map] = make_circuit_egraph(circuit);

        auto saturation = ::eqsat::saturation(std::move(egraph));
        // TODO use some algorithm
        saturation.apply(rules);

        auto optimal = make_optimal_circuit_graph(std::move(saturation));
        log_info() << "Equality saturation stopped";
        return nullptr; // TODO circuit from optimal
    }

} // namespace circ::eqsat
