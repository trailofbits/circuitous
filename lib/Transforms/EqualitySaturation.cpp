/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Transforms/EqualitySaturation.hpp>
#include <circuitous/Transforms/EqSatCost.hpp>
#include <circuitous/Transforms/EGraph.hpp>

#include <eqsat/core/egraph.hpp>
#include <eqsat/algo/saturation.hpp>

namespace circ
{
    CircuitEGraph make_circuit_egraph(CircuitPtr &&circuit) {
        return CircuitEGraph{}; // TODO synthesize EGraph
    };

    CircuitPtr run_equality_saturation(CircuitPtr &&circuit, std::span< RuleSet > rules) {
        log_info() << "Start equality saturation";
        auto egraph = ::eqsat::saturation(make_circuit_egraph(std::move(circuit)));
        // TODO run eqsat
        auto optimal = make_optimal_circuit_graph(std::move(egraph));
        log_info() << "Equality saturation stopped";
        return nullptr; // TODO circuit from optimal
    }

} // namespace circ
