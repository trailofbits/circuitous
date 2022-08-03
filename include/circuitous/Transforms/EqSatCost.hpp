/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Transforms/EGraph.hpp>

#include <eqsat/core/cost_graph.hpp>

namespace circ::eqsat
{
    struct CircuitCostFunction {
        using Cost    = ::eqsat::cost_t;
        using NodePtr = CircuitEGraph::node_pointer;

        Cost operator()(const NodePtr node) {
            // TODO(Heno) implement cost function
            // if (name(node) == "Mul") {
            //   return 1000;  // * bitwidth(node).value();
            // }
            // if (name(node) == "Add") {
            //   return 100;  // * bitwidth(node).value();
            // }
            return 1;
        }
    };

    using CircuitCostGraph = ::eqsat::cost_graph< CircuitEGraph, CircuitCostFunction >;
    static_assert(gap::graph::graph_like< CircuitCostGraph >);

    auto make_circuit_cost_graph(CircuitEGraph &&graph) -> CircuitCostGraph {
        return CircuitCostGraph(std::move(graph), CircuitCostFunction{});
    }

    using OptimalCircuitGraphView = ::eqsat::optimal_graph_view< CircuitEGraph, CircuitCostFunction >;
    static_assert(gap::graph::graph_like< OptimalCircuitGraphView >);

    auto make_optimal_circuit_graph(CircuitEGraph &&graph) -> OptimalCircuitGraphView {
        return OptimalCircuitGraphView(std::move(graph), CircuitCostFunction{});
    }

} // namespace circ::eqsat
