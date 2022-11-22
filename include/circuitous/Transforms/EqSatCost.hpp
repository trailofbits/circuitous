/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Transforms/EGraph.hpp>

#include <eqsat/core/cost_graph.hpp>

namespace circ
{
    struct circuit_cost_function {

        eqsat::cost_t operator()(const circuit_egraph::node_pointer node) {
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

    using circuit_cost_graph = eqsat::cost_graph<
        circuit_egraph, circuit_cost_function
    >;

    static_assert(gap::graph::graph_like< circuit_cost_graph >);

    auto make_circuit_cost_graph(circuit_egraph &&graph) -> circuit_cost_graph {
        return circuit_cost_graph(std::move(graph), circuit_cost_function{});
    }

    using optimal_circuit_graph_view = eqsat::optimal_graph_view<
        circuit_egraph, circuit_cost_function
    >;

    static_assert(gap::graph::graph_like< optimal_circuit_graph_view >);

    auto make_optimal_circuit_graph(circuit_egraph &&graph)
        -> optimal_circuit_graph_view
    {
        return optimal_circuit_graph_view(std::move(graph), circuit_cost_function{});
    }

} // namespace circ
