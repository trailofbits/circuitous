/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Transforms/EGraph.hpp>
#include <circuitous/Transforms/EGraphBuilder.hpp>
#include <circuitous/Transforms/CircuitBuilder.hpp>
#include <circuitous/Transforms/EqSatCost.hpp>
#include <circuitous/Transforms/EqualitySaturation.hpp>
#include <eqsat/algo/saturation.hpp>
#include <eqsat/algo/print.hpp>
#include <eqsat/core/egraph.hpp>




namespace circ
{
    egraph_builder_state make_circuit_egraph(const circuit_owner_t &circuit) {
        circuit_egraph_builder builder;
        return builder.build(circuit.get());
    };

    using node_pointer_t = circuit_egraph::node_pointer;

    using circuit_saturable_egraph = eqsat::saturable_egraph< circuit_egraph >;

    circuit_owner_t run_equality_saturation(circuit_owner_t &&circuit, std::span< eqsat::rule_set > rules) {

        spdlog::debug("[eqsat] start equality saturation");
        auto [graph, nodes_map] = make_circuit_egraph(circuit);

        auto [saturated, status] = eqsat::saturate(
            circuit_saturable_egraph(std::move(graph)), rules
        );

        auto optimal = make_optimal_circuit_graph(std::move(saturated));
        spdlog::debug("[eqsat] stop equality saturation");

        auto root = nodes_map.at(circuit->root);
        return extract_circuit_from_egraph(optimal).extract(root, circuit->ptr_size);
    }

} // namespace circ
