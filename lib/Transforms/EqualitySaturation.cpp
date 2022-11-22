/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Transforms/EGraph.hpp>
#include <circuitous/Transforms/EGraphBuilder.hpp>
#include <circuitous/Transforms/EqSatCost.hpp>
#include <circuitous/Transforms/EqualitySaturation.hpp>
#include <eqsat/algo/saturation.hpp>
#include <eqsat/algo/print.hpp>
#include <eqsat/core/egraph.hpp>
#include <eqsat/core/extend.hpp>

namespace circ
{
    egraph_builder_state make_circuit_egraph(const CircuitPtr &circuit) {
        circuit_egraph_builder builder;
        return builder.build(circuit.get());
    };

    using node_pointer_t = circuit_egraph::node_pointer;

    struct circuit_graph_from_pattern_builder {
        using storage_type = typename circuit_egraph::storage_type;

        static storage_type make(const eqsat::constant_t &con) {
            // TODO build storage
            throw std::runtime_error("not implemented constant synthesis");
        }

        static storage_type make(const eqsat::operation_t &op) {
            // TODO build storage
            throw std::runtime_error("not implemented operation synthesis");

        }
    };

    using circuit_extendable_egraph = eqsat::extendable_egraph<
        circuit_egraph, circuit_graph_from_pattern_builder
    >;

    CircuitPtr run_equality_saturation(CircuitPtr &&circuit, std::span< eqsat::rule_set > rules) {
        spdlog::debug("[eqsat] start equality saturation");
        auto [graph, nodes_map] = make_circuit_egraph(circuit);

        auto [saturated, status] = eqsat::saturate(
            circuit_extendable_egraph(std::move(graph)), rules
        );

        eqsat::to_dot(saturated, "initial.dot");

        auto optimal = make_optimal_circuit_graph(std::move(saturated));
        spdlog::debug("[eqsat] stop equality saturation");
        return nullptr; // TODO circuit from optimal
    }

} // namespace circ
