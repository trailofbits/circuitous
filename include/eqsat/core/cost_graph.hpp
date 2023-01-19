/*
 * Copyright (c) 2022-present Trail of Bits, Inc.
 */

#pragma once

#include <gap/core/graph.hpp>
#include <gap/core/concepts.hpp>
#include <gap/core/memoize.hpp>

namespace eqsat {

    using cost_t = double;

    template< gap::graph::graph_like base_graph, typename cost_function_t >
    struct cost_graph : base_graph {

        using memoized_cost_function = decltype(
            gap::memoize(std::declval< cost_function_t >())
        );

        cost_graph(cost_graph &&other) = default;
        cost_graph& operator=(cost_graph &&other) = default;

        cost_graph(base_graph &&base, cost_function_t &&cost_function)
            : base_graph(std::move(base))
            , cost_function(
                gap::memoize(std::forward< cost_function_t >(cost_function))
            )
        {}

        using node_pointer = typename base_graph::node_pointer;
        using edge_type    = typename base_graph::edge_type;

        using base_graph::nodes;

        cost_t cost(node_pointer node) const {
            return const_cast< cost_graph* >(this)->cost_function(node);
        }

        memoized_cost_function cost_function;
    };

    template< gap::graph::graph_like base_graph, typename cost_function_t >
    struct optimal_graph_view : cost_graph< base_graph, cost_function_t > {
        using cost_graph = cost_graph< base_graph, cost_function_t >;
        using node_pointer = typename cost_graph::node_pointer;

        optimal_graph_view(optimal_graph_view &&other) = default;
        optimal_graph_view& operator=(optimal_graph_view &&other) = default;

        optimal_graph_view(base_graph &&base, cost_function_t &&cost_function)
            : cost_graph(
                std::forward< base_graph >(base),
                std::forward< cost_function_t >(cost_function)
            )
        {}

        using cost_graph::eclass;
        using cost_graph::cost;

        struct optimal_node {
            gap::generator< optimal_node > children() const {
                for (auto ch : node->children()) {
                    co_yield graph.node(ch);
                }
            }

            const optimal_graph_view &graph;
            node_pointer node;
        };

        optimal_node node(graph::node_handle handle) const {
            auto minimal = std::numeric_limits< cost_t >::max();
            node_pointer minimal_node = nullptr;

            for (auto node : eclass(handle).nodes) {
                if (auto c = cost(node); c < minimal) {
                    minimal = c;
                    minimal_node = node;
                }
            }

            return { *this, minimal_node };
        }
    };

} // namespace eqsat
