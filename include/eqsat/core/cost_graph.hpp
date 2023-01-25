/*
 * Copyright (c) 2022-present Trail of Bits, Inc.
 */

#pragma once

#include <gap/core/graph.hpp>
#include <gap/core/concepts.hpp>
#include <gap/core/memoize.hpp>

#include <unordered_set>
#include <unordered_map>

namespace eqsat {

    using cost_t = double;

    template< gap::graph::graph_like base_graph, typename cost_function_t >
    struct cost_graph : base_graph {

        using memoized_cost_function = decltype(
            gap::memoize(std::declval< cost_function_t >())
        );

        using node_pointer = typename base_graph::node_pointer;
        using edge_type    = typename base_graph::edge_type;

        using base_graph::nodes;
        using base_graph::eclass;

        using nodes_set = std::unordered_set< node_pointer >;
        using cost_map  = std::unordered_map< node_pointer, cost_t >;

        struct cost_node { cost_t cost; node_pointer node; };

        // state of dfs algorith to detect loops in egraph
        struct graph_explore_state {
            enum class node_state { open, closed };

            std::unordered_map< node_pointer, node_state > nodes;

            void open(node_pointer node) {
                nodes[node] = node_state::open;
            }

            void close(node_pointer node) {
                assert(nodes[node] == node_state::open);
                nodes[node] = node_state::closed;
            }

            bool is_loop_edge(node_pointer node) const {
                return nodes.contains(node) && nodes.at(node) == node_state::open;
            }

            bool contains(node_pointer node) const {
                return nodes.contains(node);
            }
        };

        cost_graph(cost_graph &&other) = default;
        cost_graph& operator=(cost_graph &&other) = default;

        cost_graph(const cost_graph &other) = delete;
        cost_graph& operator=(const cost_graph &other) = delete;

        cost_graph(base_graph &&base, cost_function_t &&cost_function)
            : base_graph(std::move(base))
            , cost_function(
                gap::memoize(std::forward< cost_function_t >(cost_function))
            )
        {
            // precompute loop nodes to be ommited from cost computation
            {
                graph_explore_state seen;
                for (auto node : nodes()) {
                    if (!seen.contains(node)) {
                        nodes_on_loops_from(node, seen, [&] (auto n) {
                            nodes_on_loops.insert(n);
                        });
                    }
                }
            }

            // compute cost map for quick queries later
            {
                nodes_set seen;
                for (auto node : nodes()) {
                    if (!costs.contains(node)) {
                        // we compute costs in postorder for dependent cost computations
                        for (auto n : toposort(node, seen)) {
                            costs[n] = init_minimal_cost(n);
                        }
                    }
                }
            }
        }

    private:

        cost_t init_minimal_cost(node_pointer node) const {
            cost_t children_cost = 0;

            for (auto child_class : node->children()) {
                children_cost += minimal_cost(child_class).cost;
            }

            return children_cost + cost_function(node);
        }

    protected:

        template< typename Yield >
        void nodes_on_loops_from(node_pointer node, graph_explore_state &seen, Yield yield) const {
            seen.open(node);

            for (auto child_class : node->children()) {
                for (auto child : eclass(child_class).nodes) {
                    if (seen.is_loop_edge(child)) {
                        yield(node);
                    } else {
                        nodes_on_loops_from(child, seen, yield);
                    }
                }
            }

            seen.close(node);
        }

        gap::recursive_generator< node_pointer > toposort(node_pointer root, nodes_set &seen) {
            if (nodes_on_loops.contains(root)) {
                co_return;
            }

            seen.insert(root);

            for (auto child_class : root->children()) {
                for (auto child : eclass(child_class).nodes) {
                    if (!seen.count(child)) {
                        co_yield toposort(child, seen);
                    }
                }
            }

            co_yield root;
        }

        std::optional< cost_node > minimal_cost(node_pointer node) const {
            if (auto it = costs.find(node); it != costs.end())
                return {{ it->second, node }};
            return std::nullopt;
        }

        // Returns minimal cost node for given equality class
        //
        // Each eclass should have at least one non-loop node,
        // therefore the cost_node is not `optional` as in previous function.
        cost_node minimal_cost(graph::node_handle handle) const {
            std::optional< cost_node > min = std::nullopt;
            for (auto node : eclass(handle).nodes) {
                if (auto curr = minimal_cost(node)) {
                    if (!min.has_value() || curr->cost < min->cost) {
                        std::swap(min, curr);
                    }
                }
            }
            return min.value();
        }

        cost_map costs;
        nodes_set nodes_on_loops;
        memoized_cost_function cost_function;
    };

    template< gap::graph::graph_like base_graph, typename cost_function_t >
    struct optimal_graph_view : cost_graph< base_graph, cost_function_t > {
        using cost_graph = cost_graph< base_graph, cost_function_t >;
        using node_pointer = typename cost_graph::node_pointer;

        using cost_graph::find;

        using cost_graph::eclass;
        using cost_graph::eclasses;
        using cost_graph::minimal_cost;

        using cost_node = typename cost_graph::cost_node;

        optimal_graph_view(optimal_graph_view &&other) = default;
        optimal_graph_view& operator=(optimal_graph_view &&other) = default;

        optimal_graph_view(base_graph &&base, cost_function_t &&cost_function)
            : cost_graph(
                std::forward< base_graph >(base),
                std::forward< cost_function_t >(cost_function)
            )
        {
            for (const auto &[handle, _] : eclasses()) {
                optimal_nodes[handle] = minimal_cost(handle);
            }
        }

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
            auto [cost, minimal_node] = optimal_nodes.at(find(handle));
            return { *this, minimal_node };
        }

        void print_costs(graph::node_handle handle, node_pointer minimal_node, cost_t cost) const {
            spdlog::debug("[eqsat] costs minimal {} with cost {} from:", node_name(*minimal_node), cost);
            for (auto node : eclass(handle).nodes) {
                if (auto c = minimal_cost(node)) {
                    spdlog::debug("[eqsat] {} = {}", node_name(*node), c->cost);
                } else {
                    spdlog::debug("[eqsat] {} = inf", node_name(*node));
                }
            }
        }

        using handle_hash = gap::hash< graph::node_handle >;
        std::unordered_map< graph::node_handle, cost_node, handle_hash > optimal_nodes;
    };

} // namespace eqsat
