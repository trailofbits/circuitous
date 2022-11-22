/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <gap/core/graph.hpp>
#include <gap/core/concepts.hpp>

#include <eqsat/pattern/pattern.hpp>

namespace eqsat {

    template< gap::graph::graph_like egraph, typename builder >
    struct extendable_egraph : egraph {
        using node_pointer = typename egraph::node_pointer;

        using egraph::egraph;

        explicit extendable_egraph(egraph &&graph)
            : egraph(std::forward< egraph >(graph))
        {}

        node_pointer make(const constant_t &con) {
            return add_node(builder::make(con));
        }

        using egraph::add_node;

        node_pointer make(const operation_t &op) {
            return add_node(builder::make(op));
        }
    };

} // namespace eqsat
