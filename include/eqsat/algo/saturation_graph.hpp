/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/pattern/rewrite_rule.hpp>

namespace eqsat {

    template< gap::graph::graph_like egraph >
    struct saturable_egraph : egraph {

        explicit saturable_egraph(egraph &&graph)
            : egraph(std::forward< egraph >(graph))
        {}

        // return value of equality saturation
        enum class stop_reason
        {
            saturated, iteration_limit, node_limit, time_limit, unknown
        };
    };

} // namespace eqsat
