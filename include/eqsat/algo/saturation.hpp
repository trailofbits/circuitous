/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/algo/saturation_graph.hpp>

#include <eqsat/core/egraph.hpp>
#include <eqsat/core/cost_graph.hpp>

#include <eqsat/pattern/rule_set.hpp>


namespace eqsat
{
    template< gap::graph::graph_like egraph >
    struct saturation : saturable_egraph< egraph > {
        using base = saturable_egraph< egraph >;

        explicit saturation(egraph &&graph)
            : base(std::forward< egraph >(graph))
        {}

        typename base::stop_reason apply( std::span< rule_set > ) {
            return base::stop_reason::unknown;
        }
    };

} // namespace eqsat
