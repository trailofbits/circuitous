/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/pattern/rewrite_rule.hpp>
#include <eqsat/core/egraph.hpp>

#include <gap/core/generator.hpp>
#include <gap/core/dense_map.hpp>

namespace eqsat {

    template< gap::graph::graph_like egraph >
    void apply(
        const apply_pattern &rule,
        const match_result &where,
        const egraph &graph
    ) {
        /* TODO */
    }

    template< gap::graph::graph_like egraph >
    void apply(
        const rewrite_rule &rule,
        const match_result &where,
        const egraph &graph
    ) {
        apply(rule.rhs, where, graph);
    }

} // namespace eqsat
