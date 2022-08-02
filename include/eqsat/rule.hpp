/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/ADT/EGraph.hpp>
#include <circuitous/Transforms/eqsat/pattern.hpp>

namespace eqsat {

    template< gap::graph::graph_like egraph >
    struct rewrite_rule {
        rewrite_rule(const std::string &name,
                     const std::string &lhs_,
                     const std::string &rhs_)
            : name(name)
            , lhs(make_pattern(lhs_))
            , rhs(make_pattern(rhs_))
            , places(get_indexed_places(lhs))
        {}

        const std::string name;
        // Rewrite rule 'lhs -> rhs' that allows to match
        // left-hand-side and replace it with right-hand-side
        Pattern lhs;
        Pattern rhs;

        // Places that occur in the rewrite pattern
        // Note: it is required that place occurs on the left hand side
        // of the rule when it occurs on the right hand side
        indexed_places places;
    };

    template< gap::graph::graph_like egraph >
    using rewrite_rules = std::vector< rewrite_rule< egraph > >;


} // namespace eqsat
