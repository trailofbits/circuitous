/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/pattern/pattern.hpp>

#include <string>
#include <vector>

namespace eqsat {

    struct rewrite_rule {
        rewrite_rule(std::string_view name, std::string_view lhs, std::string_view rhs)
            : name(name)
            //, lhs(make_pattern(lhs_))
            //, rhs(make_pattern(rhs_))
            //, places(get_indexed_places(lhs))
        {}

        const std::string name;

        // Rewrite rule 'lhs -> rhs' that allows to match
        // left-hand-side and replace it with right-hand-side
        // Pattern lhs;
        // Pattern rhs;

        // Places that occur in the rewrite pattern
        // Note: it is required that place occurs on the left hand side
        // of the rule when it occurs on the right hand side
        // indexed_places places;
    };

    using rewrite_rules = std::vector< rewrite_rule >;

} // namespace eqsat
