/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/pattern/pattern.hpp>

#include <string>
#include <vector>

namespace eqsat {

    static inline match_pattern make_match_pattern(std::string_view pat) {
        if (auto res = parse_match_pattern(pat))
            return res.value();
        throw std::runtime_error("syntax error in match pattern: " + std::string(pat));
    }

    static inline apply_pattern make_apply_pattern(std::string_view pat) {
        if (auto res = parse_apply_pattern(pat))
            return res.value();
        throw std::runtime_error("syntax error in apply pattern: " + std::string(pat));
    }

    // TODO implement rewrite rule parser
    struct rewrite_rule {
        rewrite_rule(std::string_view name, std::string_view lhs, std::string_view rhs)
            : name(name)
            , lhs(make_match_pattern(lhs))
            , rhs(make_apply_pattern(rhs))
            //, places(get_indexed_places(lhs))
        {}

        const std::string name;

        // Rewrite rule 'lhs -> rhs' that allows to match
        // left-hand-side and replace it with right-hand-side
        match_pattern lhs;
        apply_pattern rhs;

        // Places that occur in the rewrite pattern
        // Note: it is required that place occurs on the left hand side
        // of the rule when it occurs on the right hand side
        // indexed_places places;
    };

    using rewrite_rules = std::vector< rewrite_rule >;

} // namespace eqsat
