/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/pattern/rewrite_rule.hpp>

#include <string>

namespace eqsat {

    struct rule_set {
        std::string name;
        rewrite_rules rules;
    };

} // namespace eqsat
