/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/pattern/rule_set.hpp>

#include <string>
#include <string_view>
#include <vector>

namespace eqsat
{
    std::vector< rule_set > parse_rules(std::istream &is);

    std::vector< rule_set > parse_rules(const std::string &filename);

} // namespace eqsat
