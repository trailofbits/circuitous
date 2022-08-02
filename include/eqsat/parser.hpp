/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Transforms/eqsat/rule_set.hpp>

#include <string>

namespace eqsat {

    std::vector< rule_set > parse_rules(std::istream &is);

} // namespace eqsat
