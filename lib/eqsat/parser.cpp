/*
 * Copyright (c) 2022-Present Trail of Bits, Inc.
 */

#include <eqsat/pattern/parser.hpp>

#include <fstream>

namespace eqsat
{
    std::vector< rule_set > parse_rules(const std::string &filename) {
        std::ifstream file(filename, std::ios::in);
        return parse_rules(file);
    }

    std::vector< rule_set > parse_rules(std::istream &is) {
        return {}; // TODO
    }

} // namespace eqsat
