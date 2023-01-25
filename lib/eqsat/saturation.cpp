/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#include <eqsat/algo/saturation.hpp>

namespace eqsat {

    std::string to_string(stop_reason reason) {
        switch (reason) {
            case stop_reason::saturated: return "saturated";
            case stop_reason::iteration_limit: return "iteration limit";
            case stop_reason::node_limit: return "node limit";
            case stop_reason::time_limit: return "time limit";
            case stop_reason::unknown: return "unkown";
            case stop_reason::none: return "none";
        }
    }

} // namespace eqsat
