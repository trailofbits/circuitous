/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#include <circuitous/Printers/Verilog.hpp>

#include <ostream>

namespace circ {

    void print_verilog(std::ostream &os, const std::string &name, Circuit *circuit)
    {
        return print::verilog::print(os, name, circuit);
    }

}  // namespace circ
