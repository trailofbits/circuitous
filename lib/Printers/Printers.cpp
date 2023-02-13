/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#include <circuitous/Printers/Verilog.hpp>
#include <circuitous/Printers.hpp>

#include <ostream>

namespace circ
{

    void VerilogPrinter::operator()( std::ostream &os, circ::Circuit *circuit )
    {
        return print::verilog::print( os, name, circuit, switch_as_mux );
    }

}  // namespace circ
