/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <functional>
#include <fstream>
#include <ostream>
#include <string>
#include <unordered_map>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Printers/Dot.hpp>

namespace circ {

    struct Circuit;
    struct Operation;

    void print_json(std::ostream &os, Circuit *circuit);
    void print_smt(std::ostream &os, Circuit *circuit);
    void print_bitblasted_smt(std::ostream &os, Circuit *circuit);

    struct VerilogPrinter
    {
        VerilogPrinter( const std::string &name ) : name( name ) { }
        const std::string &name;

        void operator()(std::ostream &os, Circuit *circuit);
    };

    template < print::GraphColorer Colorer >
    struct DotPrinter
    {
        Colorer colorer;
        DotPrinter() { colorer = Colorer(); }
        DotPrinter( print::GraphColorer auto&& c ) : colorer( c ) { }

        void operator()( std::ostream &os, Circuit *circuit )
        {
            print::print_dot( os, circuit, colorer );
        }
    };

    template< typename Printer>
    void print_circuit( std::string_view filename, Printer&& printer, Circuit *circuit)
    {
        std::ofstream file(std::string{filename});
        check(file);
        printer(file, circuit);
        file.flush();
    }

}  // namespace circ
