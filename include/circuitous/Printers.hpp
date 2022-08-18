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

    template <print::GraphColorer Colorer>
    struct DotPrinter
    {
      using value_map_t = std::unordered_map<Operation *, std::string>;

      DotPrinter( const value_map_t &nodeValues = {} ) : node_values(nodeValues)
      {
        colorer = Colorer();
      }

      DotPrinter(print::GraphColorer auto &&c,
                 const value_map_t &nodeValues = {} )
          : colorer(c),
            node_values(nodeValues) {}

      void operator()(std::ostream &os, Circuit *circuit)
      {
        print::print_dot(os, circuit, colorer, node_values);
      }

      Colorer colorer;
      const value_map_t node_values;
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
