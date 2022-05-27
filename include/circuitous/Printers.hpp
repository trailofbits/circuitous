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

namespace circ {

    struct Circuit;
    struct Operation;

    void print_dot(std::ostream &os, Circuit *circuit,
                    const std::unordered_map<Operation *, std::string> & = {}, const std::vector<std::string> &highlights = std::vector<std::string>());

    void print_json(std::ostream &os, Circuit *circuit);
    void print_smt(std::ostream &os, Circuit *circuit);
    void print_bitblasted_smt(std::ostream &os, Circuit *circuit);
    void print_verilog(std::ostream &os, const std::string &name, Circuit *circuit);


    template< typename Printer, typename ... Args >
    void print_circuit(std::string_view filename, Printer printer, Args &&... args  )
    {
        std::ofstream file(std::string{filename});
        check(file);
        printer(file, std::forward< Args >(args) ...);
        file.flush();
    }

}  // namespace circ
