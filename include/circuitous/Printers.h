/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <functional>
#include <ostream>
#include <string>
#include <unordered_map>

namespace circ {

    struct Circuit;
    struct Operation;


    void print_dot(std::ostream &os, Circuit *circuit,
                    const std::unordered_map<Operation *, std::string> & = {});

    void print_json(std::ostream &os, Circuit *circuit);
    void print_topology(std::ostream &os, Operation *op, unsigned max_depth,
                       std::function<bool(Operation *)> accept);
    void print_smt(std::ostream &os, Circuit *circuit);
    void print_bitblasted_smt(std::ostream &os, Circuit *circuit);
    void print_verilog(std::ostream &os, const std::string &name, Circuit *circuit);


    template< typename Printer, typename ... Args >
    void print_circuit(std::string_view filename, Printer printer, Args &&... args  )
    {
        std::fstream file(filename);
        printer(file, std::forward< Args >(args) ...);
    }



}  // namespace circ
