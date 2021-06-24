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

void PrintDOT(std::ostream &os, Circuit *circuit,
              const std::unordered_map<Operation *, std::string> & = {});
void PrintPython(std::ostream &os, Circuit *circuit);
void PrintJSON(std::ostream &os, Circuit *circuit);
void PrintTopology(std::ostream &os, Operation *op, unsigned max_depth,
                   std::function<bool(Operation *)> accept);
void PrintSMT(std::ostream &os, Circuit *circuit, bool bit_blast);
}  // namespace circ
