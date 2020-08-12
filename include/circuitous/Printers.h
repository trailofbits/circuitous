/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <functional>
#include <ostream>

namespace circuitous {

class Circuit;
class Operation;

void PrintDOT(std::ostream &os, Circuit *circuit);
void PrintPython(std::ostream &os, Circuit *circuit);
void PrintJSON(std::ostream &os, Circuit *circuit);
void PrintTopology(std::ostream &os, Operation *op, unsigned max_depth,
                   std::function<bool(Operation *)> accept);
void PrintSMT(std::ostream &os, Circuit *circuit);
}  // namespace circuitous
