/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <ostream>
#include <functional>

namespace circuitous {

class Circuit;
class Operation;

void PrintDOT(std::ostream &os, Circuit *circuit);
void PrintPython(std::ostream &os, Circuit *circuit);
void PrintTopology(std::ostream &os, Operation *op, unsigned max_depth,
                   std::function<bool(Operation *)> accept);

}  // namespace circuitous
