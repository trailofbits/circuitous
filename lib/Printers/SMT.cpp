/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/IR/SMT.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <z3++.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ
{
  void PrintSMT(std::ostream &os, Circuit *circuit)
  {
    try {
      auto visitor = IRToSMTVisitor(circuit->ptr_size);
      auto expr = visitor.Visit(circuit);
      z3::solver solver(visitor.ctx);
      solver.add(expr);

      os << solver.to_smt2() << '\n';
    } catch (const z3::exception &e) {
      LOG(FATAL) << e.what() << '\n';
    }
  }

  void PrintBitBlastSMT(std::ostream &os, Circuit *circuit)
  {
    try {
      auto visitor = IRToBitBlastableSMTVisitor(circuit->ptr_size);
      auto expr = visitor.Visit(circuit);
      auto bitblasted = bitblast(expr, visitor.ctx);
      os << bitblasted.to_smt2() << '\n';
    } catch (const z3::exception &e) {
      LOG(FATAL) << e.what() << '\n';
    }
  }

}  // namespace circ
