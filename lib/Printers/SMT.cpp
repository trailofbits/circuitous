/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <z3++.h>
#include <circuitous/IR/SMT.hpp>

namespace circ
{
  void PrintSMT(std::ostream &os, Circuit *circuit)
  {
    try {
      auto visitor = IRToSMTVisitor();
      auto expr = visitor.Visit(circuit);
      z3::solver solver(visitor.ctx);
      solver.add(expr);

      os << solver.to_smt2() << '\n';
    } catch (const z3::exception &e) {
      LOG(FATAL) << e.what() << '\n';
    }
  }

  z3::solver bitblast(z3::expr expr, z3::context &ctx)
  {
    z3::solver solver(ctx);

    z3::tactic tactic(
      z3::tactic(ctx, "ctx-simplify") &
      z3::tactic(ctx, "propagate-bv-bounds") &
      z3::tactic(ctx, "solve-eqs") &
      z3::tactic(ctx, "simplify") &
      z3::tactic(ctx, "bit-blast") &
      z3::tactic(ctx, "aig")
    );

    z3::goal goal(ctx);
    goal.add(expr);
    expr = tactic(goal)[0].as_expr();

    // Custom optimizations

    solver.add(expr);
    return solver;
  }

  void PrintBitBlastSMT(std::ostream &os, Circuit *circuit)
  {
    try {
      auto visitor = IRToBitBlastableSMTVisitor();
      auto expr = visitor.Visit(circuit);
      os << bitblast(expr, visitor.ctx).to_smt2() << '\n';
    } catch (const z3::exception &e) {
      LOG(FATAL) << e.what() << '\n';
    }
  }

}  // namespace circ
