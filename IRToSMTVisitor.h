/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */
#pragma once

#include <z3++.h>
#include <unordered_map>
#include "IR.h"

namespace circuitous {

class IRToSMTVisitor : public UniqueVisitor<IRToSMTVisitor> {
 private:
  z3::context &z3_ctx;
  // Expression map
  z3::expr_vector z3_expr_vec;
  std::unordered_map<Operation *, unsigned> z3_expr_map;
  void InsertZ3Expr(Operation *op, z3::expr z3_expr);
  z3::expr GetZ3Expr(Operation *op);

 public:
  IRToSMTVisitor(z3::context &ctx);
  void VisitInputInstructionBits(InputInstructionBits *op);
  void VisitInputRegister(InputRegister *op);
  void VisitOutputRegister(OutputRegister *op);
  void VisitConstant(Constant *op);
  void VisitLLVMOperation(LLVMOperation *op);
  void VisitExtract(Extract *op);
  void VisitRegisterCondition(RegisterCondition *op);
  void VisitPreservedCondition(PreservedCondition *op);
  void VisitDecodeCondition(DecodeCondition *op);
  void VisitOnlyOneCondition(OnlyOneCondition *op);
  void VisitVerifyInstruction(VerifyInstruction *op);
  void VisitCircuit(Circuit *op);

  z3::expr GetOrCreateZ3Expr(Operation *op);
};

void PrintSMT(std::ostream &os, Circuit *circuit);

}  // namespace circuitous
