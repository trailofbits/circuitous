/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <glog/logging.h>

#include <llvm/IR/InstrTypes.h>

#include <memory>

#include "IRToSMTVisitor.h"

namespace circuitous {

IRToSMTVisitor::IRToSMTVisitor() : z3_expr_vec(z3_ctx) {}

void IRToSMTVisitor::InsertZ3Expr(Operation *op, z3::expr z3_expr) {
  auto iter = z3_expr_map.find(op);
  CHECK(iter == z3_expr_map.end());
  z3_expr_map[op] = z3_expr_vec.size();
  z3_expr_vec.push_back(z3_expr);
}

z3::expr IRToSMTVisitor::GetZ3Expr(Operation *op) {
  auto iter = z3_expr_map.find(op);
  CHECK(iter != z3_expr_map.end());
  return z3_expr_vec[static_cast<int>(iter->second)];
}

void IRToSMTVisitor::VisitInputInstructionBits(InputInstructionBits *op) {
  DLOG(INFO) << "VisitInputInstructionBits: " << op->Name();
  InsertZ3Expr(op, z3_ctx.bv_const("InputInst", op->size));
}

void IRToSMTVisitor::VisitInputRegister(InputRegister *op) {
  DLOG(INFO) << "VisitInputRegister: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  auto name = "Input" + op->reg_name;
  InsertZ3Expr(op, z3_ctx.bv_const(name.c_str(), op->size));
}

void IRToSMTVisitor::VisitOutputRegister(OutputRegister *op) {
  DLOG(INFO) << "VisitOutputRegister: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  auto name = "Output" + op->reg_name;
  InsertZ3Expr(op, z3_ctx.bv_const(name.c_str(), op->size));
}

void IRToSMTVisitor::VisitConstant(Constant *op) {
  DLOG(INFO) << "VisitConstant: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  std::unique_ptr<bool[]> bits(new bool[op->size]);
  for (auto i = 0U; i < op->size; ++i) {
    bits[i] = op->bits[op->size - i - 1] == '0' ? false : true;
  }
  InsertZ3Expr(op, z3_ctx.bv_val(op->size, bits.get()));
}

void IRToSMTVisitor::VisitLLVMOperation(LLVMOperation *op) {
  DLOG(INFO) << "VisitLLVMOperation: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  switch (op->llvm_op_code) {
    case llvm::BinaryOperator::And: {
      auto lhs = GetZ3Expr(op->operands[0]);
      auto rhs = GetZ3Expr(op->operands[1]);
      InsertZ3Expr(op, lhs & rhs);
    } break;

    default:
      LOG(FATAL) << "Unsupported LLVMOperation: " << op->Name();
      break;
  }
}

void IRToSMTVisitor::VisitExtract(Extract *op) {
  DLOG(INFO) << "VisitExtract: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto val = GetZ3Expr(op->operands[0]);
  auto hi = op->high_hit_exc - 1;
  auto lo = op->low_bit_inc;
  InsertZ3Expr(op, val.extract(hi, lo));
}

void IRToSMTVisitor::VisitPreservedCondition(PreservedCondition *op) {
  DLOG(INFO) << "VisitPreservedCondition: " << op->Name();
  op->Traverse(*this);
}

void IRToSMTVisitor::VisitDecodeCondition(DecodeCondition *op) {
  DLOG(INFO) << "VisitDecodeCondition: " << op->Name();
  op->Traverse(*this);
}

void IRToSMTVisitor::VisitOnlyOneCondition(OnlyOneCondition *op) {
  DLOG(INFO) << "VisitOnlyOneCondition: " << op->Name();
  op->Traverse(*this);
}

void IRToSMTVisitor::VisitVerifyInstruction(VerifyInstruction *op) {
  DLOG(INFO) << "VisitVerifyInstruction: " << op->Name();
  op->Traverse(*this);
}

void IRToSMTVisitor::VisitCircuit(Circuit *op) {
  DLOG(INFO) << "Circuit: " << op->Name();
  op->Traverse(*this);
}

void DoSMT(Circuit *circuit) {
  circuitous::IRToSMTVisitor smt;
  smt.Visit(circuit);
}

}  // namespace circuitous