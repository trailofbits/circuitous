/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <glog/logging.h>

#include <llvm/IR/InstrTypes.h>

#include <memory>

#include "IRToSMTVisitor.h"

namespace circuitous {

IRToSMTVisitor::IRToSMTVisitor(z3::context &ctx)
    : z3_ctx(ctx), z3_expr_vec(z3_ctx) {}

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

void IRToSMTVisitor::VisitRegisterCondition(RegisterCondition *op) {
  DLOG(INFO) << "VisitRegisterCondition: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto val = GetZ3Expr(op->operands[0]);
  auto reg = GetZ3Expr(op->operands[1]);
  InsertZ3Expr(op, val == reg);
}

void IRToSMTVisitor::VisitPreservedCondition(PreservedCondition *op) {
  DLOG(INFO) << "VisitPreservedCondition: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto ireg = GetZ3Expr(op->operands[0]);
  auto oreg = GetZ3Expr(op->operands[1]);
  InsertZ3Expr(op, ireg == oreg);
}

void IRToSMTVisitor::VisitDecodeCondition(DecodeCondition *op) {
  DLOG(INFO) << "VisitDecodeCondition: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto inst = GetZ3Expr(op->operands[0]);
  auto bits = GetZ3Expr(op->operands[1]);
  InsertZ3Expr(op, inst == bits);
}

void IRToSMTVisitor::VisitOnlyOneCondition(OnlyOneCondition *op) {
  DLOG(INFO) << "VisitOnlyOneCondition: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto result = GetZ3Expr(op->operands[0]);
  for (auto i = 1U; i < op->operands.Size(); ++i) {
    result = z3::to_expr(z3_ctx,
                         Z3_mk_xor(z3_ctx, result, GetZ3Expr(op->operands[i])));
  }
  InsertZ3Expr(op, result);
}

void IRToSMTVisitor::VisitVerifyInstruction(VerifyInstruction *op) {
  DLOG(INFO) << "VisitVerifyInstruction: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto result = GetZ3Expr(op->operands[0]);
  for (auto i = 1U; i < op->operands.Size(); ++i) {
    result = result && GetZ3Expr(op->operands[i]);
  }
  InsertZ3Expr(op, result);
}

void IRToSMTVisitor::VisitCircuit(Circuit *op) {
  DLOG(INFO) << "VisitCircuit: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  InsertZ3Expr(op, GetZ3Expr(op->operands[0]));
}

z3::expr IRToSMTVisitor::GetOrCreateZ3Expr(Operation *op) {
  if (!z3_expr_map.count(op)) {
    Visit(op);
  }
  return GetZ3Expr(op);
}

namespace {

z3::expr OrToAnd(z3::context &ctx, z3::expr e) {
  if (!e.is_app()) {
    return e;
  }

  bool is_or = e.decl().decl_kind() == Z3_OP_OR;

  z3::expr_vector args(ctx);
  for (auto i = 0U; i < e.num_args(); ++i) {
    auto arg = OrToAnd(ctx, e.arg(i));
    args.push_back(is_or ? !(arg) : arg);
  }

  z3::expr r(ctx);
  if (is_or) {
    r = args[0];
    for (auto i = 1U; i < e.num_args(); ++i) {
      r = r && args[(signed)i];
    }
    r = !(r);
  } else {
    r = e.decl()(args);
  }
  return r;
}

z3::expr EqToXnor(z3::context &ctx, z3::expr e) {
  if (!e.is_app()) {
    return e;
  }

  bool is_eq = e.decl().decl_kind() == Z3_OP_EQ;

  z3::expr_vector args(ctx);
  for (auto i = 0U; i < e.num_args(); ++i) {
    args.push_back(EqToXnor(ctx, e.arg(i)));
  }

  return is_eq ? !z3::to_expr(ctx, Z3_mk_xor(ctx, args[0], args[1]))
               : e.decl()(args);
}

z3::expr ElimNot(z3::context &ctx, z3::expr e) {
  if (!e.is_app()) {
    return e;
  }

  z3::expr_vector args(ctx);
  for (auto i = 0U; i < e.num_args(); ++i) {
    args.push_back(ElimNot(ctx, e.arg(i)));
  }

  auto r = e.decl()(args);
  if (e.decl().decl_kind() == Z3_OP_NOT) {
    auto arg = args[0];
    if (arg.is_app() && arg.decl().decl_kind() == Z3_OP_NOT) {
      r = arg.arg(0);
    }
  }

  return r;
}

}  // namespace

void PrintSMT(std::ostream &os, Circuit *circuit) {
  z3::context ctx;
  circuitous::IRToSMTVisitor smt(ctx);
  auto expr = smt.GetOrCreateZ3Expr(circuit);
  z3::tactic to_sat(z3::tactic(ctx, "ctx-solver-simplify") &
                    z3::tactic(ctx, "bit-blast"));

  z3::goal goal(ctx);
  goal.add(expr);
  auto app = to_sat(goal);
  CHECK(app.size() == 1) << "Unexpected multiple goals in application!";
  expr = app[0].as_expr();

  expr = OrToAnd(ctx, expr);
  expr = EqToXnor(ctx, expr);
  expr = ElimNot(ctx, expr);

  z3::solver solver(ctx);
  solver.add(expr);
  os << solver.to_smt2();
}

}  // namespace circuitous