/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <glog/logging.h>
#include <llvm/IR/InstrTypes.h>
#include <z3++.h>

#include <memory>
#include <unordered_map>

#include "circuitous/IR/IR.h"

namespace circuitous {
namespace {

class IRToSMTVisitor : public UniqueVisitor<IRToSMTVisitor> {
 private:
  z3::context &z3_ctx;

  // Expression map
  z3::expr_vector z3_expr_vec;
  std::unordered_map<Operation *, unsigned> z3_expr_map;
  void InsertZ3Expr(Operation *op, z3::expr z3_expr);
  z3::expr GetZ3Expr(Operation *op);
  z3::expr Z3BVCast(z3::expr expr);

 public:
  IRToSMTVisitor(z3::context &ctx);
  void VisitInputInstructionBits(InputInstructionBits *op);
  void VisitInputRegister(InputRegister *op);
  void VisitOutputRegister(OutputRegister *op);
  void VisitConstant(Constant *op);
  void VisitUndefined(Undefined *op);
  void VisitLLVMOperation(LLVMOperation *op);
  void VisitExtract(Extract *op);
  void VisitConcat(Concat *op);
  void VisitParity(Parity *op);
  void VisitRegisterCondition(RegisterCondition *op);
  void VisitPreservedCondition(PreservedCondition *op);
  void VisitCopyCondition(CopyCondition *op);
  void VisitDecodeCondition(DecodeCondition *op);
  void VisitOnlyOneCondition(OnlyOneCondition *op);
  void VisitVerifyInstruction(VerifyInstruction *op);
  void VisitCircuit(Circuit *op);

  z3::expr GetOrCreateZ3Expr(Operation *op);
};


IRToSMTVisitor::IRToSMTVisitor(z3::context &ctx)
    : z3_ctx(ctx),
      z3_expr_vec(z3_ctx) {}

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

z3::expr IRToSMTVisitor::Z3BVCast(z3::expr expr) {
  z3::expr result(z3_ctx);
  if (expr.is_bv()) {
    result = expr;
  } else if (expr.is_bool()) {
    auto cast = z3::ite(expr, z3_ctx.bv_val(1, 1), z3_ctx.bv_val(0, 1));
    result = cast.simplify();
  } else {
    LOG(FATAL) << "Unsupported Z3 sort";
  }
  return result;
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
    bits[i] = op->bits[i] == '0' ? false : true;
  }
  InsertZ3Expr(op, z3_ctx.bv_val(op->size, bits.get()));
}

void IRToSMTVisitor::VisitUndefined(Undefined *op) {
  DLOG(INFO) << "VisitUndefined: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  InsertZ3Expr(op, z3_ctx.bv_const("Undef", op->size));
}

void IRToSMTVisitor::VisitLLVMOperation(LLVMOperation *op) {
  DLOG(INFO) << "VisitLLVMOperation: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  switch (op->llvm_op_code) {
    case llvm::BinaryOperator::Add: {
      auto lhs = GetZ3Expr(op->operands[0]);
      auto rhs = GetZ3Expr(op->operands[1]);
      InsertZ3Expr(op, lhs + rhs);
    } break;

    case llvm::BinaryOperator::Sub: {
      auto lhs = GetZ3Expr(op->operands[0]);
      auto rhs = GetZ3Expr(op->operands[1]);
      InsertZ3Expr(op, lhs - rhs);
    } break;

    case llvm::BinaryOperator::Mul: {
      auto lhs = GetZ3Expr(op->operands[0]);
      auto rhs = GetZ3Expr(op->operands[1]);
      InsertZ3Expr(op, lhs * rhs);
    } break;

    case llvm::BinaryOperator::And: {
      auto lhs = GetZ3Expr(op->operands[0]);
      auto rhs = GetZ3Expr(op->operands[1]);
      InsertZ3Expr(op, lhs & rhs);
    } break;

    case llvm::BinaryOperator::Or: {
      auto lhs = GetZ3Expr(op->operands[0]);
      auto rhs = GetZ3Expr(op->operands[1]);
      InsertZ3Expr(op, lhs | rhs);
    } break;

    case llvm::BinaryOperator::Xor: {
      auto lhs = GetZ3Expr(op->operands[0]);
      auto rhs = GetZ3Expr(op->operands[1]);
      InsertZ3Expr(op, lhs ^ rhs);
    } break;

    case llvm::BinaryOperator::Trunc: {
      auto expr = GetZ3Expr(op->operands[0]);
      InsertZ3Expr(op, expr.extract(op->size, 1));
    } break;

    case llvm::BinaryOperator::Shl: {
      auto lhs = GetZ3Expr(op->operands[0]);
      auto rhs = GetZ3Expr(op->operands[1]);
      InsertZ3Expr(op, z3::shl(lhs, rhs));
    } break;

    case llvm::BinaryOperator::LShr: {
      auto lhs = GetZ3Expr(op->operands[0]);
      auto rhs = GetZ3Expr(op->operands[1]);
      InsertZ3Expr(op, z3::lshr(lhs, rhs));
    } break;

    case llvm::BinaryOperator::ZExt: {
      auto operand = GetZ3Expr(op->operands[0]);
      auto diff = op->size - op->operands[0]->size;
      InsertZ3Expr(op, z3::zext(operand, diff));
    } break;

    case llvm::BinaryOperator::ICmp: {
      auto lhs = GetZ3Expr(op->operands[0]);
      auto rhs = GetZ3Expr(op->operands[1]);
      z3::expr result(z3_ctx);
      switch (op->llvm_predicate) {
        case llvm::CmpInst::ICMP_ULT: {
          result = z3::ult(lhs, rhs);
        } break;

        case llvm::CmpInst::ICMP_SLT: {
          result = z3::slt(lhs, rhs);
        } break;

        case llvm::CmpInst::ICMP_UGT: {
          result = z3::ugt(lhs, rhs);
        } break;

        case llvm::CmpInst::ICMP_EQ: {
          result = lhs == rhs;
        } break;

        case llvm::CmpInst::ICMP_NE: {
          result = lhs != rhs;
        } break;

        default:
          LOG(FATAL) << "Unsupported LLVMOperation: " << op->Name();
          break;
      }
      InsertZ3Expr(op, Z3BVCast(result));
    } break;

    default: LOG(FATAL) << "Unsupported LLVMOperation: " << op->Name(); break;
  }
}

void IRToSMTVisitor::VisitParity(Parity *op) {
  DLOG(INFO) << "VisitParity: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto operand = GetZ3Expr(op->operands[0]);
  auto operand_size = operand.get_sort().bv_size();
  auto sum = operand.extract(0, 0);
  for (auto i = 1U; i < operand_size; ++i) {
    sum = sum ^ operand.extract(i, i);
  }
  InsertZ3Expr(op, sum);
}

void IRToSMTVisitor::VisitExtract(Extract *op) {
  DLOG(INFO) << "VisitExtract: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto val = GetZ3Expr(op->operands[0]);
  auto hi = op->high_bit_exc - 1;
  auto lo = op->low_bit_inc;
  InsertZ3Expr(op, val.extract(hi, lo));
}

void IRToSMTVisitor::VisitConcat(Concat *op) {
  LOG(FATAL) << "VisitConcat: " << op->Name();

  // if (z3_expr_map.count(op)) {
  //   return;
  // }
  // op->Traverse(*this);
  // auto val = GetZ3Expr(op->operands[0]);
  // auto hi = op->high_bit_exc - 1;
  // auto lo = op->low_bit_inc;
  // InsertZ3Expr(op, val.extract(hi, lo));
}

void IRToSMTVisitor::VisitRegisterCondition(RegisterCondition *op) {
  DLOG(INFO) << "VisitRegisterCondition: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto val = GetZ3Expr(op->operands[0]);
  auto reg = GetZ3Expr(op->operands[1]);
  InsertZ3Expr(op, Z3BVCast(val == reg));
}

void IRToSMTVisitor::VisitPreservedCondition(PreservedCondition *op) {
  DLOG(INFO) << "VisitPreservedCondition: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto ireg = GetZ3Expr(op->operands[0]);
  auto oreg = GetZ3Expr(op->operands[1]);
  InsertZ3Expr(op, Z3BVCast(ireg == oreg));
}

void IRToSMTVisitor::VisitCopyCondition(CopyCondition *op) {
  DLOG(INFO) << "VisitCopyCondition: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto ireg = GetZ3Expr(op->operands[0]);
  auto oreg = GetZ3Expr(op->operands[1]);
  InsertZ3Expr(op, Z3BVCast(ireg == oreg));
}

void IRToSMTVisitor::VisitDecodeCondition(DecodeCondition *op) {
  DLOG(INFO) << "VisitDecodeCondition: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto inst = GetZ3Expr(op->operands[0]);
  auto bits = GetZ3Expr(op->operands[1]);
  InsertZ3Expr(op, Z3BVCast(inst == bits));
}

void IRToSMTVisitor::VisitOnlyOneCondition(OnlyOneCondition *op) {
  DLOG(INFO) << "VisitOnlyOneCondition: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto result = GetZ3Expr(op->operands[0]);
  for (auto i = 1U; i < op->operands.Size(); ++i) {
    result = result ^ GetZ3Expr(op->operands[i]);
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
    result = result & GetZ3Expr(op->operands[i]);
  }
  InsertZ3Expr(op, result);
}

void IRToSMTVisitor::VisitCircuit(Circuit *op) {
  DLOG(INFO) << "VisitCircuit: " << op->Name();
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
  auto expr = GetZ3Expr(op->operands[0]);
  InsertZ3Expr(op, expr != z3_ctx.num_val(0, expr.get_sort()));
}

z3::expr IRToSMTVisitor::GetOrCreateZ3Expr(Operation *op) {
  if (!z3_expr_map.count(op)) {
    Visit(op);
  }
  return GetZ3Expr(op);
}

z3::expr OrToAnd(z3::context &ctx, const z3::expr &e,
                 std::unordered_map<unsigned, unsigned> &z3_expr_map,
                 z3::expr_vector &z3_expr_vec) {
  auto iter = z3_expr_map.find(e.id());
  if (iter != z3_expr_map.end()) {
    return z3_expr_vec[(signed) iter->second];
  }

  if (!e.is_app()) {
    return e;
  }

  bool is_or = e.decl().decl_kind() == Z3_OP_OR;

  z3::expr_vector args(ctx);
  for (auto i = 0U; i < e.num_args(); ++i) {
    auto arg = OrToAnd(ctx, e.arg(i), z3_expr_map, z3_expr_vec);
    args.push_back(is_or ? !(arg) : arg);
  }

  z3::expr r(ctx);
  if (is_or) {
    r = args[0];
    for (auto i = 1U; i < e.num_args(); ++i) {
      r = r && args[(signed) i];
    }
    r = !(r);
  } else {
    r = e.decl()(args);
  }

  z3_expr_map[e.id()] = z3_expr_vec.size();
  z3_expr_vec.push_back(r);

  return r;
}

z3::expr EqToXnor(z3::context &ctx, const z3::expr &e,
                  std::unordered_map<unsigned, unsigned> &z3_expr_map,
                  z3::expr_vector &z3_expr_vec) {
  auto iter = z3_expr_map.find(e.id());
  if (iter != z3_expr_map.end()) {
    return z3_expr_vec[(signed) iter->second];
  }

  if (!e.is_app()) {
    return e;
  }

  bool is_eq = e.decl().decl_kind() == Z3_OP_EQ;

  z3::expr_vector args(ctx);
  for (auto i = 0U; i < e.num_args(); ++i) {
    args.push_back(EqToXnor(ctx, e.arg(i), z3_expr_map, z3_expr_vec));
  }

  auto r = is_eq ? !z3::to_expr(ctx, Z3_mk_xor(ctx, args[0], args[1]))
                 : e.decl()(args);

  z3_expr_map[e.id()] = z3_expr_vec.size();
  z3_expr_vec.push_back(r);

  return r;
}

z3::expr ElimNot(z3::context &ctx, const z3::expr &e,
                 std::unordered_map<unsigned, unsigned> &z3_expr_map,
                 z3::expr_vector &z3_expr_vec) {
  auto iter = z3_expr_map.find(e.id());
  if (iter != z3_expr_map.end()) {
    return z3_expr_vec[(signed) iter->second];
  }

  if (!e.is_app()) {
    return e;
  }

  z3::expr_vector args(ctx);
  for (auto i = 0U; i < e.num_args(); ++i) {
    args.push_back(ElimNot(ctx, e.arg(i), z3_expr_map, z3_expr_vec));
  }

  auto r = e.decl()(args);
  if (e.decl().decl_kind() == Z3_OP_NOT) {
    auto arg = args[0];
    if (arg.is_app() && arg.decl().decl_kind() == Z3_OP_NOT) {
      r = arg.arg(0);
    }
  }

  z3_expr_map[e.id()] = z3_expr_vec.size();
  z3_expr_vec.push_back(r);

  return r;
}

z3::expr EqToXnor(z3::context &ctx, const z3::expr &e) {
  std::unordered_map<unsigned, unsigned> m;
  z3::expr_vector v(ctx);
  return EqToXnor(ctx, e, m, v);
}

z3::expr OrToAnd(z3::context &ctx, const z3::expr &e) {
  std::unordered_map<unsigned, unsigned> m;
  z3::expr_vector v(ctx);
  return OrToAnd(ctx, e, m, v);
}

z3::expr ElimNot(z3::context &ctx, const z3::expr &e) {
  std::unordered_map<unsigned, unsigned> m;
  z3::expr_vector v(ctx);
  return ElimNot(ctx, e, m, v);
}

}  // namespace

void PrintSMT(std::ostream &os, Circuit *circuit) {
  z3::context ctx;
  circuitous::IRToSMTVisitor smt(ctx);
  auto expr = smt.GetOrCreateZ3Expr(circuit);

  // Declare tactical
  z3::tactic to_sat(z3::tactic(ctx, "ctx-simplify") &
                    z3::tactic(ctx, "propagate-bv-bounds") &
                    z3::tactic(ctx, "solve-eqs") & z3::tactic(ctx, "simplify") &
                    z3::tactic(ctx, "bit-blast"));

  // Apply tactics
  z3::goal goal(ctx);
  goal.add(expr);
  auto app = to_sat(goal);
  CHECK(app.size() == 1) << "Unexpected multiple goals in application!";
  expr = app[0].as_expr();
  // Custom optimizations
  expr = EqToXnor(ctx, expr);
  expr = ElimNot(ctx, expr);
  expr = OrToAnd(ctx, expr);
  // Dump to SMT-LIBv2
  z3::solver solver(ctx);
  solver.add(expr);
  os << solver.to_smt2();
}

}  // namespace circuitous
