/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#include <glog/logging.h>
#include <llvm/IR/InstrTypes.h>
#include <z3++.h>
#pragma clang diagnostic pop

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
  void Visit(Operation *op);
  void VisitOperation(Operation *op);
  void VisitInputInstructionBits(InputInstructionBits *op);
  void VisitInputRegister(InputRegister *op);
  void VisitOutputRegister(OutputRegister *op);
  void VisitConstant(Constant *op);
  void VisitHint(Hint *op);
  void VisitUndefined(Undefined *op);
  void VisitLLVMOperation(LLVMOperation *op);
  void VisitNot(Not *op);
  void VisitExtract(Extract *op);
  void VisitConcat(Concat *op);
  void VisitParity(Parity *op);
  void VisitPopulationCount(PopulationCount *op);
  void VisitCountLeadingZeroes(CountLeadingZeroes *op);
  void VisitCountTrailingZeroes(CountTrailingZeroes *op);
  void VisitRegisterCondition(RegisterCondition *op);
  void VisitPreservedCondition(PreservedCondition *op);
  void VisitCopyCondition(CopyCondition *op);
  void VisitDecodeCondition(DecodeCondition *op);
  void VisitHintCondition(HintCondition *op);
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

void IRToSMTVisitor::Visit(Operation *op) {
  if (z3_expr_map.count(op)) {
    return;
  }
  op->Traverse(*this);
}

void IRToSMTVisitor::VisitOperation(Operation *op) {
  LOG(FATAL) << "Unhandled operation: " << op->Name();
}

void IRToSMTVisitor::VisitInputInstructionBits(InputInstructionBits *op) {
  DLOG(INFO) << "VisitInputInstructionBits: " << op->Name();
  InsertZ3Expr(op, z3_ctx.bv_const("InputInst", op->size));
}

void IRToSMTVisitor::VisitInputRegister(InputRegister *op) {
  DLOG(INFO) << "VisitInputRegister: " << op->Name();
  auto name = "Input" + op->reg_name;
  InsertZ3Expr(op, z3_ctx.bv_const(name.c_str(), op->size));
}

void IRToSMTVisitor::VisitOutputRegister(OutputRegister *op) {
  DLOG(INFO) << "VisitOutputRegister: " << op->Name();
  auto name = "Output" + op->reg_name;
  InsertZ3Expr(op, z3_ctx.bv_const(name.c_str(), op->size));
}

void IRToSMTVisitor::VisitConstant(Constant *op) {
  DLOG(INFO) << "VisitConstant: " << op->Name();
  std::unique_ptr<bool[]> bits(new bool[op->size]);
  for (auto i = 0U; i < op->size; ++i) {
    bits[i] = op->bits[i] == '0' ? false : true;
  }
  InsertZ3Expr(op, z3_ctx.bv_val(op->size, bits.get()));
}

void IRToSMTVisitor::VisitHint(Hint *op) {
  DLOG(INFO) << "VisitHint: " << op->Name();
  auto name = "Hint" + std::to_string(reinterpret_cast<uint64_t>(op));
  InsertZ3Expr(op, z3_ctx.bv_const(name.c_str(), op->size));
}

void IRToSMTVisitor::VisitUndefined(Undefined *op) {
  DLOG(INFO) << "VisitUndefined: " << op->Name();
  InsertZ3Expr(op, z3_ctx.bv_const("Undef", op->size));
}

void IRToSMTVisitor::VisitLLVMOperation(LLVMOperation *op) {
  DLOG(INFO) << "VisitLLVMOperation: " << op->Name();
  auto lhs{[this, op] { return GetZ3Expr(op->operands[0]); }};
  auto rhs{[this, op] { return GetZ3Expr(op->operands[1]); }};
  switch (op->llvm_op_code) {
    case llvm::BinaryOperator::Add: {
      InsertZ3Expr(op, lhs() + rhs());
    } break;

    case llvm::BinaryOperator::Sub: {
      InsertZ3Expr(op, lhs() - rhs());
    } break;

    case llvm::BinaryOperator::Mul: {
      InsertZ3Expr(op, lhs() * rhs());
    } break;

    case llvm::BinaryOperator::And: {
      InsertZ3Expr(op, lhs() & rhs());
    } break;

    case llvm::BinaryOperator::Or: {
      InsertZ3Expr(op, lhs() | rhs());
    } break;

    case llvm::BinaryOperator::Xor: {
      InsertZ3Expr(op, lhs() ^ rhs());
    } break;

    case llvm::BinaryOperator::Trunc: {
      InsertZ3Expr(op, lhs().extract(op->size - 1, 0));
    } break;

    case llvm::BinaryOperator::Shl: {
      InsertZ3Expr(op, z3::shl(lhs(), rhs()));
    } break;

    case llvm::BinaryOperator::LShr: {
      InsertZ3Expr(op, z3::lshr(lhs(), rhs()));
    } break;

    case llvm::BinaryOperator::ZExt: {
      auto diff = op->size - op->operands[0]->size;
      InsertZ3Expr(op, z3::zext(lhs(), diff));
    } break;

    case llvm::BinaryOperator::ICmp: {
      z3::expr result(z3_ctx);
      switch (op->llvm_predicate) {
        case llvm::CmpInst::ICMP_ULT: {
          result = z3::ult(lhs(), rhs());
        } break;

        case llvm::CmpInst::ICMP_SLT: {
          result = z3::slt(lhs(), rhs());
        } break;

        case llvm::CmpInst::ICMP_UGT: {
          result = z3::ugt(lhs(), rhs());
        } break;

        case llvm::CmpInst::ICMP_EQ: {
          result = lhs() == rhs();
        } break;

        case llvm::CmpInst::ICMP_NE: {
          result = lhs() != rhs();
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

void IRToSMTVisitor::VisitNot(Not *op) {
  LOG(FATAL) << "VisitNot: " << op->Name();
}

void IRToSMTVisitor::VisitExtract(Extract *op) {
  DLOG(INFO) << "VisitExtract: " << op->Name();
  auto val = GetZ3Expr(op->operands[0]);
  auto hi = op->high_bit_exc - 1;
  auto lo = op->low_bit_inc;
  InsertZ3Expr(op, val.extract(hi, lo));
}

void IRToSMTVisitor::VisitConcat(Concat *op) {
  LOG(FATAL) << "VisitConcat: " << op->Name();
  // auto val = GetZ3Expr(op->operands[0]);
  // auto hi = op->high_bit_exc - 1;
  // auto lo = op->low_bit_inc;
  // InsertZ3Expr(op, val.extract(hi, lo));
}

void IRToSMTVisitor::VisitParity(Parity *op) {
  DLOG(INFO) << "VisitParity: " << op->Name();
  auto operand = GetZ3Expr(op->operands[0]);
  auto operand_size = operand.get_sort().bv_size();
  auto sum = operand.extract(0, 0);
  for (auto i = 1U; i < operand_size; ++i) {
    sum = sum ^ operand.extract(i, i);
  }
  InsertZ3Expr(op, sum);
}

void IRToSMTVisitor::VisitPopulationCount(PopulationCount *op) {
  LOG(FATAL) << "VisitPopulationCount: " << op->Name();
}

void IRToSMTVisitor::VisitCountLeadingZeroes(CountLeadingZeroes *op) {
  LOG(FATAL) << "VisitCountLeadingZeroes: " << op->Name();
}

void IRToSMTVisitor::VisitCountTrailingZeroes(CountTrailingZeroes *op) {
  LOG(FATAL) << "VisitCountTrailingZeroes: " << op->Name();
}

void IRToSMTVisitor::VisitRegisterCondition(RegisterCondition *op) {
  DLOG(INFO) << "VisitRegisterCondition: " << op->Name();
  auto val = GetZ3Expr(op->operands[0]);
  auto reg = GetZ3Expr(op->operands[1]);
  InsertZ3Expr(op, Z3BVCast(val == reg));
}

void IRToSMTVisitor::VisitPreservedCondition(PreservedCondition *op) {
  DLOG(INFO) << "VisitPreservedCondition: " << op->Name();
  auto ireg = GetZ3Expr(op->operands[0]);
  auto oreg = GetZ3Expr(op->operands[1]);
  InsertZ3Expr(op, Z3BVCast(ireg == oreg));
}

void IRToSMTVisitor::VisitCopyCondition(CopyCondition *op) {
  DLOG(INFO) << "VisitCopyCondition: " << op->Name();
  auto ireg = GetZ3Expr(op->operands[0]);
  auto oreg = GetZ3Expr(op->operands[1]);
  InsertZ3Expr(op, Z3BVCast(ireg == oreg));
}

void IRToSMTVisitor::VisitDecodeCondition(DecodeCondition *op) {
  DLOG(INFO) << "VisitDecodeCondition: " << op->Name();
  auto inst = GetZ3Expr(op->operands[0]);
  auto bits = GetZ3Expr(op->operands[1]);
  InsertZ3Expr(op, Z3BVCast(inst == bits));
}

void IRToSMTVisitor::VisitOnlyOneCondition(OnlyOneCondition *op) {
  DLOG(INFO) << "VisitOnlyOneCondition: " << op->Name();
  auto result = GetZ3Expr(op->operands[0]);
  for (auto i = 1U; i < op->operands.Size(); ++i) {
    result = result ^ GetZ3Expr(op->operands[i]);
  }
  InsertZ3Expr(op, result);
}

void IRToSMTVisitor::VisitHintCondition(HintCondition *op) {
  DLOG(INFO) << "VisitHintCondition: " << op->Name();
  auto real = GetZ3Expr(op->operands[0]);
  auto hint = GetZ3Expr(op->operands[1]);
  InsertZ3Expr(op, Z3BVCast(real == hint));
}

void IRToSMTVisitor::VisitVerifyInstruction(VerifyInstruction *op) {
  DLOG(INFO) << "VisitVerifyInstruction: " << op->Name();
  auto result = GetZ3Expr(op->operands[0]);
  for (auto i = 1U; i < op->operands.Size(); ++i) {
    result = result & GetZ3Expr(op->operands[i]);
  }
  InsertZ3Expr(op, result);
}

void IRToSMTVisitor::VisitCircuit(Circuit *op) {
  DLOG(INFO) << "VisitCircuit: " << op->Name();
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

void PrintSMT(std::ostream &os, Circuit *circuit, bool bit_blast) {
  z3::context ctx;
  circuitous::IRToSMTVisitor smt(ctx);
  z3::solver solver(ctx);

  // Convert `circuit` to a z3::expr
  auto expr = smt.GetOrCreateZ3Expr(circuit);

  // Dump to SMT-LIBv2 as is
  if (!bit_blast) {
    solver.add(expr);
    os << solver.to_smt2();
    return;
  }
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

  // Dump to SMT-LIBv2 as a SAT problem
  solver.add(expr);
  os << solver.to_smt2();
}

}  // namespace circuitous
