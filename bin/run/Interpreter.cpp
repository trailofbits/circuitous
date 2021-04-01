#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <gflags/gflags.h>
#include <glog/logging.h>
#pragma clang diagnostic pop

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/InstrTypes.h>

#include "Interpreter.h"

namespace circuitous {

Interpreter::Interpreter(Circuit *c) : circuit(c) {}

void Interpreter::SetNodeVal(Operation *op, const llvm::APInt &val) {
  // CHECK(!node_values.count(op));
  node_values[op] = val;
}

llvm::APInt &Interpreter::GetNodeVal(Operation *op) {
  auto iter{node_values.find(op)};
  CHECK(iter != node_values.end());
  return iter->second;
}

void Interpreter::SetInstructionBitsValue(const std::string &bits) {
  auto inst = circuit->Attr<InputInstructionBits>()[0];
  SetNodeVal(inst, llvm::APInt(inst->size, bits, /*radix=*/16U));
}

void Interpreter::SetInputRegisterValue(const std::string &name,
                                        uint64_t bits) {
  for (auto reg : circuit->Attr<InputRegister>()) {
    if (reg->reg_name == name) {
      SetNodeVal(reg, llvm::APInt(reg->size, bits));
      return;
    }
  }
  LOG(WARNING) << "Input register " << name << " not present in circuit.";
}

uint64_t Interpreter::GetOutputRegisterValue(const std::string &name) {
  for (auto reg : circuit->Attr<OutputRegister>()) {
    if (reg->reg_name == name) {
      return GetNodeVal(reg).getLimitedValue();
    }
  }

  LOG(FATAL) << "Output register " << name << " not present in circuit.";

  return 0ULL;
}

void Interpreter::Visit(Operation *op) {
  op->Traverse(*this);
  if (node_values.count(op)) {
    // Remember previous node value
    auto prev_val{GetNodeVal(op)};
    // Compute new node value
    Visitor::Visit(op);
    // Was there a change?
    changed |= prev_val != GetNodeVal(op);
  } else {
    // We have no value. Just do it!
    Visitor::Visit(op);
    changed = true;
  }
}

void Interpreter::VisitOperation(Operation *op) {
  LOG(FATAL) << "Unhandled operation: " << op->Name();
}

void Interpreter::VisitConstant(Constant *op) {
  DLOG(INFO) << "VisitConstant: " << op->Name();
  std::string bits{op->bits.rbegin(), op->bits.rend()};
  SetNodeVal(op, llvm::APInt(op->size, bits, /*radix=*/2U));
}

void Interpreter::VisitInputRegister(InputRegister *op) {
  DLOG(INFO) << "VisitInputRegister: " << op->Name();
  CHECK(node_values.count(op))
      << "Input register " << op->reg_name << " bits not set.";
}

void Interpreter::VisitInputImmediate(InputImmediate *op) {
  LOG(INFO) << "VisitInputImmediate: " << op->Name();
  CHECK(op->operands.Size() == 1)
    << "Incorrect number of operands of InputImmediate:"
    << op->operands.Size() << "!= 1";
  SetNodeVal(op, GetNodeVal(op->operands[0]));
}


void Interpreter::VisitOutputRegister(OutputRegister *op) {
  DLOG(INFO) << "VisitOutputRegister: " << op->Name();
  // TODO(surovic): figure out a better way to represent an
  // undefined initial value;
  if (!node_values.count(op)) {
    SetNodeVal(op, llvm::APInt(op->size, 0ULL));
  }
}

void Interpreter::VisitInputInstructionBits(InputInstructionBits *op) {
  DLOG(INFO) << "VisitInputInstructionBits: " << op->Name();
  CHECK(node_values.count(op)) << "Input instruction bits not set.";
}

void Interpreter::VisitHint(Hint *op) {
  DLOG(INFO) << "VisitHint: " << op->Name();
  // TODO(surovic): See VisitOutputRegister()
  if (!node_values.count(op)) {
    SetNodeVal(op, llvm::APInt(op->size, 0ULL));
  }
}

void Interpreter::VisitUndefined(Undefined *op) {
  DLOG(INFO) << "VisitUndefined: " << op->Name();
  // TODO(surovic): See VisitOutputRegister()
  if (!node_values.count(op)) {
    SetNodeVal(op, llvm::APInt(op->size, 0ULL));
  }
}

void Interpreter::VisitExtract(Extract *op) {
  DLOG(INFO) << "VisitExtract: " << op->Name();
  auto val{GetNodeVal(op->operands[0])};
  auto pos{op->low_bit_inc};
  auto num{op->high_bit_exc - pos};
  node_values[op] = val.extractBits(num, pos);
}

// TODO(lukas): Some non-align cases most likely need some extra handling
//              which is currently not happening? Investigate.
void Interpreter::VisitConcat(Concat *op) {
    llvm::APInt build{ op->size, 0, false };
    auto current = 0u;
    for (auto i = 0u; i < op->operands.Size(); ++i) {
        build.insertBits(node_values[op->operands[i]], current);
        current += op->operands[i]->size;
    }
    node_values[op] = build;
}

void Interpreter::VisitNot(Not *op) {
  LOG(INFO) << "Visit Not" << op->Name();
  auto val = GetNodeVal(op->operands[0]);
  // NOTE(lukas): To avoid confusion the copy is here explicitly, since `negate` does
  //              change the APInt instead of returning a new one.
  llvm::APInt copy = val;
  copy.negate();
  SetNodeVal(op, copy);
}

void Interpreter::VisitLLVMOperation(LLVMOperation *op) {
  DLOG(INFO) << "VisitLLVMOperation: " << op->Name();
  auto lhs{[this, op] { return GetNodeVal(op->operands[0]); }};
  auto rhs{[this, op] { return GetNodeVal(op->operands[1]); }};
  switch (op->llvm_op_code) {
    case llvm::Instruction::OtherOps::Select: {
      auto selector = GetNodeVal(op->operands[0]);
      auto true_val = GetNodeVal(op->operands[1]);
      auto false_val = GetNodeVal(op->operands[2]);
      SetNodeVal(op, selector.getBoolValue() ? true_val : false_val );
    } break;

    case llvm::BinaryOperator::Add: {
      SetNodeVal(op, lhs() + rhs());
    } break;

    case llvm::BinaryOperator::Sub: {
      SetNodeVal(op, lhs() - rhs());
    } break;

    case llvm::BinaryOperator::Mul: {
      SetNodeVal(op, lhs() * rhs());
    } break;

    case llvm::BinaryOperator::UDiv: {
      SetNodeVal(op, lhs().udiv(rhs()));
    } break;

    case llvm::BinaryOperator::SDiv: {
      SetNodeVal(op, lhs().sdiv(rhs()));
    } break;

    case llvm::BinaryOperator::And: {
      SetNodeVal(op, lhs() & rhs());
    } break;

    case llvm::BinaryOperator::Or: {
      SetNodeVal(op, lhs() | rhs());
    } break;

    case llvm::BinaryOperator::Xor: {
      SetNodeVal(op, lhs() ^ rhs());
    } break;

    case llvm::BinaryOperator::Shl: {
      SetNodeVal(op, lhs() << rhs());
    } break;

    case llvm::BinaryOperator::LShr: {
      SetNodeVal(op, lhs().lshr(rhs()));
    } break;

    case llvm::BinaryOperator::AShr: {
      SetNodeVal(op, lhs().ashr(rhs()));
    } break;

    case llvm::BinaryOperator::Trunc: {
      SetNodeVal(op, lhs().trunc(op->size));
    } break;

    case llvm::BinaryOperator::ZExt: {
      SetNodeVal(op, lhs().zext(op->size));
    } break;

    case llvm::BinaryOperator::SExt: {
      SetNodeVal(op, lhs().sext(op->size));
    } break;

    case llvm::BinaryOperator::ICmp: {
      auto result{false};
      switch (op->llvm_predicate) {
        case llvm::CmpInst::ICMP_ULT: {
          result = lhs().ult(rhs());
        } break;

        case llvm::CmpInst::ICMP_SLT: {
          result = lhs().slt(rhs());
        } break;

        case llvm::CmpInst::ICMP_UGT: {
          result = lhs().ugt(rhs());
        } break;

        case llvm::CmpInst::ICMP_EQ: {
          result = lhs() == rhs();
        } break;

        case llvm::CmpInst::ICMP_NE: {
          result = lhs() != rhs();
        } break;

        default: LOG(FATAL) << "Unknown LLVM operation: " << op->Name(); break;
      }
      SetNodeVal(op, result ? TrueVal() : FalseVal());
    } break;

    default: LOG(FATAL) << "Unknown LLVM operation: " << op->Name(); break;
  }
}

void Interpreter::VisitParity(Parity *op) {
  DLOG(INFO) << "VisitParity: " << op->Name();
  auto val{GetNodeVal(op->operands[0])};
  SetNodeVal(op, llvm::APInt(1, val.countPopulation() % 2));
}

void Interpreter::VisitPopulationCount(PopulationCount *op) {
  DLOG(INFO) << "VisitPopulationCount: " << op->Name();
  auto val{GetNodeVal(op->operands[0])};
  SetNodeVal(op, llvm::APInt(op->size, val.countPopulation()));
}

void Interpreter::VisitDecodeCondition(DecodeCondition *op) {
  DLOG(INFO) << "VisitDecodeCondition: " << op->Name();
  auto inst{GetNodeVal(op->operands[0])};
  auto bits{GetNodeVal(op->operands[1])};
  SetNodeVal(op, inst == bits ? TrueVal() : FalseVal());
}

void Interpreter::VisitRegisterCondition(RegisterCondition *op) {
  DLOG(INFO) << "VisitRegisterCondition: " << op->Name();
  auto val{op->operands[0]};
  auto reg{op->operands[1]};
  SetNodeVal(reg, GetNodeVal(val));
  SetNodeVal(op, TrueVal());
  // NOTE(msurovic): This is where we get compute "results".
  // Although I'm not sure if this is the correct way to do it.
  // Consider that we have output register condition checks
  // for various instructions present in the circuit. A valid
  // for one may not be a valid value for the another.
  // if (node_values.count(reg)) {
  //   SetNodeVal(op, GetNodeVal(reg) == GetNodeVal(val) ? TrueVal() : FalseVal());
  // } else {
  //   SetNodeVal(reg, GetNodeVal(val));
  //   SetNodeVal(op, TrueVal());
  // }
}

void Interpreter::VisitPreservedCondition(PreservedCondition *op) {
  DLOG(INFO) << "VisitPreservedCondition: " << op->Name();
  auto ireg{op->operands[0]};
  auto oreg{op->operands[1]};
  SetNodeVal(oreg, GetNodeVal(ireg));
  SetNodeVal(op, TrueVal());
  // NOTE(msurovic): See VisitRegisterCondition()
  // if (node_values.count(oreg)) {
  //   SetNodeVal(op,
  //              GetNodeVal(oreg) == GetNodeVal(ireg) ? TrueVal() : FalseVal());
  // } else {
  //   SetNodeVal(oreg, GetNodeVal(ireg));
  //   SetNodeVal(op, TrueVal());
  // }
}

void Interpreter::VisitCopyCondition(CopyCondition *op) {
  DLOG(INFO) << "VisitCopyCondition: " << op->Name();
  auto ireg{op->operands[0]};
  auto oreg{op->operands[1]};
  SetNodeVal(oreg, GetNodeVal(ireg));
  SetNodeVal(op, TrueVal());
  // NOTE(msurovic): See VisitRegisterCondition()
  // if (node_values.count(oreg)) {
  //   SetNodeVal(op,
  //              GetNodeVal(oreg) == GetNodeVal(ireg) ? TrueVal() : FalseVal());
  // } else {
  //   SetNodeVal(oreg, GetNodeVal(ireg));
  //   SetNodeVal(op, TrueVal());
  // }
}

void Interpreter::VisitHintCondition(HintCondition *op) {
  DLOG(INFO) << "VisitHintCondition: " << op->Name();
  auto real{op->operands[0]};
  auto hint{op->operands[1]};
  SetNodeVal(hint, GetNodeVal(real));
  SetNodeVal(op, TrueVal());
}

void Interpreter::VisitVerifyInstruction(VerifyInstruction *op) {
  DLOG(INFO) << "VisitVerifyInstruction: " << op->Name();
  auto result{GetNodeVal(op->operands[0])};
  for (auto i = 1U; i < op->operands.Size(); ++i) {
    result = result & GetNodeVal(op->operands[i]);
  }
  SetNodeVal(op, result);
}

void Interpreter::VisitOnlyOneCondition(OnlyOneCondition *op) {
  auto result = 0u;
  for (std::size_t i = 0; i < op->operands.Size(); ++i) {
    result += (GetNodeVal(op->operands[i]) == TrueVal()) ? 1 : 0;
  }
  SetNodeVal(op, result == 1U ? TrueVal() : FalseVal());
}

void Interpreter::VisitCircuit(Circuit *op) {
  DLOG(INFO) << "VisitCircuit: " << op->Name();
  SetNodeVal(op, GetNodeVal(op->operands[0]));
}

bool Interpreter::Run() {
  // Save initial node values
  auto node_values_init{node_values};
  // Run verification nodes until one succeeds
  for (auto op : circuit->Attr<VerifyInstruction>()) {
    // Re-initialize node values
    node_values.swap(node_values_init);
    // Reset
    changed = true;
    // Evaluate verification node until fixpoint
    while (changed) {
      changed = false;
      Visit(op);
    }
    // Verification successful
    if (GetNodeVal(op).getBoolValue()) {
      return true;
    }
  }
  // All verifications failed
  return false;
}
}  // namespace circuitous