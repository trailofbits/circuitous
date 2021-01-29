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

void Interpreter::SetInstructionBitsValue(const uint64_t bits) {
  auto inst{circuit->inst_bits[0]};
  SetNodeVal(inst, llvm::APInt(inst->size, bits));
}

void Interpreter::SetInputRegisterValue(const std::string &name,
                                        uint64_t bits) {
  for (auto reg : circuit->input_regs) {
    if (reg->reg_name == name) {
      SetNodeVal(reg, llvm::APInt(reg->size, bits));
      return;
    }
  }

  LOG(FATAL) << "Input register " << name << " not present in circuit.";
}

uint64_t Interpreter::GetOutputRegisterValue(const std::string &name) {
  for (auto reg : circuit->output_regs) {
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
  node_values[op] = llvm::APInt(op->size, bits, /*radix=*/2U);
}

void Interpreter::VisitInputRegister(InputRegister *op) {
  DLOG(INFO) << "VisitInputRegister: " << op->Name();
  CHECK(node_values.count(op))
      << "Input register " << op->reg_name << " bits not set.";
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
  // TODO(surovic): figure out a better way to represent an
  // undefined initial value;
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

void Interpreter::VisitLLVMOperation(LLVMOperation *op) {
  DLOG(INFO) << "VisitLLVMOperation: " << op->Name();
  switch (op->llvm_op_code) {
    case llvm::BinaryOperator::Add: {
      auto lhs{GetNodeVal(op->operands[0])};
      auto rhs{GetNodeVal(op->operands[1])};
      SetNodeVal(op, lhs + rhs);
    } break;

    case llvm::BinaryOperator::And: {
      auto lhs{GetNodeVal(op->operands[0])};
      auto rhs{GetNodeVal(op->operands[1])};
      SetNodeVal(op, lhs & rhs);
    } break;

    case llvm::BinaryOperator::Or: {
      auto lhs{GetNodeVal(op->operands[0])};
      auto rhs{GetNodeVal(op->operands[1])};
      SetNodeVal(op, lhs | rhs);
    } break;

    case llvm::BinaryOperator::Xor: {
      auto lhs{GetNodeVal(op->operands[0])};
      auto rhs{GetNodeVal(op->operands[1])};
      SetNodeVal(op, lhs ^ rhs);
    } break;

    case llvm::BinaryOperator::Shl: {
      auto lhs{GetNodeVal(op->operands[0])};
      auto rhs{GetNodeVal(op->operands[1])};
      SetNodeVal(op, lhs << rhs);
    } break;

    case llvm::BinaryOperator::LShr: {
      auto lhs{GetNodeVal(op->operands[0])};
      auto rhs{GetNodeVal(op->operands[1])};
      SetNodeVal(op, lhs.lshr(rhs));
    } break;

    case llvm::BinaryOperator::Trunc: {
      auto operand{GetNodeVal(op->operands[0])};
      SetNodeVal(op, operand.trunc(op->size));
    } break;

    case llvm::BinaryOperator::ZExt: {
      auto operand{GetNodeVal(op->operands[0])};
      SetNodeVal(op, operand.zext(op->size));
    } break;

    case llvm::BinaryOperator::ICmp: {
      auto lhs{GetNodeVal(op->operands[0])};
      auto rhs{GetNodeVal(op->operands[1])};
      auto result{false};
      switch (op->llvm_predicate) {
        case llvm::CmpInst::ICMP_ULT: {
          result = lhs.ult(rhs);
        } break;

        case llvm::CmpInst::ICMP_SLT: {
          result = lhs.slt(rhs);
        } break;

        case llvm::CmpInst::ICMP_UGT: {
          result = lhs.ugt(rhs);
        } break;

        case llvm::CmpInst::ICMP_EQ: {
          result = lhs == rhs;
        } break;

        case llvm::CmpInst::ICMP_NE: {
          result = lhs != rhs;
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
  DLOG(INFO) << "VisitOnlyOneCondition: " << op->Name();
  auto val{GetNodeVal(op)};
  SetNodeVal(op, val.countPopulation() == 1U ? TrueVal() : FalseVal());
}

void Interpreter::VisitCircuit(Circuit *op) {
  DLOG(INFO) << "VisitCircuit: " << op->Name();
  SetNodeVal(op, GetNodeVal(op->operands[0]));
}

bool Interpreter::Run() {
  // Run verification nodes until one succeeds
  for (auto op : circuit->verifications) {
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