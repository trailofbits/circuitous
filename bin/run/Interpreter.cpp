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
  // if (node_values.count(op)) {
  //   return;
  // }
  op->Traverse(*this);
  Visitor::Visit(op);
}

void Interpreter::VisitOperation(Operation *op) {
  LOG(FATAL) << "Unhandled operation: " << op->Name();
}

void Interpreter::VisitConstant(Constant *op) {
  DLOG(INFO) << "VisitConstant: " << op->Name();
  std::string bits{op->bits.rbegin(), op->bits.rend()};
  node_values[op] = llvm::APInt(op->size, bits, 2U);
}

void Interpreter::VisitInputRegister(InputRegister *op) {
  DLOG(INFO) << "VisitInputRegister: " << op->Name();
  CHECK(node_values.count(op))
      << "Input register " << op->reg_name << " bits not set.";
}

void Interpreter::VisitOutputRegister(OutputRegister *op) {
  DLOG(INFO) << "VisitOutputRegister: " << op->Name();
  // CHECK(!node_values.count(op));
}

void Interpreter::VisitInputInstructionBits(InputInstructionBits *op) {
  DLOG(INFO) << "VisitInputInstructionBits: " << op->Name();
  CHECK(node_values.count(op)) << "Input instruction bits not set.";
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
    case llvm::BinaryOperator::And:
      auto lhs{GetNodeVal(op->operands[0])};
      auto rhs{GetNodeVal(op->operands[1])};
      SetNodeVal(op, lhs & rhs);
      break;
  }
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
  auto sum{0U};
  for (auto operand : op->operands) {
    sum += GetNodeVal(operand).getLimitedValue();
  }
  SetNodeVal(op, sum == 1U ? TrueVal() : FalseVal());
  // auto result{GetNodeVal(op->operands[0])};
  // for (auto i = 1U; i < op->operands.Size(); ++i) {
  //   result = result ^ GetNodeVal(op->operands[i]);
  // }
  // SetNodeVal(op, result);
}

void Interpreter::VisitCircuit(Circuit *op) {
  DLOG(INFO) << "VisitCircuit: " << op->Name();
  SetNodeVal(op, GetNodeVal(op->operands[0]));
}

void Interpreter::Run() {
  for (auto op : circuit->verifications) {
    Visit(op);
    if (GetNodeVal(op).getBoolValue()) {
      return;
    }
  }
}
}  // namespace circuitous