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
  auto iter{node_values.find(op)};
  CHECK(iter == node_values.end());
  node_values[op] = val;
}

llvm::APInt &Interpreter::GetNodeVal(Operation *op) {
  auto iter{node_values.find(op)};
  CHECK(iter != node_values.end());
  return iter->second;
}

void Interpreter::SetInstructionBitsValue(uint64_t bits) {
  auto inst{circuit->inst_bits[0]};
  SetNodeVal(inst, llvm::APInt(inst->size, bits));
}

void Interpreter::SetInputRegisterValue(std::string name, uint64_t bits) {
  for (auto reg : circuit->input_regs) {
    if (reg->reg_name == name) {
      SetNodeVal(reg, llvm::APInt(reg->size, bits));
      return;
    }
  }

  LOG(FATAL) << "Input register " << name << " not present in circuit.";
}

void Interpreter::Visit(Operation *op) {
  if (node_values.count(op)) {
    return;
  }
  op->Traverse(*this);
  UniqueVisitor::Visit(op);
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
  CHECK(!node_values.count(op));
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
      SetNodeVal(op, lhs | rhs);
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
  // NOTE(msurovic): This is where we get compute "results".
  // Although I'm not sure if this is the correct way to do it.
  // Consider that we have output register condition checks
  // for various instructions present in the circuit. A valid
  // for one may not be a valid value for the another.
  if (node_values.count(reg)) {
    SetNodeVal(op, GetNodeVal(reg) == GetNodeVal(val) ? TrueVal() : FalseVal());
  } else {
    SetNodeVal(reg, GetNodeVal(val));
    SetNodeVal(op, TrueVal());
  }
}

void Interpreter::VisitPreservedCondition(PreservedCondition *op) {
  DLOG(INFO) << "VisitPreservedCondition: " << op->Name();
  DLOG(INFO) << "IREG: " << op->operands[0]->Name();
  DLOG(INFO) << "OREG: " << op->operands[1]->Name();
  auto ireg{GetNodeVal(op->operands[0])};
  auto oreg{GetNodeVal(op->operands[1])};
  SetNodeVal(op, ireg == oreg ? TrueVal() : FalseVal());
}

void Interpreter::VisitVerifyInstruction(VerifyInstruction *op) {
  DLOG(INFO) << "VisitVerifyInstruction: " << op->Name();
  auto result{GetNodeVal(op->operands[0])};
  for (auto i = 1U; i < op->operands.Size(); ++i) {
    result = result & GetNodeVal(op->operands[i]);
  }
  SetNodeVal(op, result);
  CHECK(false) << "TODO(msurovic): Check this before going further.";
}

void Interpreter::VisitCircuit(Circuit *op) {
  DLOG(INFO) << "VisitCircuit: " << op->Name();
  SetNodeVal(op, GetNodeVal(op->operands[0]));
  CHECK(false) << "TODO(msurovic): Check this before going further.";
}
}  // namespace circuitous