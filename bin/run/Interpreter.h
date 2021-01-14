/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <llvm/ADT/APInt.h>

namespace circuitous {
class Interpreter : public UniqueVisitor<Interpreter> {
 private:
  
  inline llvm::APInt TrueVal() {
    return llvm::APInt(1, 1);
  }

  inline llvm::APInt FalseVal() {
    return llvm::APInt(1, 0);
  }

  Circuit *circuit;

  std::unordered_map<Operation *, llvm::APInt> node_values;

  void SetNodeVal(Operation *op, const llvm::APInt &val);
  llvm::APInt &GetNodeVal(Operation *op);

 public:
  Interpreter(Circuit *circuit);
  void SetInstructionBitsValue(uint64_t bits);
  void SetInputRegisterValue(std::string name, uint64_t bits);
  void Visit(Operation *op);
  // Default
  void VisitOperation(Operation *op);
  // Operands
  void VisitConstant(Constant *op);
  void VisitInputRegister(InputRegister *op);
  void VisitOutputRegister(OutputRegister *op);
  void VisitInputInstructionBits(InputInstructionBits *op);
  // Operations
  void VisitExtract(Extract *op);
  void VisitLLVMOperation(LLVMOperation *op);
  // Conditions
  void VisitDecodeCondition(DecodeCondition *op);
  void VisitRegisterCondition(RegisterCondition *op);
  void VisitPreservedCondition(PreservedCondition *op);
  void VisitVerifyInstruction(VerifyInstruction *op);
  // Circuit
  void VisitCircuit(Circuit *op);
};
}  // namespace circuitous