/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <unordered_map>

#include <circuitous/IR/IR.h>
#include <llvm/ADT/APInt.h>

namespace circuitous {
class Interpreter : public Visitor<Interpreter> {
 private:
  
  inline llvm::APInt TrueVal() {
    return llvm::APInt(1, 1);
  }

  inline llvm::APInt FalseVal() {
    return llvm::APInt(1, 0);
  }

  Circuit *circuit;

  bool changed{false};

  std::unordered_map<Operation *, llvm::APInt> node_values;

  void SetNodeVal(Operation *op, const llvm::APInt &val);
  llvm::APInt &GetNodeVal(Operation *op);

 public:
  Interpreter(Circuit *circuit);
  void SetInstructionBitsValue(const std::string &bits);
  void SetInputRegisterValue(const std::string &name, uint64_t bits);
  uint64_t GetOutputRegisterValue(const std::string &name);
  void Visit(Operation *op);
  // Default
  void VisitOperation(Operation *op);
  // Operands
  void VisitConstant(Constant *op);
  void VisitInputRegister(InputRegister *op);
  void VisitOutputRegister(OutputRegister *op);
  void VisitInputInstructionBits(InputInstructionBits *op);
  void VisitHint(Hint *op);
  void VisitUndefined(Undefined *op);
  // Operations
  void VisitExtract(Extract *op);
  void VisitLLVMOperation(LLVMOperation *op);
  void VisitParity(Parity *op);
  void VisitPopulationCount(PopulationCount *op);
  // Conditions
  void VisitDecodeCondition(DecodeCondition *op);
  void VisitRegisterCondition(RegisterCondition *op);
  void VisitPreservedCondition(PreservedCondition *op);
  void VisitCopyCondition(CopyCondition *op);
  void VisitHintCondition(HintCondition *op);
  void VisitVerifyInstruction(VerifyInstruction *op);
  void VisitOnlyOneCondition(OnlyOneCondition *op);
  // Circuit
  void VisitCircuit(Circuit *op);
  // Runner
  bool Run();
};
}  // namespace circuitous
