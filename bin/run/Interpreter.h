/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <llvm/ADT/APInt.h>

#include <optional>
#include <unordered_map>

namespace circuitous {

class Interpreter : public Visitor<Interpreter> {
  using raw_value_type = llvm::APInt;
  // If no value is held <=> value is undefined
  using value_type = std::optional<raw_value_type>;

  Circuit *circuit{nullptr};
  bool changed{false};

  std::unordered_map<Operation *, value_type> node_values;

  value_type Undef() { LOG(FATAL) << "Cannot produce undef."; return {}; }
  llvm::APInt TrueVal() { return llvm::APInt(1, 1); }
  llvm::APInt FalseVal() { return llvm::APInt(1, 0); }

  void SetNodeVal(Operation *op, const raw_value_type &val);
  void SetNodeVal(Operation *op, const value_type &val);
  value_type GetNodeVal(Operation *op);

  template<typename ...Args>
  bool ValidVals(Args &&... args) {
    return (GetNodeVal(args).has_value() && ...);
  }

  template<uint64_t ... idxs>
  bool ValidChildren(Operation *op) {
    for (std::size_t i = 0; i < op->operands.size(); ++i) {
      if (!GetNodeVal(op->operands[i])) {
        return false;
      }
    }
    return true;
  }

 public:
  Interpreter(Circuit *circuit);
  void SetInstructionBitsValue(const std::string &bits);
  void SetInputRegisterValue(const std::string &name, uint64_t bits);
  std::optional<uint64_t> GetOutputRegisterValue(const std::string &name);

  void Visit(Operation *op);

  // Default
  void VisitOperation(Operation *op);

  // Operands
  void VisitConstant(Constant *op);
  void VisitInputRegister(InputRegister *op);
  void VisitInputImmediate(InputImmediate *op);
  void VisitOutputRegister(OutputRegister *op);
  void VisitInputInstructionBits(InputInstructionBits *op);
  void VisitHint(Hint *op);
  void VisitUndefined(Undefined *op);

  // Operations
  void VisitConcat(Concat *op);
  void VisitExtract(Extract *op);
  void VisitNot(Not *op);
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

  std::unordered_map<Operation *, llvm::APInt> values() const {
    std::unordered_map<Operation *, llvm::APInt> out;
    for (auto &[op, val] : node_values) {
      if (val) {
        out[op] = *val;
      }
    }
    return out;
  }
};
}  // namespace circuitous