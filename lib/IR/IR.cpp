/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <glog/logging.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#pragma clang diagnostic pop

#include <sstream>
#include <unordered_map>

namespace circuitous {

namespace {

static unsigned SizeOfValue(llvm::Instruction *inst) {
  auto module = inst->getParent()->getParent()->getParent();
  const auto &dl = module->getDataLayout();
  return static_cast<unsigned>(dl.getTypeSizeInBits(inst->getType()));
}

static unsigned Predicate(llvm::Instruction *inst) {
  if (auto cmp = llvm::dyn_cast<llvm::CmpInst>(inst); cmp) {
    return cmp->getPredicate();
  } else {
    return LLVMOperation::kInvalidLLVMPredicate;
  }
}

template<typename ...Args>
std::string StreamName(Args &&... args) {
  std::stringstream ss;
  (ss << ... << args);
  return ss.str();
}

}  // namespace


const uint32_t LLVMOperation::kInvalidLLVMPredicate =
    static_cast<uint32_t>(llvm::CmpInst::BAD_ICMP_PREDICATE);

Operation::Operation(unsigned op_code_, unsigned size_)
    : op_code(op_code_),
      size(size_) {}

bool Operation::Equals(const Operation *that) const {
  if (this == that) {
    return true;
  }

  const auto num_ops = operands.size();
  if (op_code != that->op_code || size != that->size ||
      num_ops != that->operands.size()) {
    return false;
  }

  for (auto i = 0u; i < num_ops; ++i) {
    if (!operands[i]->Equals(that->operands[i])) {
      return false;
    }
  }

  return true;
}

std::string Operation::Name(void) const {
  return to_string(op_code);
}

LLVMOperation::LLVMOperation(unsigned llvm_opcode_, unsigned llvm_predicate_,
                             unsigned size_)
    : Operation(Operation::kLLVMOperation, size_),
      llvm_op_code(llvm_opcode_),
      llvm_predicate(llvm_predicate_) {}

LLVMOperation::LLVMOperation(llvm::Instruction *inst)
    : LLVMOperation(inst->getOpcode(), Predicate(inst), SizeOfValue(inst)) {}

#define STREAM_NAME(cls, ...) \
  std::string cls::Name(void) const { \
    std::stringstream ss; \
    ss << __VA_ARGS__; \
    return ss.str(); \
  }


Operation *LLVMOperation::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<LLVMOperation>(llvm_op_code, llvm_predicate, size);
}

std::string LLVMOperation::Name(void) const {
  std::stringstream ss;
  ss << "LLVM_" << llvm::Instruction::getOpcodeName(llvm_op_code);
  if (llvm_predicate != LLVMOperation::kInvalidLLVMPredicate) {
    const auto pred = static_cast<llvm::CmpInst::Predicate>(llvm_predicate);
    ss << '_' << llvm::CmpInst::getPredicateName(pred).str();
  }
  ss << '_' << size;
  return ss.str();
}

bool LLVMOperation::Equals(const Operation *that_) const {
  if (this == that_) {
    return true;
  } else if (auto that = dynamic_cast<const LLVMOperation *>(that_); that) {
    if (size != that->size || llvm_op_code != that->llvm_op_code ||
        llvm_predicate != that->llvm_predicate) {
      return false;
    }

    const auto num_ops = operands.size();
    if (2u == num_ops) {
      const auto this_lhs = operands[0];
      const auto that_lhs = that->operands[0];
      const auto this_rhs = operands[1];
      const auto that_rhs = that->operands[1];
      if (llvm::Instruction::isCommutative(llvm_op_code) ||
          llvm::CmpInst::ICMP_EQ == llvm_predicate ||
          llvm::CmpInst::ICMP_NE == llvm_predicate) {
        return (this_lhs->Equals(that_lhs) && this_rhs->Equals(that_rhs)) ||
               (this_lhs->Equals(that_rhs) && this_rhs->Equals(that_lhs));
      } else {
        return this_lhs->Equals(that_lhs) && this_rhs->Equals(that_rhs);
      }

    } else if (1u == num_ops) {
      return operands[0]->Equals(that->operands[0]);
    }
  }
  return this->Operation::Equals(that_);
}

STREAM_NAME(Undefined, "UNDEF_" << size)
Operation *Undefined::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<Undefined>(size);
}


std::string Constant::Name(void) const {
  std::stringstream ss;
  ss << "CONST_" << size << "_";
  for (auto i = 0U; i < size; ++i) {
    ss << bits[size - i - 1];
  }
  return ss.str();
}

bool Constant::Equals(const Operation *that_) const {
  if (auto that = dynamic_cast<const Constant *>(that_); that) {
    return bits == that->bits;
  } else {
    return this->Operation::Equals(that_);
  }
}

std::string BitOperation::Name(void) const {
  if (this->operands.size() == 0) {
    LOG(ERROR) << this->id() << ": " << to_string(this->op_code) << " has no operands!";
    return StreamName(to_string(this->op_code), "_", "IS_INVALID");
  }
  return StreamName(to_string(op_code), "_", this->operands[0]->size);
}

Operation *Constant::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<Constant>(bits, size);
}

Operation *Not::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<Not>(size);
}

STREAM_NAME(Extract, "EXTRACT_" << high_bit_exc << "_" << low_bit_inc)

bool Extract::Equals(const Operation *that_) const {
  if (this == that_) {
    return true;
  } else if (auto that = dynamic_cast<const Extract *>(that_); that) {
    return size == that->size && high_bit_exc == that->high_bit_exc &&
           low_bit_inc == that->low_bit_inc &&
           operands[0]->Equals(that->operands[0]);
  } else {
    return this->Operation::Equals(that_);
  }
}

Operation *Extract::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<Extract>(low_bit_inc, high_bit_exc);
}

Operation *Concat::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<Concat>(size);
}

Operation *PopulationCount::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<PopulationCount>(size);
}

Operation *CountLeadingZeroes::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<CountLeadingZeroes>(size);
}

Operation *CountTrailingZeroes::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<CountTrailingZeroes>(size);
}

Operation *Parity::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<Parity>();
}

std::string Parity::Name() const {
  CHECK(operands.size() != 0);
  return StreamName(to_string(this->op_code), "_", operands[0]-size);
}

STREAM_NAME(ReadMemoryCondition, "CHECK_MEM_READ_ADDR_" << operands[0]->size)

Operation *ReadMemoryCondition::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<ReadMemoryCondition>();
}

STREAM_NAME(InputRegister, "INPUT_REGISTER_" << reg_name << "_" << size)

bool InputRegister::Equals(const Operation *that_) const {
  if (this == that_) {
    return true;
  } else if (auto that = dynamic_cast<const InputRegister *>(that_); that) {
    return reg_name == that->reg_name;
  } else {
    return this->Operation::Equals(that_);
  }
}

Operation *InputRegister::CloneWithoutOperands(Circuit *) const {
  return const_cast<InputRegister *>(this);
}

STREAM_NAME(InputImmediate, "INPUT_IMMEDIATE_" << size)
bool InputImmediate::Equals(const Operation *other) const {
  return this->Operation::Equals(other);
}

Operation *InputImmediate::CloneWithoutOperands(Circuit *) const {
  return const_cast<InputImmediate *>(this);
}

STREAM_NAME(OutputRegister, "OUTPUT_REGISTER_" << reg_name << "_" << size)

bool OutputRegister::Equals(const Operation *that_) const {
  if (this == that_) {
    return true;
  } else if (auto that = dynamic_cast<const OutputRegister *>(that_); that) {
    return reg_name == that->reg_name;
  } else {
    return this->Operation::Equals(that_);
  }
}

Operation *OutputRegister::CloneWithoutOperands(Circuit *) const {
  LOG(FATAL) << "Not implemented";
  return const_cast<OutputRegister *>(this);
}

std::string RegisterCondition::Name() const {
  CHECK(operands.size() == 2);
  auto out_reg = dynamic_cast<OutputRegister *>(operands[kOutputRegister]);
  CHECK(out_reg) << (operands[kOutputRegister]->op_code);
  return StreamName(to_string(this->op_code), "_", out_reg->reg_name, "_", out_reg->size);
}

Operation *RegisterCondition::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<RegisterCondition>();
}

STREAM_NAME(HintCondition, "HINT_CHECK_" << operands[kHint]->size)

Operation *HintCondition::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<HintCondition>();
}

STREAM_NAME(
    PreservedCondition,
    "PRESERVED_REGISTER_CHECK_"
        << dynamic_cast<OutputRegister *>(operands[kOutputRegister])->reg_name
        << "_" << operands[kOutputRegister]->size)

Operation *PreservedCondition::CloneWithoutOperands(Circuit *circuit) const {
  // TOOD(lukas): Was originally, is it correct?
  // return circuit->transitions.Create();
  return circuit->Create<RegisterCondition>();
}

STREAM_NAME(
    CopyCondition,
    "COPIED_REGISTER_CHECK_"
        << dynamic_cast<OutputRegister *>(operands[kOutputRegister])->reg_name
        << "_" << operands[kOutputRegister]->size)

Operation *CopyCondition::CloneWithoutOperands(Circuit *circuit) const {
  // TODO(lukas): Same as above
  return circuit->Create<RegisterCondition>();
}

STREAM_NAME(Hint, "HINT_" << size)

Operation *Hint::CloneWithoutOperands(Circuit *) const {
  return const_cast<Hint *>(this);
}

STREAM_NAME(InputInstructionBits, "INSTRUCTION_BITS_" << size)

Operation *InputInstructionBits::CloneWithoutOperands(Circuit *) const {
  return const_cast<InputInstructionBits *>(this);
}

STREAM_NAME(DecodeCondition, "INSTRUCTION_BITS_CHECK_" << operands[0]->size)

Operation *DecodeCondition::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<DecodeCondition>();
}

STREAM_NAME(VerifyInstruction, "ALL_OF_" << operands.size())

Operation *VerifyInstruction::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<VerifyInstruction>();
}

STREAM_NAME(OnlyOneCondition, "ONE_OF_" << operands.size())

Operation *OnlyOneCondition::CloneWithoutOperands(Circuit *circuit) const {
  return circuit->Create<OnlyOneCondition>();
}

Circuit::~Circuit(void) {
  // TODO(lukas): Maybe somethign is needed here?
}

Operation *Circuit::CloneWithoutOperands(Circuit *) const {
  LOG(FATAL) << "Not allowed to clone the circuit";
  return nullptr;
}

Circuit::Circuit(void)
    : Condition(Operation::kCircuit) {}

}  // namespace circuitous
