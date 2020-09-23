/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <glog/logging.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>

#include <sstream>
#include <unordered_map>

namespace circuitous {

Operation::~Operation(void) {}

Operation::Operation(unsigned op_code_, unsigned size_)
    : User(this),
      Def<Operation>(this),
      op_code(op_code_),
      size(size_),
      operands(this) {}

Operation::Operation(unsigned op_code_, unsigned size_, Operation *eq_class_)
    : User(this),
      Def<Operation>(this),
      op_code(op_code_),
      size(size_),
      operands(this),
      eq_class(this, eq_class_) {}

bool Operation::Equals(const Operation *that) const {
  if (this == that || (eq_class && eq_class.get() == that->eq_class.get())) {
    return true;
  }

  const auto num_ops = operands.Size();
  if (op_code != that->op_code || size != that->size ||
      num_ops != that->operands.Size()) {
    return false;
  }

  for (auto i = 0u; i < num_ops; ++i) {
    if (!operands[i]->Equals(that->operands[i])) {
      return false;
    }
  }

  return true;
}

const unsigned LLVMOperation::kInvalidLLVMPredicate =
    static_cast<unsigned>(llvm::CmpInst::BAD_ICMP_PREDICATE);

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

}  // namespace

LLVMOperation::LLVMOperation(unsigned llvm_opcode_, unsigned llvm_predicate_,
                             unsigned size_)
    : Operation(Operation::kLLVMOperation, size_),
      llvm_op_code(llvm_opcode_),
      llvm_predicate(llvm_predicate_) {}

LLVMOperation::LLVMOperation(unsigned llvm_opcode_, unsigned llvm_predicate_,
                             unsigned size_, Operation *eq_class_)
    : Operation(Operation::kLLVMOperation, size_, eq_class_),
      llvm_op_code(llvm_opcode_),
      llvm_predicate(llvm_predicate_) {}

LLVMOperation::LLVMOperation(llvm::Instruction *inst)
    : LLVMOperation(inst->getOpcode(), Predicate(inst), SizeOfValue(inst)) {}

LLVMOperation::LLVMOperation(llvm::Instruction *inst, Operation *eq_class_)
    : LLVMOperation(inst->getOpcode(), Predicate(inst), SizeOfValue(inst),
                    eq_class_) {}

#define COMMON_METHODS(cls) \
  cls::~cls(void) {}

#define STREAM_NAME(cls, ...) \
  std::string cls::Name(void) const { \
    std::stringstream ss; \
    ss << __VA_ARGS__; \
    return ss.str(); \
  }

#define RETURN_NAME(cls, ...) \
  std::string cls::Name(void) const { \
    return __VA_ARGS__; \
  }

COMMON_METHODS(LLVMOperation)
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

    const auto num_ops = operands.Size();
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

COMMON_METHODS(EquivalenceClass)
STREAM_NAME(EquivalenceClass, "EQ_CLASS_" << size);

bool EquivalenceClass::Equals(const Operation *that_) const {
  if (that_ == this || that_->eq_class.get() == this ||
      eq_class.get() == that_) {
    return true;
  }

  if (auto that = dynamic_cast<const EquivalenceClass *>(that_); that) {
    for (auto sub_op : operands) {
      if (that->Equals(sub_op)) {
        return true;
      }
    }
  } else {
    for (auto sub_op : operands) {
      if (sub_op->Equals(that_)) {
        return true;
      }
    }
  }
  return false;
}

COMMON_METHODS(Undefined)
STREAM_NAME(Undefined, "UNDEF_" << size)

COMMON_METHODS(Constant)

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

BitOperation::~BitOperation(void) {}

COMMON_METHODS(Not)
RETURN_NAME(Not, "NOT")

COMMON_METHODS(Extract)
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

COMMON_METHODS(Concat)
RETURN_NAME(Concat, "CONCAT")

COMMON_METHODS(PopulationCount)
STREAM_NAME(PopulationCount, "POPULATION_COUNT_" << operands[0]->size)

COMMON_METHODS(Parity)
STREAM_NAME(Parity, "PARITY_" << operands[0]->size)

COMMON_METHODS(CountLeadingZeroes)
STREAM_NAME(CountLeadingZeroes, "COUNT_LEADING_ZEROES_" << operands[0]->size)

COMMON_METHODS(CountTrailingZeroes)
STREAM_NAME(CountTrailingZeroes, "COUNT_TRAILING_ZEROES_" << operands[0]->size)

Condition::~Condition(void) {}

COMMON_METHODS(ReadMemoryCondition)
STREAM_NAME(ReadMemoryCondition, "CHECK_MEM_READ_ADDR_" << operands[0]->size)

COMMON_METHODS(InputRegister)
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

COMMON_METHODS(OutputRegister)
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

COMMON_METHODS(RegisterCondition)
STREAM_NAME(RegisterCondition,
            "OUTPUT_REGISTER_CHECK_"
                << dynamic_cast<OutputRegister *>(operands[1])->reg_name << "_"
                << operands[1]->size)

COMMON_METHODS(PreservedCondition)
STREAM_NAME(PreservedCondition,
            "PRESERED_REGISTER_CHECK_"
                << dynamic_cast<OutputRegister *>(operands[1])->reg_name << "_"
                << operands[1]->size)

COMMON_METHODS(CopyCondition)
STREAM_NAME(CopyCondition,
            "COPIED_REGISTER_CHECK_"
                << dynamic_cast<OutputRegister *>(operands[1])->reg_name << "_"
                << operands[1]->size)

COMMON_METHODS(Hint)
STREAM_NAME(Hint, "HINT_" << size)

COMMON_METHODS(InputInstructionBits)
STREAM_NAME(InputInstructionBits, "INSTRUCTION_BITS_" << size)

COMMON_METHODS(DecodeCondition)
STREAM_NAME(DecodeCondition, "INSTRUCTION_BITS_CHECK_" << operands[0]->size)

COMMON_METHODS(VerifyInstruction)
STREAM_NAME(VerifyInstruction, "ALL_OF_" << operands.Size())

COMMON_METHODS(OnlyOneCondition)
STREAM_NAME(OnlyOneCondition, "ONE_OF_" << operands.Size())

Circuit::~Circuit(void) {
  operands.ClearWithoutErasure();
#define CLEAR_FIELD(type, field) \
  for (auto op : field) { \
    op->operands.ClearWithoutErasure(); \
  }

  FOR_EACH_OPERATION(CLEAR_FIELD)
#undef CLEAR_FIELD
}

RETURN_NAME(Circuit, "RESULT")

#define INIT_FIELD(type, field) , field(this)

Circuit::Circuit(void)
    : Condition(Operation::kCircuit) FOR_EACH_OPERATION(INIT_FIELD) {}

#undef INIT_FIELD

}  // namespace circuitous
