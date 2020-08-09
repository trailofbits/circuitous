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
      eq_class(eq_class_->CreateWeakUse(this)) {}


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

COMMON_METHODS(EquivalenceClass)
STREAM_NAME(EquivalenceClass, "EQ_CLASS_" << size);

COMMON_METHODS(Constant)
STREAM_NAME(Constant, "CONST_" << size << "_" << bits)

BitOperation::~BitOperation(void) {}

COMMON_METHODS(Extract)
STREAM_NAME(Extract, "EXTRACT_" << high_bit_exc << "_" << low_bit_inc)

COMMON_METHODS(Concat)
RETURN_NAME(Concat, "CONCAT")

COMMON_METHODS(PopulationCount)
STREAM_NAME(PopulationCount, "POPULATION_COUNT_" << operands[0]->size)

COMMON_METHODS(CountLeadingZeroes)
STREAM_NAME(CountLeadingZeroes, "COUNT_LEADING_ZEROES_" << operands[0]->size)

COMMON_METHODS(CountTrailingZeroes)
STREAM_NAME(CountTrailingZeroes, "COUNT_TRAILING_ZEROES_" << operands[0]->size)

COMMON_METHODS(ZeroFillLeft)
STREAM_NAME(ZeroFillLeft, "ZERO_FILL_LEFT_" << (size - operands[0]->size))

COMMON_METHODS(ZeroFillRight)
STREAM_NAME(ZeroFillRight, "ZERO_FILL_RIGHT_" << (size - operands[0]->size))

COMMON_METHODS(SignExtend)
STREAM_NAME(SignExtend, "SIGN_EXTEND_" << operands[0]->size << "_" << size)

Condition::~Condition(void) {}

COMMON_METHODS(ReadMemoryCondition)
STREAM_NAME(ReadMemoryCondition, "CHECK_MEM_READ_ADDR_" << operands[0]->size)

COMMON_METHODS(InputRegister)
STREAM_NAME(InputRegister, "INPUT_REGISTER_" << reg_name << "_" << size)

COMMON_METHODS(OutputRegister)
STREAM_NAME(OutputRegister, "OUTPUT_REGISTER_" << reg_name << "_" << size)

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
