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
    if (auto cmp = llvm::dyn_cast<llvm::CmpInst>(inst)) {
      return cmp->getPredicate();
    }
    return LLVMOperation::kInvalidLLVMPredicate;
  }

  template<typename ...Args>
  std::string StreamName(Args &&... args) {
    std::stringstream ss;
    (ss << ... << args);
    return ss.str();
  }

  template<typename T>
  bool RegEquals(const T& self, const Operation *that_) {
    if (&self == that_) {
      return true;
    }
    if (auto that = dynamic_cast<const T *>(that_)) {
      return self.reg_name == that->reg_name;
    }
    return self.Operation::Equals(that_);
  }

}  // namespace

const uint32_t LLVMOperation::kInvalidLLVMPredicate =
    static_cast<uint32_t>(llvm::CmpInst::BAD_ICMP_PREDICATE);

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

std::string Operation::Name() const {
  LOG(FATAL) << op_code << " does not provide Name() method override.";
}

LLVMOperation::LLVMOperation(unsigned llvm_opcode_, unsigned llvm_predicate_,
                             unsigned size_)
    : Operation(size_, LLVMOperation::kind),
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

std::string LLVMOperation::Name() const {
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
  }

  if (auto that = dynamic_cast<const LLVMOperation *>(that_)) {
    if (size != that->size ||
        llvm_op_code != that->llvm_op_code ||
        llvm_predicate != that->llvm_predicate) {
      return false;
    }

    const auto num_ops = operands.size();
    if (1u == num_ops) {
      return operands[0]->Equals(that->operands[0]);
    }

    if (2u == num_ops) {
      const auto this_lhs = operands[0];
      const auto that_lhs = that->operands[0];
      const auto this_rhs = operands[1];
      const auto that_rhs = that->operands[1];
      if (llvm::Instruction::isCommutative(llvm_op_code) ||
          llvm::CmpInst::ICMP_EQ == llvm_predicate ||
          llvm::CmpInst::ICMP_NE == llvm_predicate) {
        // It is commutative, we need to check both possible orderings
        return (this_lhs->Equals(that_lhs) && this_rhs->Equals(that_rhs)) ||
               (this_lhs->Equals(that_rhs) && this_rhs->Equals(that_lhs));
      }
      return this_lhs->Equals(that_lhs) && this_rhs->Equals(that_rhs);
    }
  }

  return this->Operation::Equals(that_);
}

std::string Constant::Name() const {
  std::stringstream ss;
  ss << "CONST_" << size << "_";
  for (auto i = 0U; i < size; ++i) {
    ss << bits[size - i - 1];
  }
  return ss.str();
}

bool Constant::Equals(const Operation *that_) const {
  if (auto that = dynamic_cast<const Constant *>(that_)) {
    return bits == that->bits;
  }
  return this->Operation::Equals(that_);
}

bool Extract::Equals(const Operation *that_) const {
  if (this == that_) {
    return true;
  }
  if (auto that = dynamic_cast<const Extract *>(that_); that) {
    return size == that->size && high_bit_exc == that->high_bit_exc &&
           low_bit_inc == that->low_bit_inc &&
           operands[0]->Equals(that->operands[0]);
  }
  return this->Operation::Equals(that_);
}

bool InputImmediate::Equals(const Operation *other) const {
  return this->Operation::Equals(other);
}

Circuit::Circuit() : Operation(1u, Circuit::kind)  {}

}  // namespace circuitous
