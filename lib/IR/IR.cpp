/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Memory.hpp>

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

namespace circ {

namespace {

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

#define STREAM_NAME(cls, ...) \
  std::string cls::Name(void) const { \
    std::stringstream ss; \
    ss << __VA_ARGS__; \
    return ss.str(); \
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

uint32_t Memory::expected_size(uint32_t ptr_size) {
  return irops::memory::size(ptr_size);
}

}  // namespace circ
