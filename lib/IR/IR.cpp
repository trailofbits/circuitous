/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Memory.hpp>

#include <circuitous/Util/Logging.hpp>
#include <circuitous/Support/Check.hpp>


CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
CIRCUITOUS_UNRELAX_WARNINGS

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

}  // namespace

std::string Operation::Name() const {
  unreachable() << util::to_underlying(op_code) << " does not provide Name() method override.";
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

uint32_t Memory::expected_size(uint32_t ptr_size) {
  return irops::memory::size(ptr_size);
}

}  // namespace circ
