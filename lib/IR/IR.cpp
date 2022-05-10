/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.hpp>
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

std::string Operation::name() const {
  unreachable() << util::to_underlying(op_code) << " does not provide name() method override.";
}

std::string Constant::name() const {
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
