/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

// TODO(lukas): Fill llvm headers
#include <string>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Value.h>
#include <llvm/IR/Module.h>
#pragma clang diagnostic pop

namespace circuitous {
  static inline std::string LLVMName(llvm::Value *val,
                                     const std::string &def = "(name not set)") {
    if (!val->hasName()) {
      return def;
    }
    return val->getName().str();
  }
} // namespace circuitous
