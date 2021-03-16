/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>

#include <unordered_map>
#include <sstream>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Instruction.h>
#pragma clang diagnostic pop

namespace circuitous {

struct RawNodesCounter : UniqueVisitor<RawNodesCounter> {
  using parent = UniqueVisitor<RawNodesCounter>;

  std::unordered_map<uint64_t, uint64_t> nodes;
  std::unordered_map<uint32_t, uint64_t> llvm_ops;

  void Visit(Operation *op) {
    const auto &[it, _] = nodes.try_emplace(op->op_code, 0);
    ++it->second;
    op->Traverse(*this);
  }

  void VisitLLVMOperation(LLVMOperation *op) {
    const auto &[it, _] = llvm_ops.try_emplace(op->op_code, 0);
    ++it->second;
    op->Traverse(*this);
  }

  void Run(Operation *op) {
    Visit(op);
  }
};

template<typename Collector>
struct PrintStatistics : Collector {

  template<typename OS>
  void Run(Operation *op, OS &os) {
    this->Collector::Run(op);
    Print(os);
  }

  template<typename OS>
  void Print(OS &ss) {
    ss << "Node counts:" << std::endl;
    for (auto &[op_code, count] : this->nodes) {
      ss << " " << to_string(op_code) << " " << count << std::endl;

      if (op_code == Operation::kLLVMOperation) {
        for (auto &[llvm_op, count] : this->llvm_ops) {
          ss << "\t " << llvm::Instruction::getOpcodeName(llvm_op) << " " << count << std::endl;
        }
      }
    }
    ss << std::endl;
  }
};

// TODO(lukas): Make more configurable
std::string GetStats(Operation *op) {
  std::stringstream ss;
  PrintStatistics<RawNodesCounter>().Run(op, ss);
  return ss.str();
}

} // namespace circuitous