/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Hash.h>
#include <circuitous/IR/IR.h>
#include <circuitous/Util/BitManipulation.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include <string>
#include <unordered_map>

namespace circuitous {
namespace {

static const std::hash<std::string> kStringHasher = {};

}  // namespace

class HashVisitor::Impl : public Visitor<HashVisitor::Impl> {
 public:
  uint64_t Lookup(Operation *op);

  void VisitOperation(Operation *op);
  void VisitLLVMOperation(LLVMOperation *op);
  void VisitConstant(Constant *op);
  void VisitInputRegister(InputRegister *op);
  void VisitOutputRegister(OutputRegister *op);
  void VisitExtract(Extract *op);
  void VisitEquivalenceClass(EquivalenceClass *op);

  std::unordered_map<Operation *, uint64_t> op_hash;
};

uint64_t HashVisitor::Impl::Lookup(Operation *op) {
  auto it = op_hash.find(op);
  if (it == op_hash.end()) {
    Visit(op);
    it = op_hash.find(op);
  }
  return it->second;
}

void HashVisitor::Impl::VisitOperation(Operation *op) {
  uint64_t hash = kStringHasher(op->Name());
  for (auto sub_op : op->operands) {
    hash ^= RotateRight64(hash, 33u) * Lookup(sub_op);
  }
  op_hash.emplace(op, hash);
}

void HashVisitor::Impl::VisitLLVMOperation(LLVMOperation *op) {
  uint64_t hash = kStringHasher(op->Name());
  if (llvm::Instruction::isCommutative(op->op_code) ||
      llvm::CmpInst::ICMP_EQ == op->llvm_predicate ||
      llvm::CmpInst::ICMP_NE == op->llvm_predicate) {
    for (auto sub_op : op->operands) {
      hash ^= Lookup(sub_op);
    }
  } else {
    for (auto sub_op : op->operands) {
      hash ^= RotateRight64(hash, 33u) * Lookup(sub_op);
    }
  }
  op_hash.emplace(op, hash);
}

void HashVisitor::Impl::VisitConstant(Constant *op) {
  uint64_t hash = kStringHasher(op->bits);
  hash ^= RotateRight64(hash, 33u) * (op->op_code + 7u);
  op_hash.emplace(op, hash);
}

void HashVisitor::Impl::VisitInputRegister(InputRegister *op) {
  uint64_t hash = kStringHasher(op->reg_name);
  hash ^= RotateRight64(hash, 33u) * (op->op_code + 7u);
  op_hash.emplace(op, hash);
}

void HashVisitor::Impl::VisitOutputRegister(OutputRegister *op) {
  uint64_t hash = kStringHasher(op->reg_name);
  hash ^= RotateRight64(hash, 33u) * (op->op_code + 7u);
  op_hash.emplace(op, hash);
}

void HashVisitor::Impl::VisitExtract(Extract *op) {
  auto hash = kStringHasher(op->Name());
  hash ^= RotateRight64(hash, 33u) * Lookup(op->operands[0]);
  op_hash.emplace(op, hash);
}

void HashVisitor::Impl::VisitEquivalenceClass(EquivalenceClass *op) {
  uint64_t hash = 0u;
  for (auto sub_op : op->operands) {
    hash ^= Lookup(sub_op);
  }
  op_hash.emplace(op, hash);
}

HashVisitor::~HashVisitor(void) {}

HashVisitor::HashVisitor(void) : impl(new Impl) {}

uint64_t HashVisitor::operator[](Operation *op) {
  return impl->Lookup(op);
}

// Force `op` to have the hash `hash_val`. This is a convenient way of
// poking holes.
void HashVisitor::Force(Operation *op, uint64_t hash_val) {
  impl->op_hash.emplace(op, hash_val);
}

// Reset `op_hash`.
void HashVisitor::Reset(void) {
  impl->op_hash.clear();
}

}  // namespace circuitous
