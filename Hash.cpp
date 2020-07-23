/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include "Hash.h"

#include <string>
#include <unordered_map>

#include "BitManipulation.h"
#include "IR.h"

namespace circuitous {
namespace {

static const std::hash<std::string> kStringHasher = {};

}  // namespace

class HashVisitor::Impl : protected Visitor<HashVisitor::Impl> {
 public:
  uint64_t Lookup(Operation *op);

  void VisitOperation(Operation *op);
  void VisitConstant(Constant *op);
  void VisitInputRegister(InputRegister *op);
  void VisitOutputRegister(OutputRegister *op);
  void VisitExtract(Extract *op);
  void VisitEquivalenceClass(EquivalenceClass *op);

  std::unordered_map<Operation *, uint64_t> op_hash;
};

uint64_t HashVisitor::Impl::Lookup(Operation *op) {
  auto it = impl->op_hash.find(op);
  if (it == impl->op_hash.end()) {
    Visit(op);
    it = impl->op_hash.find(op);
  }
  return it->second;
}

void HashVisitor::Impl::VisitOperation(Operation *op) {
  uint64_t hash = 0u;
  for (auto sub_op : op->operands) {
    hash ^= RotateRight64(hash, 33u) * Lookup(sub_op);
  }

  hash ^= RotateRight64(hash, 33u) * (op->op_code + 7u);
  hash ^= RotateRight64(hash, 33u) * (op->size + 13u);
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
  auto hash = Lookup(op->operands[0]);
  hash ^= RotateRight64(hash, 33u) * (op->op_code + 7u);
  hash ^= RotateRight64(hash, 33u) * (op->high_hit_exc + 13u);
  hash ^= RotateRight64(hash, 33u) * (op->low_bit_inc + 17u);
  op_hash.emplace(op, hash);
}

void HashVisitor::Impl::VisitEquivalenceClass(EquivalenceClass *op) {
  uint64_t hash = 0u;
  for (auto sub_op : op->operands) {
    hash ^= Lookup(sub_op);
  }
  op_hash.emplace(op, hash);
}

HashVisitor::HashVisitor(void)
    : impl(new Impl) {}

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
