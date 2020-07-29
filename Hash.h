/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <cstdint>
#include <memory>

namespace circuitous {

class Operation;

class HashVisitor {
 public:
  ~HashVisitor(void);
  HashVisitor(void);

  // Lookup an operator.
  uint64_t operator[](Operation *op);

  // Force `op` to have the hash `hash_val`. This is a convenient way of
  // poking holes.
  void Force(Operation *op, uint64_t hash_val);

  // Reset `op_hash`.
  void Reset(void);

 private:
  class Impl;

  std::unique_ptr<Impl> impl;
};

}  // namespace circuitous
