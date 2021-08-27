/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Hash.h>
#include <circuitous/IR/Circuit.hpp>

namespace circ {

uint64_t IdentityHasher::operator[](Operation *op) { return op->id(); }

}  // namespace circ
