/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Util/Warnings.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/Module.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ
{
    circuit_owner_t lower_fn(llvm::Function *circuit_func, std::size_t ptr_size);

    // Expects there is exactly one function with a body to be lowered.
    circuit_owner_t lower_module(llvm::Module *circuit_module, std::size_t ptr_size);

} // namespace circ
