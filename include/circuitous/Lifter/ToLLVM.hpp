/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/Support/Check.hpp>

#include <utility>
#include <memory>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/IRBuilder.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace llvm
{
    class Function;
    class Module;
    class LLVMContext;
} // namespace llvm

namespace circ
{
    // Simply convert circuit to llvm function.
    // Returns context and function.
    auto convert_to_llvm( Circuit *circuit )
    -> std::tuple< std::shared_ptr< llvm::LLVMContext >, llvm::Function * >;

    // Convert circuit to llvm function using only the provided builder.
    // It is expected this builder already is properly set up - i.e. has an
    // insertion point to some basic block.
    void convert_to_llvm( Circuit *circuit, llvm::IRBuilder<> &irb );

    // Convert circuit to llvm function with given name in the provided module.
    auto convert_to_llvm( Circuit *circuit, llvm::Module *lmodule, const std::string &name )
    -> llvm::Function *;

} // namespace circ
