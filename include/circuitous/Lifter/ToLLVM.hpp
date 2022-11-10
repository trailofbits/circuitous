/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/Support/Check.hpp>

#include <utility>
#include <memory>
#include <string>

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
    [[ nodiscard ]]
    std::tuple< std::shared_ptr< llvm::LLVMContext >, std::unique_ptr< llvm::Module > >
    convert_to_llvm( circuit_ref_t circuit, const std::string &module_name );

    // Convert circuit to llvm function using only the provided builder.
    // It is expected this builder already is properly set up - i.e. has an
    // insertion point to some basic block.
    void convert_to_llvm( Circuit *circuit, llvm::IRBuilder<> &irb );

    // Convert circuit to llvm function with given name in the provided module.
    [[ nodiscard ]] llvm::Function *
    convert_to_llvm( circuit_ref_t circuit, llvm::Module *lmodule, const std::string &name );

} // namespace circ
