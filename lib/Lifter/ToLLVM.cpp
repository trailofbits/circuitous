/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/ToLLVM.hpp>

#include <circuitous/Support/Check.hpp>

namespace circ
{
    // Simply convert circuit to llvm function.
    // Returns context and function.
    auto convert_to_llvm( Circuit *circuit )
        -> std::tuple< std::shared_ptr< llvm::LLVMContext >, llvm::Function * >
    {
        log_kill() << "Not implemented";
    }

    void convert_to_llvm( Circuit *circuit, llvm::IRBuilder<> &irb )
    {
        log_kill() << "Not implemented";
    }

    // Convert circuit to llvm function with given name in the provided module.
    auto convert_to_llvm( Circuit *circuit, llvm::Module *lmodule, const std::string &name )
        -> llvm::Function *
    {
        log_kill() << "Not implemented";
    }

} // namespace circ
