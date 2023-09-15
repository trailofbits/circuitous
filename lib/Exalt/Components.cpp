/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#include <circuitous/Exalt/Components.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Check.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Verifier.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
#include <llvm/Transforms/Utils/Cloning.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <sstream>

namespace circ
{
    /* Initialisation */
    auto circuit_function::make( CtxRef ctx ) -> circuit_function
    {
        auto type = llvm::FunctionType::get( ctx.ir().getInt1Ty(), {}, false );
        auto linkage = llvm::GlobalValue::ExternalLinkage;
        auto fn = llvm::Function::Create( type, linkage, "__circ.circuit_v2", ctx.module() );

        llvm::BasicBlock::Create( *ctx.llvm_ctx(), "entry", fn );
        return circuit_function( *fn );
    }

    auto function_context::make( CtxRef ctx_ref ) -> function_context
    {
        return function_context( circuit_function::make( ctx_ref ) );
    }

    void submodules::init( builder_context & )
    {

    }

    auto submodules::exalt_prologue( unit_t &unit ) -> exalted_value_buckets
    {
        arch_state.reset( fn_ctx.irb() );
        return {};
    }

}  // namespace circ
