/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Exalt/Common.hpp>
#include <circuitous/IR/Intrinsics.hpp>

CIRCUITOUS_RELAX_WARNINGS
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ
{
    /* llvm-16 was too conservative with CSE on function calls due to
     * coroutines. We know there are none, so we implement a super simple
     * replacement ourselves.
     */

    void cse( llvm::Function &circuit_fn )
    {
        using call_args_t = values_t;
        using calls = std::vector< llvm::CallInst * >;
        using call_mapping = std::vector< std::tuple< call_args_t, calls > >;

        std::unordered_map< llvm::Function *, call_mapping > cache;

        auto add_mapping = [ & ]( llvm::CallInst *call, llvm::Function *callee )
        {
            auto &mapping = cache[ callee ];
            auto current_args = freeze< std::vector >( call_args( call ) );

            for ( auto &[ args, entries ] : mapping )
            {
                if ( current_args == args )
                {
                    entries.push_back( call );
                    return;
                }

            }
            mapping.emplace_back( current_args, calls{ call } );
        };

        log_dbg() << "[exalt:cse]:" << "Computing irops calls to de-duplicate.";
        for ( auto &inst : *( circuit_fn.begin() ) )
        {
            auto call = llvm::dyn_cast< llvm::CallInst >( &inst );
            if ( !call )
                continue;

            auto callee = call->getCalledFunction();
            if ( !irops::is_any( call ) || irops::is_frozen( callee ) )
                continue;
            add_mapping( call, callee );
        }

        log_dbg() << "[exalt:cse]:" << "Elimination.";
        for ( auto &[ fn, e ] : cache )
        {
            for ( auto &[ _, to_replace ] : e )
            {
                check( to_replace.size() >= 1 );

                if ( to_replace.size() == 1 )
                    continue;

                log_dbg() << "[exalt:cse]:" << fn->getName().str()
                          << "has" << to_replace.size() << " calls to eliminate.";
                for ( std::size_t i = 1; i < to_replace.size(); ++i )
                {
                    to_replace[ i ]->replaceAllUsesWith( to_replace[ 0 ] );
                    to_replace[ i ]->eraseFromParent();
                }
            }
        }

    }


} // namespace circ
