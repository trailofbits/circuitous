/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Intrinsics.hpp>
#include <circuitous/Lifter/DependencyVisitor.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/IRBuilder.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ
{

    static inline void replace_undef( llvm::Instruction *inst, unsigned int idx )
    {
        auto constraint_instance = UndefChain< irops::OutputCheck >().get( inst );
        auto output_reg = irops::Instance< irops::Reg >( constraint_instance.fixed() );

        check( output_reg );
        auto [ size, reg, io_type ] = irops::Reg::parse_args( output_reg.fn );

        llvm::IRBuilder<> irb( inst );
        auto value = irops::make_leaf< irops::Reg >( irb, size, reg, irops::twin( io_type ) );


        auto undef = inst->getOperand( idx );

        auto coerced = irb.CreateSExtOrTrunc( value, undef->getType() );
        inst->setOperand( idx, coerced );
    }

    static inline void propagate_remill_undefs( llvm::Function *circuit_fn )
    {
        // We need to freeze them as we will be modifying the function.
        std::vector< std::tuple< llvm::Instruction *, unsigned int > > with_undefs;

        for ( auto &bb : *circuit_fn )
            for ( auto &inst : bb )
            {
                unsigned int idx = 0;
                for ( auto user : inst.operand_values() )
                {
                    if ( auto undef = llvm::dyn_cast< llvm::UndefValue >( user ) )
                        with_undefs.emplace_back( &inst, idx );
                    ++idx;
                }
            }

        for ( auto [ inst, idx ] : with_undefs )
            replace_undef( inst, idx );
    }

    static inline void replace_remill_undefs( llvm::Function *circuit_fn,
                                              std::string undef_fn )
    {
        // We need to freeze them as we will be modifying the function.
        std::vector< llvm::CallInst * > origins;

        for ( auto &bb : *circuit_fn )
            for ( auto &inst : bb )
                if ( auto call = llvm::dyn_cast< llvm::CallInst >( &inst ) )
                {
                    // This should never be the case in the circuit function,
                    // so we can be do a hard assert here.
                    check( call->getCalledFunction()->hasName() );
                    if ( call->getCalledFunction()->getName() == undef_fn )
                        origins.push_back( call );
                }

        for ( auto origin : origins )
        {
            auto undef = llvm::UndefValue::get( origin->getType() );

            origin->replaceAllUsesWith( undef );
            origin->eraseFromParent();
        }
    }


    static inline void replace_remill_undefs( llvm::Function *circuit_fn )
    {
        for ( auto s : { 8, 16, 32, 64 } )
            replace_remill_undefs( circuit_fn, "__remill_undefined_" + std::to_string( s ) );
    }
} // namespace circ
