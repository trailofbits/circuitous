/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Intrinsics.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/IRBuilder.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <array>
#include <ranges>

namespace circ::exalt
{
    enum class component_tag : uint32_t
    {
        syscall = 0,
        ebit = 1
    };

    // A simple wrapper - for now just useful to help
    // with tag dispatch.
    struct syscall_reg
    {
        std::string name;
        std::size_t size;
    };

    // Maybe rename just to `component`?
    struct dfa_component
    {
        using builder_t = llvm::IRBuilder<>;
        using reg_ptr_t = Ctx::reg_ptr_t;

        using maybe_value_t = std::optional< llvm::Value * >;

        virtual ~dfa_component() = default;

        // Return `irops::instance< irops::Option >` ?
        // `flag` is the data that should be passed in? For example current ebit or syscall
        // What if I need to pass in more things?
        virtual maybe_value_t in_mux( builder_t &, reg_ptr_t,
                                      llvm::Value *current, llvm::Value *flag ) = 0;

        virtual maybe_value_t out_mux( builder_t &, reg_ptr_t,
                                       llvm::Value *current, llvm::Value *flag ) = 0;

        /* Syscall related hooks */

        virtual maybe_value_t in_mux( builder_t &, syscall_reg,
                                      llvm::Value *current, llvm::Value *flag ) = 0;

        /* Generic hooks */

        virtual component_tag component() const = 0;
    };

    struct dfa_base : dfa_component, has_ctx_ref
    {
        using has_ctx_ref::has_ctx_ref;
    };

    struct syscall_dfa : dfa_base
    {
        using builder_t = dfa_base::builder_t;
        using reg_ptr_t = dfa_base::reg_ptr_t;

        using dfa_base::dfa_base;

        // State and other context related things will live outside in the main
        // owner.
        syscall_submodule &submodule;
        State &state;

        syscall_dfa( CtxRef ctx_ref, syscall_submodule &submodule, State &state )
            : dfa_base( ctx_ref ),
              submodule( submodule ),
              state( state )
        {}

        /* Static information per given dfa - we may need a way to override this later on. */


        /* Generic helpers related to syscalls. */

        static llvm::Value *past_state( builder_t &irb )
        {
            return irops::make_leaf< irops::SyscallState >( irb, irops::io_type::in );
        }

        static llvm::Value *future_state( builder_t &irb )
        {
            return irops::make_leaf< irops::SyscallState >( irb, irops::io_type::out );
        }

        static llvm::Value *is_active( builder_t &irb, llvm::Value *state )
        {
            auto zero = llvm::ConstantInt::get( state->getType(), 0 );
            return irb.CreateICmpNE( state, zero );
        }

        static llvm::Value *is_inactive( builder_t &irb, llvm::Value *state )
        {
            return irb.CreateNot( is_active( irb, state ) );
        }

        /* `dfa_component` implementation. */

        component_tag component() const override
        {
            return component_tag::syscall;
        }

        maybe_value_t out_mux( builder_t &irb, reg_ptr_t reg,
                               llvm::Value *current, llvm::Value *flag ) override
        {
            // We only care if the syscall is ending -> value of EAX should be propagated.
            // Otherwise, every transition happens in syscall registers.
            if ( !submodule.is_output( reg ) )
                return {};
            auto sys_reg = submodule.get( reg );
            auto bw = ctx.bw( current );
            return irops::Option::make( irb, { flag, sys_reg }, bw );
        }

        maybe_value_t in_mux( builder_t &irb, syscall_reg reg,
                              llvm::Value *current, llvm::Value *flag ) override
        {
            // TODO: Here I need hook for state.
            return {};
        }
    };

} // namespace circ::exalt
