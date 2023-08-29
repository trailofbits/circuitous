/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Lifter/Context.hpp>

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/Lifter.h>

#include <unordered_map>
#include <vector>

namespace circ
{
    using value_t = llvm::Value *;
    using values_t = std::vector< llvm::Value * >;

    using type_t = llvm::Type *;

    using builder_t = llvm::IRBuilder<>;

    using reg_ptr_t = Ctx::reg_ptr_t;

    struct wraps_remill_value
    {
      protected:
        value_t storage = nullptr;

      public:

        wraps_remill_value() = delete;
        wraps_remill_value( value_t storage ) : storage( storage ) {}

        wraps_remill_value( llvm::BasicBlock *where, type_t t )
            : storage( builder_t( where ).CreateAlloca( t ) )
        {}

        wraps_remill_value( llvm::Function *fn, type_t t );

        value_t operator->() const { return storage; }
        value_t operator->() { return storage; }

        value_t operator*() const { return storage; }
        value_t operator*() { return storage; }
    };

    struct State : wraps_remill_value
    {
        using reg_ptr_t = const remill::Register *;

        using wraps_remill_value::wraps_remill_value;

        void store(builder_t &ir, const reg_ptr_t where, value_t what);
        value_t load(builder_t &ir, const reg_ptr_t where);

        // This is a very random function, but sadly the semantic do store this
        // into a state instead of properly using it as a value, so we do not
        // have much choice.
        value_t load_interrupt_vector( builder_t &irb );

        void reset( builder_t &irb, const Ctx::regs_t &regs );
        void commit( builder_t &irb, CtxRef ctx );
    };

    struct MemoryPtr : wraps_remill_value
    {
        MemoryPtr( type_t t )
            : wraps_remill_value( llvm::UndefValue::get( t ) )
        {}
    };

    struct Trace
    {
        CtxRef ctx;
        std::vector< State > storage;

        Trace( CtxRef ctx, State input, State output )
            : ctx( ctx ),
              storage{ input, output }
        {}

        void reset( builder_t &bld, const Ctx::regs_t &regs )
        {
            for ( auto &state : storage )
                state.reset( bld, regs );
        }

        void commit( builder_t &bld )
        {
            for ( auto &state : storage )
                state.commit( bld, ctx );
        }

        auto &input() { return storage[ 0 ]; }
        auto &output() { return storage[ 1 ]; }
    };

}  // namespace circ
