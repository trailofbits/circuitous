/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Exalt/States.hpp>
#include <circuitous/Exalt/Value.hpp>

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/IntrinsicTable.h>
#include <remill/BC/Lifter.h>
#include <remill/BC/Optimizer.h>
#include <remill/BC/Util.h>

#include <iomanip>
#include <unordered_map>
#include <vector>

namespace circ::exalt
{
    // Forward declare
    struct InstructionBatch;

    namespace isem
    {
        struct ISem;
    }

    // Can be just passed around by value.
    struct circuit_function
    {
        llvm::Function &fn;
      protected:
         circuit_function( llvm::Function &fn ) : fn( fn ) {}
      public:

        auto &operator*() const { return fn; }
        auto operator->() const { return &fn; }

        auto get() const { return &fn; }

        static circuit_function make( CtxRef ctx_ref );

        llvm::BasicBlock *bb() const { return &*fn.begin(); }
    };

    /* Owns everything related to function manipulation. */
    struct function_context
    {
        circuit_function fn;
        // We never want to copy this. We want to be using only one
        // to avoid problems with insertion points and bitcode being
        // invalidated.
        builder_t _irb;

      protected:

        function_context( circuit_function fn )
            : fn( fn ), _irb( fn.bb() )
        {}

      public:

        static function_context make( CtxRef ctx_ref );

        auto &irb() { return _irb; }
    };

    struct builder_context;

    // TODO( exalt ): I would like this to be simply a vector of things implementing a
    //                virtual interface eventually. Since I don't yet know what
    //                that interface should look like, I am unfolding things.
    //                Implement this in the similar way to unit_components, bonus points
    //                for making it the same (use persistent option).
    struct submodules
    {
        // TODO( exalt ): Figure out if this is the correct coupling.
        //                Another option is to pass in a pointer to the `builder_context`.
        function_context &fn_ctx;

        /* Required to lift semantics. */
        // What about old `Trace` class?
        RemillArchState arch_state;
        MemoryPtr mem_ptr;

        /* Additional submodules to handle ZK related functionality. */
        // TODO( syscalls ): Add syscalls.

        // TODO( exalt ): Right now, this has 2 phases:
        //                * Construct the data members (ctor/make).
        //                * Initialise them -> construct llvm structures needed for them to
        //                  operate correctly.
        //                Investigate if we can have it in ctors only. I am not sure because
        //                I suspect later on there can be dependencies between these
        //                submodules on the object level.
        submodules( CtxRef ctx_ref, function_context &fn_ctx )
            : fn_ctx( fn_ctx ),
              arch_state( fn_ctx.irb(), ctx_ref ),
              mem_ptr( ctx_ref.memory_ptr_type() )
        {}

        // TODO( exalt ): Needed?
        void init( builder_context &bld_ctx );

        auto exalt_prologue( unit_t &unit ) -> exalted_value_buckets;
    };


    struct builder_context : has_ctx_ref
    {
        function_context fn_ctx;
        submodules sub_mods;

        builder_context( CtxRef ctx_ref ) : has_ctx_ref( ctx_ref ),
                                            fn_ctx( function_context::make( ctx_ref ) ),
                                            sub_mods( ctx_ref, fn_ctx )
        {
            sub_mods.init( *this );
        }

        auto &irb() { return fn_ctx.irb(); }
        auto &arch_state() { return sub_mods.arch_state; }
        auto &mem_ptr() { return sub_mods.mem_ptr; }

        llvm::Function *take_fn() &&
        {
            return &*( fn_ctx.fn );
        }

        auto exalt_prologue( unit_t &unit ) { return sub_mods.exalt_prologue( unit ); }
    };

}  // namespace circ::exalt
