/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Lifter/Instruction.hpp>
#include <circuitous/IR/Intrinsics.hpp>

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
    using value_set_t = std::unordered_set< value_t >;

    using type_t = llvm::Type *;

    using builder_t = llvm::IRBuilder<>;

    using reg_ptr_t = Ctx::reg_ptr_t;

    using unit_t = Unit< Atom >;

    /* Components that are useful only on per-unit scope.
     * Therefore we can expect they can be supplied with some for of `builder_context`
     * (or something similarly powerful) on creation.
     */

    enum class place : uint8_t
    {
        // These are *operadns* of given type of place, not the result value
        // itself.
        unspecified = 0,
        root,
        ctx,
        meta,
        computation,
    };

    static inline std::string to_string( place p )
    {
        switch ( p )
        {
            case place::unspecified: return "unspecified";
            case place::root: return "root";
            case place::ctx: return "ctx";
            case place::meta: return "meta";
            case place::computation: return "computation";
        }
    }


    // Idea is that this way components may be able to communicate where they need some
    // values to live in the resulting circuit.
    using exalted_value_t = std::tuple< place, llvm::Value * >;

    using exalted_values_gen_t = gap::generator< exalted_value_t >;
    using exalted_values_t = std::vector< exalted_value_t >;

    using exalted_ctx_t = value_t;

    // `[ begin, end ]` of an inlined semantic.
    // Used as a handle for some components that do post-processing.
    using isem_range_t = std::tuple< llvm::CallInst *, llvm::CallInst * >;

    // TODO( exalt ): We probably want to generalise this.
    using unit_t = Unit< Atom >;

    using semantic_fn_t = llvm::Function *;

}  // namespace circ
