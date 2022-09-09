/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/Transforms/PassBase.hpp>

#include <vector>

namespace circ
{
    Circuit::circuit_ptr_t merge_with_advice( Circuit::circuit_ptr_t &&circuit,
                                              const std::vector< Operation::kind_t > &kinds );
    struct MergeWithAdvicesPass : PassBase
    {
        using kinds_t = std::vector< Operation::kind_t >;

        static Pass get() { return std::make_shared< MergeWithAdvicesPass >(); }

        kinds_t kinds;

        CircuitPtr run( CircuitPtr &&circuit ) override
        {
            return merge_with_advice( std::move( circuit ), kinds );
        }

    };

} // namespace circ
