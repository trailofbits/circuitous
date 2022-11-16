/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/Transforms/PassBase.hpp>

#include <vector>

namespace circ
{
    circuit_owner_t conjure_alu( circuit_owner_t &&circuit,
                                 const std::vector< Operation::kind_t > &kinds );

    struct ConjureALUPass : PassBase
    {
        using kinds_t = std::vector< Operation::kind_t >;
        kinds_t kinds;

        ConjureALUPass( const kinds_t &kinds ) : kinds( kinds ) {}

        circuit_owner_t run( circuit_owner_t &&circuit ) override
        {
            return conjure_alu( std::move( circuit ), kinds );
        }

    };

} // namespace circ
