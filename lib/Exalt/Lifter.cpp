/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#include <circuitous/Exalt/Lifter.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Check.hpp>

CIRCUITOUS_RELAX_WARNINGS
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ
{
    void unit_lifter::exalt( unit_t &unit )
    {
        b_ctx.exalt_prologue( unit );
        std::ignore = unit;
    }

    void circuit_producer::exalt( unit_t &unit )
    {
        std::ignore = unit;
    }

    void circuit_producer::finalize()
    {

    }

}  // namespace circ
