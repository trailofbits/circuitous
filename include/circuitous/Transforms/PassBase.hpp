/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>

namespace circ
{
    struct PassBase
    {
        virtual ~PassBase() = default;

        virtual circuit_owner_t run(circuit_owner_t &&) = 0;
    };

    using Pass = std::shared_ptr< PassBase >;

    using NamedPass = std::pair< std::string, Pass >;

} // namespace circ
