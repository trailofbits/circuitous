/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>

namespace circ
{
    using CircuitPtr = std::unique_ptr< Circuit >;

    struct PassBase
    {
        virtual ~PassBase() = default;

        virtual CircuitPtr run(CircuitPtr &&) = 0;
    };

    using Pass = std::shared_ptr< PassBase >;

    using NamedPass = std::pair< std::string, Pass >;

} // namespace circ
