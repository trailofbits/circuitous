/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Lifter/Instruction.hpp>
#include <circuitous/Util/InstructionBytes.hpp>

#include <string>
#include <vector>

namespace circ
{
    struct Circuit;

    struct CircuitSmithy
    {
        using self_t = CircuitSmithy;
        using circuit_ptr_t = std::unique_ptr< Circuit >;

        using batch_t = InstructionBatch;
        // This class owns the lifting context.
        Ctx ctx;

      private:
        batch_t batch;
        circuit_ptr_t circuit;


      public:
        CircuitSmithy(const std::string &arch_name, const std::string &os_name);
        // Take ownership of already existing context.
        // TODO(lukas): It cannot be retrieved back, should it?
        CircuitSmithy(Ctx ctx_);

        self_t &smelt(const std::vector< InstBytes > &insts);
        self_t &smelt(std::string_view raw_bytes);
        self_t &smelt(std::vector< remill::Instruction > &&rinsts);

        // Returns circuit created from all data provided by `smelt`. Will return owning
        // pointer to circuit and resets internal state of `this`.
        circuit_ptr_t forge();
    };
}  // namespace circ
