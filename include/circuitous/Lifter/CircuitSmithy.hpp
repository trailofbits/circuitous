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
        self_t &smelt(const std::string &raw_bytes);
        self_t &smelt(std::vector< remill::Instruction > &&rinsts);
        // TODO(lukas): Return circuit_ptr_t here
        self_t &forge();

        circuit_ptr_t take() { return std::move(circuit); }
    };
}  // namespace circ
