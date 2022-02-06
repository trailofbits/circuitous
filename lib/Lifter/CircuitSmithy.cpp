/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/BaseLifter.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <remill/BC/Compat/CallSite.h>
#include <remill/BC/Util.h>
#include <remill/BC/Optimizer.h>

#include <circuitous/Lifter/Flatten.hpp>
#include <circuitous/Fuzz/InstructionFuzzer.hpp>

#include <circuitous/Util/Logging.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Lifter.hpp>

#include <circuitous/Lifter/CircuitSmithy.hpp>

namespace circ
{

    CircuitSmithy::CircuitSmithy(const std::string &arch_name, const std::string &os_name)
        : ctx(arch_name, os_name), batch(ctx)
    {}

    CircuitSmithy::CircuitSmithy(Ctx ctx_) : ctx(std::move(ctx_)), batch(ctx) {}

    auto CircuitSmithy::smelt(const std::vector< InstBytes > &insts) -> self_t &
    {
        auto decoder = Decoder(ctx);
        std::vector< remill::Instruction > rinsts;
        for (const auto &x : insts)
        {
            auto maybe_inst = decoder.decode(x);
            check(maybe_inst);
            rinsts.push_back(std::move(*maybe_inst));
        }
        return smelt(std::move(rinsts));
    }

    auto CircuitSmithy::smelt(const std::string &raw_bytes) -> self_t &
    {
        return smelt(Decoder(ctx).decode_all(raw_bytes));
    }

    auto CircuitSmithy::smelt(std::vector< remill::Instruction > &&rinsts) -> self_t &
    {
        batch.add(std::move(rinsts));
        return *this;
    }

    auto CircuitSmithy::forge() -> self_t &
    {
        batch.fuzz();
        batch.lift< InstructionLifter >();
        return *this;
    }
} // namespace circ
