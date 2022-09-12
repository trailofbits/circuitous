/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/Util/Warnings.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <circuitous/IR/Circuit.hpp>

#include <circuitous/Lifter/BaseLifter.hpp>
#include <circuitous/Lifter/CircuitBuilder.hpp>
#include <circuitous/Lifter/CircuitSmithy.hpp>
#include <circuitous/Lifter/Lifter.hpp>
#include <circuitous/Lifter/LLVMToCircIR.hpp>

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
            check(maybe_inst) << "Decoder failed on:" << x.as_hex_str();
            rinsts.push_back(std::move(*maybe_inst));
        }
        return smelt(std::move(rinsts));
    }

    auto CircuitSmithy::smelt(std::string_view raw_bytes) -> self_t &
    {
        return smelt(Decoder(ctx).decode_all(raw_bytes));
    }

    auto CircuitSmithy::smelt(std::vector< remill::Instruction > &&rinsts) -> self_t &
    {
        batch.add(std::move(rinsts));
        return *this;
    }

    auto CircuitSmithy::forge() -> circuit_ptr_t
    {
        check(!batch->empty()) << "No valid instructions provided, cannot produce circuit.";
        batch.fuzz();
        batch.lift< ILifter< OpaqueILifter > >();
        return lower_fn(CircuitMaker(ctx).make_from(std::move(batch)),
                        ctx.ptr_size);
    }
} // namespace circ
