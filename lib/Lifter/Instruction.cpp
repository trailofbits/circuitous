/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/Instruction.hpp>

#include <circuitous/Util/InstructionBytes.hpp>

namespace circ
{

    InstructionBatch::InstructionBatch(Ctx &ctx) : parent_t(ctx) {}

    InstructionBatch::InstructionBatch(
            Ctx &ctx, const std::vector< remill::Instruction > &rinsts)
        : parent_t(ctx)
    {

    }

    InstructionBatch::InstructionBatch(
            Ctx &ctx, const std::vector< InstBytes > &instbytes)
        : parent_t(ctx)
    {

    }

    InstructionBatch::InstructionBatch(
            Ctx &ctx, const std::string &raw_bytes)
        : parent_t(ctx)
    {

    }

    auto InstructionBatch::add(InstructionBatch &&other) -> self_t &
    {
        insts.insert(insts.end(),
                     std::make_move_iterator(other->begin()),
                     std::make_move_iterator(other->end()));
        return *this;
    }

    auto InstructionBatch::add(InstructionInfo &&info) -> self_t &
    {
        insts.push_back(std::move(info));
        return *this;
    }

    auto InstructionBatch::add(raw_insts_t &&rinsts) -> self_t &
    {
        for (auto &&rinst : rinsts)
            insts.emplace_back(std::move(rinst));
        return *this;
    }

    auto InstructionBatch::fuzz() -> self_t &
    {
        for (auto &info : insts)
            if (!info.has_shadow())
                info.make_fuzz(this->ctx);
        return *this;
    }

} // namespace circ
