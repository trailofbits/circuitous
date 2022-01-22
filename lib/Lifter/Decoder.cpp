/*
 * Copyright (c) 2021 - 2022 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/Decoder.hpp>

#include <circuitous/Lifter/Context.hpp>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Util/Logging.hpp>

namespace circ
{
    std::string isel_name(const remill::Instruction &rinst)
    {
        check(15 >= rinst.bytes.size());
        return rinst.function + ("123456789abcdef"[rinst.bytes.size()]);
    }

    // TODO(lukas): Is this still needed?
    bool IsDecodePositionIndependent(const remill::Arch::ArchPtr &arch,
                                     const remill::Instruction &inst)
    {
        remill::Instruction copy;
        if (!arch->DecodeInstruction(inst.pc + 32, inst.bytes, copy) ||
            inst.operands.size() != copy.operands.size())
        {
            return false;
        }

        for (auto i = 0u; i < inst.operands.size(); ++i)
            if (inst.operands[i].Serialize() != copy.operands[i].Serialize())
                return false;

        return true;
    }

    // Try to decode bytes in buff using arch from `ctx_ref`.
    auto Decoder::decode(std::string_view buff) -> std::tuple< maybe_inst_t, std::string_view >
    {
        // Nothing to do anymore.
        if (buff.empty())
            return std::make_tuple( std::nullopt, buff );

        remill::Instruction inst;
        if (!ctx.arch()->DecodeInstruction(0, buff, inst) || !inst.IsValid())
            // Failed, move one byte.
            return decode(buff.substr(1));
        // Success, return inst with shorter buffer.
        return std::make_tuple( std::make_optional(std::move(inst)),
                                buff.substr(inst.bytes.size()) );
    }

    // Recursively try to decode everything present, call `process` for each decoded inst.
    void Decoder::decode_all_(std::string_view buff)
    {
        while (!buff.empty())
        {
            auto [inst, rest] = decode(buff);
            if (inst)
                process(std::move(*inst));
            buff = rest;
        }
    }

    auto Decoder::decode_all(llvm::StringRef buff) -> self_t &
    {
        std::string_view coerced(buff.data(), buff.size());
        decode_all_(coerced);
        return *this;
    }

    void Decoder::process(remill::Instruction inst)
    {
        if (!IsDecodePositionIndependent(ctx._arch, inst))
        {
            log_error() << "Skipping position-dependent instruction: " << inst.Serialize();
            return;
        }

        // Make sure the size of inst is not bigger than our assumption.
        CHECK(inst.bytes.size() * 8u <= kMaxNumInstBits);

        // Group the unique decoded instructions in terms of their ISELs, i.e. the
        // general semantic category of those instructions.
        if (auto [_, inserted] = inst_bytes.insert(inst.bytes); inserted)
        {
            auto &iclass = [&]() -> InstructionSelection &
            {
                // Is it present already?
                if (auto [it, inserted] =
                        isel_index.emplace(isel_name(inst), grouped_insts.size());
                    !inserted)
                {
                    auto &iclass = grouped_insts[it->second];
                    CHECK(inst.bytes.size() == iclass.instructions.back().bytes.size());
                    return iclass;
                }
                // We need to create it -- `isel_index` was already update in the `if`,
                return grouped_insts.emplace_back();
            }();

            iclass.PartialAdd(std::move(inst),
                              InstructionSelection::RemillBytesToEncoding(inst.bytes));
        }
    }

} // namespace circ
