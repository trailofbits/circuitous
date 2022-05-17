/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/Decoder.hpp>

#include <circuitous/Lifter/Context.hpp>

#include <circuitous/Support/Check.hpp>

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
    auto Decoder::decode_all_(std::string_view buff) -> std::vector< remill::Instruction >
    {
        std::vector< remill::Instruction > out;
        while (!buff.empty())
        {
            auto [inst, rest] = decode(buff);
            if (inst)
            {
                if (!IsDecodePositionIndependent(ctx._arch, *inst))
                    log_error() << "Skipping position-dependent instruction: "
                                << inst->Serialize();
                else
                    out.push_back(std::move(*inst));
            }
            buff = rest;
        }
        return out;
    }

    auto Decoder::decode_all(llvm::StringRef buff) -> std::vector< remill::Instruction >
    {
        std::string_view coerced(buff.data(), buff.size());
        return decode_all_(coerced);
    }

    auto Decoder::decode(const InstBytes &bytes) -> std::optional< remill::Instruction >
    {
        auto [maybe_inst, tail] = decode(bytes.raw());
        if (!maybe_inst || tail.size() != 0)
            return {};
        return maybe_inst;
    }

} // namespace circ
