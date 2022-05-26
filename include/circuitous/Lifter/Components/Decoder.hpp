/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Lifter/Component.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/IntrinsicTable.h>

#include <string>
#include <unordered_map>
#include <vector>

namespace circ::build
{
    struct Decoder : has_ctx_ref
    {
        using has_ctx_ref::has_ctx_ref;
        using values_t = std::vector< llvm::Value * >;

        llvm::IRBuilder<> &ir;
        ISEL_view isel;
        std::vector< llvm::Value *> to_verify;

        Decoder(CtxRef ctx_, llvm::IRBuilder<> &ir_, ISEL_view &isel_)
            : has_ctx_ref(ctx_), ir(ir_), isel(isel_)
        {}


        auto get_decoder_tree() -> std::tuple< llvm::Value *, std::vector< llvm::Value * > >;

      private:

        values_t byte_fragments();
        std::string generate_raw_bytes(const std::string &str, uint64_t form, uint64_t to);
        llvm::Value *create_bit_check(uint64_t from, uint64_t to);

        auto rinst_size() { return isel.instruction.bytes.size() * 8; }

        std::string convert_encoding(const auto &encoding)
        {
            std::string full_inst;
            // Encoding check needed since `x` is unsigned.
            for (std::size_t i = 0; i < encoding.size(); ++i)
                full_inst += (encoding[i]) ? '1' : '0';

            return full_inst;
        }

        values_t emit_translation_trees();
        llvm::Value *emit_translation_tree(const shadowinst::Reg &sreg);
    };

}  // namespace circ::build
