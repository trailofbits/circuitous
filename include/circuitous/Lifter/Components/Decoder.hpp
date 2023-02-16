/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Lifter/Component.hpp>
#include <circuitous/Lifter/Instruction.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/IntrinsicTable.h>

#include <string>
#include <unordered_map>
#include <vector>

namespace circ::build
{
    struct Decoder
    {
        using values_t = std::vector< llvm::Value * >;

        llvm::IRBuilder<> &ir;

        using shadow_t = typename InstructionInfo::shadow_t;
        using shadows_t = std::vector< shadow_t >;
        using enc_t = typename InstructionInfo::enc_t;

        using navig_t = shadowinst::navig;

        const shadows_t &shadows;

        std::vector< llvm::Value * > to_verify;

        Decoder( llvm::IRBuilder<> &ir, ISEL_view &isel )
            : ir( ir ),
              shadows( isel.shadows )
        {}

        Decoder( llvm::IRBuilder<> &ir, const InstructionInfo &info )
            : ir( ir ),
              shadows( info.shadows )
        {}

       Decoder( llvm::IRBuilder<> &ir, const shadows_t &shadows )
            : ir( ir ),
              shadows( shadows )
        {}

        auto get_decoder_tree() -> std::tuple< llvm::Value *, std::vector< llvm::Value * > >;
        auto hardcoded_checks() -> values_t;
      private:

        values_t byte_fragments( const shadow_t &shadow );
        std::string generate_raw_bytes(const std::string &str, uint64_t form, uint64_t to);
        llvm::Value *create_bit_check( const shadow_t &shadow,
                                       uint64_t from, uint64_t to);

        std::string convert_encoding(const auto &encoding)
        {
            std::string full_inst;
            // Encoding check needed since `x` is unsigned.
            for (std::size_t i = 0; i < encoding.size(); ++i)
                full_inst += (encoding[i]) ? '1' : '0';

            return full_inst;
        }

        llvm::Value *emit_translation_tree( const shadowinst::Reg &sreg,
                                            llvm::Value *selector );

        std::tuple< values_t, values_t > tie_selectors( const values_t &conds );
    };

}  // namespace circ::build
