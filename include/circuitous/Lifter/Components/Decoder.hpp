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
    struct DecoderBase
    {
        using shadow_t = typename InstructionInfo::shadow_t;
        using shadows_t = std::vector< shadow_t >;

        using values_t = std::vector< llvm::Value * >;
        using enc_t = typename InstructionInfo::enc_t;

        llvm::IRBuilder<> &irb;

        DecoderBase( llvm::IRBuilder<> &irb ) : irb( irb ) {}

        static std::string generate_raw_bytes( const std::string &str,
                                               uint64_t from, uint64_t to );

        static std::string convert_encoding( const auto &encoding )
        {
            std::string full_inst;
            // Encoding check needed since `x` is unsigned.
            for ( std::size_t i = 0; i < encoding.size(); ++i )
                full_inst += ( encoding[ i ] ) ? '1' : '0';

            return full_inst;
        }


        llvm::Value *create_bit_check( const shadow_t &shadow,
                                       uint64_t from, uint64_t to);

        values_t byte_fragments( const shadow_t &shadow );

        llvm::Value *emit_translation_tree( const shadowinst::Reg &sreg,
                                            llvm::Value *selector );

        llvm::Value *holes( const shadow_t &shadow );
        llvm::Value *holes( const shadowinst::Reg &sreg );

        llvm::Value *hardcoded_check( const shadow_t &shadow );

    };

    struct Decoder : protected DecoderBase
    {
        using base_t = DecoderBase;

        const shadows_t &shadows;

        std::vector< llvm::Value * > to_verify;

        Decoder( llvm::IRBuilder<> &irb, ISEL_view &isel )
            : base_t( irb ),
              shadows( isel.shadows )
        {}

        Decoder( llvm::IRBuilder<> &irb, const InstructionInfo &info )
            : base_t( irb ),
              shadows( info.shadows )
        {}

       Decoder( llvm::IRBuilder<> &irb, const shadows_t &shadows )
            : base_t( irb ),
              shadows( shadows )
        {}

        auto get_decoder_tree() -> std::tuple< llvm::Value *, std::vector< llvm::Value * > >;
        auto hardcoded_checks() -> values_t;
      private:

        llvm::Value *all_holes();
    };

    struct AtomDecoder : protected DecoderBase
    {
        using base_t = DecoderBase;
        using atom_t = Atom;

        const atom_t &atom;

        AtomDecoder( llvm::IRBuilder<> &irb, const atom_t &atom )
            : base_t( irb ), atom( atom )
        {}

        llvm::Value *get_decoder_tree();
    };

}  // namespace circ::build
