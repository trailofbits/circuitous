/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>
#include <circuitous/Util/Warnings.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Verifier.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
#include <llvm/Transforms/Utils/Cloning.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <circuitous/Lifter/Components/Decoder.hpp>
#include <circuitous/Util/Overloads.hpp>

#include <sstream>

namespace circ::build
{
    /* DecoderBase:: */

    std::string DecoderBase::generate_raw_bytes( const std::string &full,
                                                 uint64_t from, uint64_t to )
    {
        auto n = full.substr(from, to - from);
        std::reverse(n.begin(), n.end());
        return n;
    }

    llvm::Value *DecoderBase::create_bit_check( const shadow_t &shadow,
                                                uint64_t from, uint64_t to )
    {
        auto encoding = convert_encoding( shadow.enc );
        std::string expected = generate_raw_bytes(encoding, from, to);

        auto size = static_cast< uint32_t >(expected.size());
        check(size == to - from) << size << " != " << to - from
                                 << ":" << to << "-" << from;

        auto expected_v = irb.getInt(llvm::APInt(size, expected, 2));
        auto extracted = irops::make_leaf< irops::ExtractRaw >(irb,
                static_cast< std::size_t >(from),
                static_cast< std::size_t >(to - from));
        return irops::make< irops::DecodeCondition >(irb, {expected_v, extracted}, size);
    }

    auto DecoderBase::byte_fragments( const shadow_t &shadow ) -> values_t
    {
        values_t out;

        auto unknown_regions = shadow.UnknownRegions( shadow.enc_bitsize );
        // `unknown_regions` are in `[ from, size ]` format.
        for ( auto [ from, to ] : shadowinst::FromToFormat( unknown_regions ) )
            out.push_back( create_bit_check( shadow, from, to ) );

        // TODO(lukas): Now we need to check the tail.
        //              Try to lift `6689d8` and `89d8` to demonstrate the issue.
        // TODO(lukas): For now we assume it is padded with 0s.
        auto tail_size = static_cast< std::size_t >( kMaxNumInstBits - shadow.enc_bitsize );
        auto tail = irb.getInt( llvm::APInt( static_cast< std::uint32_t >( tail_size ),
                                            0, false) );
        auto coerced_size = static_cast< std::size_t >( shadow.enc_bitsize );

        auto extracted = irops::make_leaf< irops::ExtractRaw >( irb, coerced_size, tail_size );
        auto compare = irops::make< irops::DecodeCondition >( irb,
                                                              { tail, extracted },
                                                              tail_size );
        out.push_back( compare );
        return out;
    }

    llvm::Value *DecoderBase::hardcoded_check( const shadow_t &shadow )
    {
        auto all_fragments = byte_fragments( shadow );
        return irops::make< irops::And >( irb, all_fragments );
    }

    auto Decoder::hardcoded_checks() -> values_t
    {
        std::vector< llvm::Value * > fragment_instances;
        // Collect all fragment conditions.
        for ( const auto &shadow : shadows )
            fragment_instances.emplace_back( hardcoded_check( shadow ) );
        return fragment_instances;
    }


    llvm::Value *DecoderBase::holes( const shadow_t &shadow )
    {
        std::vector< llvm::Value * > holes_per_tm;

        auto process = overloaded
        {
            [ & ]( const shadowinst::Reg &sreg )
            {
                if ( sreg.regions.marked_size() != 0 && !sreg.tm().is_saturated() )
                    holes_per_tm.emplace_back( holes( sreg ) );
            },
            [ & ]( const auto & ) {} // ignore rest
        };
        shadow.for_each_present( process );

        if ( holes_per_tm.empty() )
            return nullptr;
        return irops::make< irops::And >( irb, holes_per_tm );
    }

    llvm::Value *DecoderBase::holes( const shadowinst::Reg &sreg )
    {
        auto materializer = shadowinst::Materializer( irb, sreg );
        auto selector = materializer.region_selector();
        return emit_translation_tree( sreg, selector );
    }

    llvm::Value *DecoderBase::emit_translation_tree( const shadowinst::Reg &sreg,
                                                     llvm::Value *selector )
    {
        if ( sreg.regions.marked_size() == 0 )
            return irb.getTrue();

        if (sreg.tm().is_saturated())
            return irb.getTrue();

        values_t conds;
        for ( auto &val : sreg.tm().complement() )
            conds.push_back( irb.CreateICmpEQ( selector, irb.getInt( val ) ) );
        return irb.CreateNot( irops::make< irops::Or >( irb, conds ) );
    }

    /* AtomDecoder:: */

    llvm::Value *AtomDecoder::get_decoder_tree()
    {
        auto fragments = byte_fragments( atom.abstract );
        if ( auto hole_check = holes( atom.abstract ) )
            fragments.push_back( hole_check );
        return irops::make< irops::DecoderResult >( irb, fragments );
    }

    /* Decoder:: */

    auto Decoder::get_decoder_tree()
    -> std::tuple< llvm::Value *, std::vector< llvm::Value * > >
    {
        std::vector< llvm::Value * > fragment_instances;
        // Collect all fragment conditions.
        for ( const auto &shadow : shadows )
        {
            auto all_fragments = byte_fragments( shadow );
            fragment_instances.emplace_back( irops::make< irops::And >( irb, all_fragments ) );
        }

        fragment_instances.emplace_back( all_holes() );
        return { irops::make< irops::DecoderResult >( irb, fragment_instances ), values_t{} };

    }

    llvm::Value *Decoder::all_holes()
    {
        std::vector< llvm::Value * > holes_per_tm;

        for ( auto &shadow : shadows )
            holes_per_tm.emplace_back( holes( shadow ) );

        return irops::make< irops::And >( irb, holes_per_tm );
    }

}  // namespace circ::build
