/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>
#include <circuitous/Util/Warnings.hpp>

#include <remill/BC/Compat/CallSite.h>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Verifier.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
#include <llvm/Transforms/Utils/Cloning.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <circuitous/Lifter/Components/Decoder.hpp>

#include <sstream>

namespace circ::build
{
    std::string Decoder::generate_raw_bytes(const std::string &full,
                                            uint64_t from, uint64_t to)
    {
        auto n = full.substr(from, to - from);
        std::reverse(n.begin(), n.end());
        return n;
    }

    llvm::Value *Decoder::create_bit_check( const shadow_t &shadow,
                                            uint64_t from, uint64_t to )
    {
        auto encoding = convert_encoding( shadow.enc );
        std::string expected = generate_raw_bytes(encoding, from, to);

        auto size = static_cast< uint32_t >(expected.size());
        check(size == to - from) << size << " != " << to - from
                                 << ":" << to << "-" << from;

        auto expected_v = ir.getInt(llvm::APInt(size, expected, 2));
        auto extracted = irops::make_leaf< irops::ExtractRaw >(ir,
                static_cast< std::size_t >(from),
                static_cast< std::size_t >(to - from));
        return irops::make< irops::DecodeCondition >(ir, {expected_v, extracted}, size);
    }

    auto Decoder::byte_fragments( const shadow_t &shadow ) -> values_t
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
        auto tail = ir.getInt( llvm::APInt( static_cast< std::uint32_t >( tail_size ),
                                            0, false) );
        auto coerced_size = static_cast< std::size_t >( shadow.enc_bitsize );

        auto extracted = irops::make_leaf< irops::ExtractRaw >( ir, coerced_size, tail_size );
        auto compare = irops::make< irops::DecodeCondition >( ir,
                                                              { tail, extracted },
                                                              tail_size );
        out.push_back( compare );
        return out;
    }

    auto Decoder::hardcoded_checks() -> values_t
    {
        std::vector< llvm::Value * > fragment_instances;
        // Collect all fragment conditions.
        for ( const auto &shadow : shadows )
        {
            auto all_fragments = byte_fragments( shadow );
            fragment_instances.emplace_back( irops::make< irops::And >( ir, all_fragments ) );
        }
        return fragment_instances;
    }

    auto Decoder::get_decoder_tree()
    -> std::tuple< llvm::Value *, std::vector< llvm::Value * > >
    {
        std::vector< llvm::Value * > fragment_instances;
        // Collect all fragment conditions.
        for ( const auto &shadow : shadows )
        {
            auto all_fragments = byte_fragments( shadow );
            fragment_instances.emplace_back( irops::make< irops::And >( ir, all_fragments ) );
        }

        auto [ dec, ties ] = tie_selectors( fragment_instances );
        fragment_instances.insert( fragment_instances.end(), dec.begin(), dec.end() );

        return { irops::make< irops::DecoderResult >( ir, fragment_instances ), ties };
    }

    llvm::Value *Decoder::emit_translation_tree( const shadowinst::Reg &sreg,
                                                 llvm::Value *selector )
    {
        if ( sreg.regions.marked_size() == 0 )
            return ir.getTrue();

        if (sreg.tm().is_saturated())
            return ir.getTrue();

        values_t conds;
        for ( auto &val : sreg.tm().complement() )
            conds.push_back( ir.CreateICmpEQ( selector, ir.getInt( val ) ) );
        return ir.CreateNot( irops::make< irops::Or >( ir, conds ) );
    }

    auto Decoder::tie_selectors( const values_t &conds ) -> std::tuple< values_t, values_t >
    {
        check( conds.size() == shadows.size() );

        if ( shadows.empty() )
            return {};

        using s_reg_t = const shadowinst::Reg;
        auto get_reg = [ & ]( const auto &from ) -> s_reg_t *
        {
            return from.reg();
        };

        auto get_base = [ & ]( const auto &from ) -> s_reg_t *
        {
            if ( !from.address() || !from.address()->base_reg() )
                return nullptr;
            return from.address()->base_reg();
        };

        auto get_index = [ & ]( const auto &from ) -> s_reg_t *
        {
            if ( !from.address() || !from.address()->index_reg() )
                return nullptr;
            return from.address()->index_reg();
        };

        auto mk_selector = [ & ]( auto sreg )
        {
            auto materializer = shadowinst::Materializer( ir, *sreg );
            return materializer.region_selector();
        };

        std::vector< llvm::Value * > decoder_fragments;
        std::vector< llvm::Value * > ties;

        using bucket_t = std::tuple< llvm::Type *, std::size_t, llvm::Value * >;
        std::vector< bucket_t > buckets;

        auto handle_bucket = [ & ]( llvm::Type *t, std::size_t selector )
            -> bucket_t &
        {
            for ( auto &entry : buckets )
            {
                auto &[ a, b, _ ] = entry;
                if ( a == t && b == selector )
                    return entry;
            }

            auto val = llvm::ConstantInt::get( t, 0, false );
            buckets.emplace_back( t, selector, val );
            return buckets.back();
        };

        auto operands_size = shadows[ 0 ].operands.size();
        auto exec = [ & ]( auto get )
        {
            for ( std::size_t i = 0; i < operands_size; ++i )
            {
                for ( std::size_t j = 0; j < shadows.size(); ++j )
                {
                    auto sreg = get( shadows[ j ][ i ] );
                    if ( !sreg || sreg->regions.marked_size() == 0 )
                       continue;

                    auto materializer = shadowinst::Materializer( ir, *sreg );
                    auto type = materializer.selector_type();

                    auto &[ t, s, val ] = handle_bucket( type, *sreg->selector );
                    val = ir.CreateSelect( conds[ j ], mk_selector( sreg ), val );
                    decoder_fragments.push_back( emit_translation_tree( *sreg, val ) );
                }
            }
        };

        exec( get_reg );
        exec( get_base );
        exec( get_index );
        exec( get_segment );

        for ( auto [ t, s, v ] : buckets )
        {
            auto opaque = irops::make_leaf< irops::RegSelector >( ir, t, s );
            auto ac = irops::make< irops::AdviceConstraint >( ir, { v, opaque } );
            ties.push_back( ac );
        }

        return { decoder_fragments, ties };
    }

}  // namespace circ::build
