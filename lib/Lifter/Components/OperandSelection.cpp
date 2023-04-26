/*
 * Copyright (c) 2023 Trail of Bits, Inc.
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

#include <circuitous/Lifter/Components/OperandSelection.hpp>
#include <circuitous/Lifter/SReg.hpp>

#include <circuitous/Util/Overloads.hpp>

namespace circ::build
{
    llvm::Type *zero_reg_type( llvm::IRBuilder<> &irb, llvm::StringRef name )
    {
        auto [ _, suffix ] = name.rsplit( 'i' );
        unsigned as_int = 0;
        if ( suffix.getAsInteger( 10u, as_int ) )
        {
            log_kill() << "Invalid zero reg" << name.str();
        }

        return irb.getIntNTy( as_int );
    }

    void OperandSelection::Builder::add_sat( const shadowinst::TM_t &tm )
    {
        for ( const auto &item : saturated )
            if ( tm.is_subset_of( item ) )
                return;
        saturated.push_back( tm );
    }

    void OperandSelection::Builder::add_unsat( const shadowinst::TM_t &tm )
    {
        for ( const auto &item : saturated )
            if ( tm.is_subset_of( item ) )
                return;
        saturated.push_back( tm );
    }

    llvm::Value *OperandSelection::make_select( llvm::IRBuilder<> &irb,
                                                const shadowinst::TM_t &tm,
                                                Ctx &ctx,
                                                llvm::Value *selector,
                                                bool is_read )
    {
        auto trg_type = [ & ]() -> llvm::Type *
        {
            llvm::Type *out = nullptr;
            for ( const auto &[ trg, _ ] : tm )
            {
                auto trg_type = [ & ]( auto what )
                {
                    auto reg = ctx.reg( what );
                    if ( reg )
                        return reg->type;

                    check( llvm::StringRef( what ).startswith( "__remill_zero" ) );
                    return zero_reg_type( irb, what );
                }( trg );

                if ( !out )
                    out = trg_type;
                check( out == trg_type ) << "Type missmatch when constructing select";
            }
            check( out ) << "Was not able to derive type of select!";
            return out;
        }();

        auto default_zero = llvm::ConstantInt::get( trg_type, 0 );
        std::vector< llvm::Value * > args( ( 1 << tm.bitsize ) + 1, default_zero );
        args[ 0 ] = selector;

        for ( const auto &[ str, reg ] : tm.reverse_bitmap() )
        {
            auto idx = llvm::APInt( static_cast< uint32_t >( tm.bitsize ), str, 2 )
                .getLimitedValue();

            check( args.size() > idx + 1 );
            args[ idx + 1 ] = irops::mk_reg(
                    irb, ctx.reg( reg ),
                    ( is_read ) ? irops::io_type::in : irops::io_type::out );
            check( args[ idx + 1 ]->getType() == trg_type );
        }

        return irops::make< irops::Select >( irb, args );
    }

    llvm::Value *OperandSelection::materialize( llvm::IRBuilder<> &irb,
                                                std::size_t idx, bool is_read )
    {
        auto pristine = ( *this )[ idx ];
        auto selector = irops::make_leaf< irops::OpSelector >( irb, pristine.bitsize,
                                                               next_id++ );
        get( is_read )[ idx ].emplace_back( make_select( irb, pristine, ctx,
                                                         selector, is_read ) );
        return get( is_read )[ idx ].back();
    }

    auto OperandSelection::request( llvm::IRBuilder<> &irb,
                                    std::size_t idx,
                                    std::size_t nth, bool is_read )
        -> llvm::Value *
    {
        auto &already_present = get( is_read )[ idx ];
        while ( already_present.size() <= nth )
        {
            const auto &tm = ( *this )[ idx ];
            auto selector = irops::make_leaf< irops::OpSelector >( irb, tm.bitsize, next_id++ );
            already_present.emplace_back( make_select( irb, tm, ctx, selector, is_read ) );
        }

        return already_present[ nth ];
    }

    llvm::Value *OperandSelection::Requester::request( llvm::IRBuilder<> &irb, std::size_t idx )
    {
        if ( processed.count( idx ) == 0 )
            processed[ idx ] = 0;

        return storage.request( irb, idx, processed[ idx ]++, is_read );
    }

    llvm::Value *OperandSelection::Requester::request( llvm::IRBuilder<> &irb,
                                                       const shadowinst::TM_t &tm )
    {
        return request( irb, storage.match( tm ) );
    }

    std::string OperandSelection::to_string() const
    {
        std::stringstream ss;
        ss << "Operand selection uses: " << saturated.size() << "\n";
        for ( const auto &tm : saturated )
            ss << "Item:\n" << tm.to_string( 1u );
        return ss.str();
    }


    OperandSelection OperandSelection::build( CtxRef ctx, const InstructionBatch &batch )
    {
        OperandSelection::Builder bld;

        auto process_saturated = overloaded
        {
            [ & ]( const shadowinst::Reg &reg )
            {
                if ( reg.translation_map.is_saturated() )
                    bld.add_sat( reg.translation_map );
            },
            [ & ]( const auto & ) {} // ignore rest
        };

        auto process_unsaturated = overloaded
        {
            [ & ]( const shadowinst::Reg &reg )
            {
                if ( !reg.translation_map.is_saturated() )
                    bld.add_unsat( reg.translation_map );
            },
            [ & ]( const auto & ) {} // ignore rest
        };

        for ( const auto &info : batch.get() )
            info.shadow().for_each_present( process_saturated );
        for ( const auto &info : batch.get() )
            info.shadow().for_each_present( process_unsaturated );
        return { ctx, std::move( bld.saturated ) };
    }

    auto OperandSelection::assign( llvm::IRBuilder<> &irb, const InstructionInfo &info )
        -> values_t
    {
        check( info._rinst );
        log_kill() << "Not implemented.";
    }

}  // namespace circ::build
