/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Exalt/OperandSelection.hpp>

#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Lifter/Instruction.hpp>

#include <circuitous/Lifter/SReg.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/IntrinsicTable.h>

#include <string>
#include <unordered_map>
#include <vector>

namespace circ::exalt
{
    struct OperandLifter : has_ctx_ref
    {
        llvm::IRBuilder<> &irb;
        requester_base &requester;
        bool is_read;

        OperandLifter( CtxRef &ctx,
                       llvm::IRBuilder<> &irb,
                       requester_base &requester,
                       bool is_read )
            : has_ctx_ref( ctx ), irb( irb ),
              requester( requester ),
              is_read( is_read )
        {}

        template< typename S >
        llvm::Value *request( S &&arg )
        {
           return requester.request( std::forward< S >( arg ) );
        }

        template< bool is_signed, typename S >
        llvm::Value *request_word( S &&arg )
        {
            return requester.request( std::forward< S >( arg ) );
        }

        llvm::Value *lift( typename Atom::slice_view view )
        {
            auto process = [ & ]( const auto &raw )
            {
                auto [ c, a ] = raw;
                return lift( c, a );
            };

            return std::visit( process, view.raw );
        }

        llvm::Value *lift( const remill::Operand::Immediate *r_imm,
                           const shadowinst::Immediate *s_imm )
        {
            check( r_imm && s_imm );

            if ( s_imm->regions.marked_size() == 0 )
            {
                auto trg_type = ctx.zero()->getType();
                return llvm::ConstantInt::get( trg_type, r_imm->val, r_imm->is_signed );
            }
            check( s_imm->regions.marked_size() != 0 );
            // This maybe needs to be relaxed?
            check( s_imm->regions.areas.size() == 1 );

            auto [ from, size ] = *( s_imm->regions.areas.begin() );
            return irops::make_leaf< irops::ExtractRaw >( irb, from, size );
        }

        // Dedup
        llvm::Value *lift( const shadowinst::Immediate *s_imm )
        {
            check( s_imm );

            if ( s_imm->regions.marked_size() == 0 )
                return ctx.zero();
            check( s_imm->regions.marked_size() != 0 );
            // This maybe needs to be relaxed?
            check( s_imm->regions.areas.size() == 1 );

            auto [ from, size ] = *( s_imm->regions.areas.begin() );
            return irops::make_leaf< irops::ExtractRaw >( irb, from, size );
        }

        llvm::Value *lift( const remill::Operand::Register *r_reg,
                           const shadowinst::Reg *s_reg )
        {
            check( r_reg && s_reg );
            log_info() << s_reg->to_string();

            if ( s_reg->tm().is_saturated_by_zeroes() )
            {
                return ctx.zero();
            }

            if ( s_reg->tm().empty() || s_reg->tm().max_mats_count() == 0 )
            {
                if ( r_reg->name.empty() )
                    return ctx.zero();

                log_info() << "[operand-lifter]:" << "Hardcoded register op:"
                           << r_reg->name;
                return requester.state.load( irb, ctx.reg( r_reg->name ) );
            }

            auto select = request_word< false >( *s_reg );
            check( select );

            log_info() << "[operand-lifter]:" << "mask_shift_coerce";
            auto x = shadowinst::mask_shift_coerce( select, irb, *s_reg, *ctx.arch() );
            log_info() << "[operand-lifter]:"<< "done";
            return x;
        }

        llvm::Value *lift( const remill::Operand::Address *r_addr,
                           const shadowinst::Address *s_addr )
        {
            check( r_addr && s_addr );
            // We are on purpose ignoring segments here, even though they are reflected in the
            // decoder check that gets emitted eventually.
            log_info() << "[operand-lifter]:" << "addr:base";
            auto base = [ & ]() -> llvm::Value *
            {
                if ( auto s_base = s_addr->base_reg() )
                    return lift( &r_addr->base_reg, s_base );

                if ( r_addr->base_reg.name.empty() )
                    return ctx.zero();

                return make_reg( r_addr->base_reg.name );
            }();

            log_info() << "[operand-lifter]:" << "addr:index";
            auto index = [ & ]() -> llvm::Value *
            {
                if ( auto s_index = s_addr->index_reg() )
                    return lift( &r_addr->index_reg, s_index );

                log_info() << "[operand-lifter]:" << "addr:index:empty";
                if ( r_addr->index_reg.name.empty() )
                    return ctx.zero();

                log_info() << "[operand-lifter]:" << "addr:index:defaulting to"
                                                  << r_addr->index_reg.name;
                return make_reg( r_addr->index_reg.name );
            }();

            log_info() << "[operand-lifter]:" << "addr:scale";
            auto scale = [ & ]() -> llvm::Value *
            {
                if ( auto s_scale = s_addr->scale() )
                    return lift( s_scale );

                return llvm::ConstantInt::get( ctx.word_type(),
                                               static_cast< unsigned >( r_addr->scale ), true );
            }();

            log_info() << "[operand-lifter]:" << "addr:displacement";
            auto displacement = [ & ]() -> llvm::Value *
            {
                if ( auto s_displacement = s_addr->displacement() )
                   return lift( s_displacement );

               return llvm::ConstantInt::get( ctx.word_type(),
                                              static_cast< unsigned >( r_addr->displacement ),
                                              true );
            }();

            auto resize = [ & ]( auto what, bool is_signed )
            {
                if ( what->getType() == ctx.word_type() )
                    return what;

                if ( is_signed )
                    return irb.CreateSExt( what, ctx.word_type() );
                return irb.CreateZExt( what, ctx.word_type() );
            };

            log_info() << "[operand-lifter]:" << "addr:Building the expression!";
            auto scale_factor = irb.CreateShl( resize( index, false ), resize( scale, false ) );

            llvm::Value *out = resize( base, false );
            out = irb.CreateAdd( out, scale_factor );
            out = irb.CreateAdd( out, resize( displacement, true ) );

            return out;
        }


        llvm::Value *make_reg( std::string name )
        {
            return irops::mk_reg(
                irb, ctx.reg( name ),
                ( is_read ) ? irops::io_type::in : irops::io_type::out );
        }

    };
}  // namespace circ::exalt
