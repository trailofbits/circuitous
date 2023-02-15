/*
 * Copyright (c) 2023 Trail of Bits, Inc.
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

#include <circuitous/Lifter/Components/OperandSelection.hpp>
#include <circuitous/Lifter/SReg.hpp>

#include <circuitous/Util/Overloads.hpp>

namespace circ::build
{
    namespace
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

        llvm::Value *make_reg( llvm::IRBuilder<> &irb, auto &ctx, std::string name,
                               bool is_read )
        {
            return irops::mk_reg(
                irb, ctx.reg( name ),
                ( is_read ) ? irops::io_type::in : irops::io_type::out );
        }
    } // namespace


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
                                                bool is_read )
    {
        auto selector = irops::make_leaf< irops::OpSelector >(
                irb, tm.bitsize, next_id++ );

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
        get( is_read )[ idx ].emplace_back( make_select( irb, pristine, is_read ) );
        return get( is_read )[ idx ].back();
    }

    auto OperandSelection::request( llvm::IRBuilder<> &irb,
                                    std::size_t idx,
                                    std::size_t nth, bool is_read )
        -> llvm::Value *
    {
        auto &already_present = get( is_read )[ idx ];
        while ( already_present.size() <= nth )
            already_present.emplace_back( make_select( irb, ( *this )[ idx ], is_read ) );

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
        return OperandLifter( ctx, irb, *this, info ).lift();
    }

    auto OperandLifter::lift() &&
        -> values_t
    {
        values_t out;

        check( info.has_shadow() ) << info.rinst().Serialize();
        const auto &s_ops = info.shadow().operands;
        const auto &r_ops = info.rinst().operands;
        check( s_ops.size() == r_ops.size() );

        log_info() << "Lift:" << info.rinst().Serialize() << "\n" << info.shadow().to_string();

        for ( std::size_t i = 0; i < s_ops.size(); ++i )
        {
            is_read = r_ops[ i ].action == remill::Operand::kActionRead;
            out.push_back( lift( s_ops[ i ], r_ops[ i ] ) );
        }

        return out;
    }

    llvm::Value *OperandLifter::request( const shadowinst::TM_t &tm )
    {
       if ( is_read )
           return read_requester.request( irb, tm );
       return write_requester.request( irb, tm );
    }

    llvm::Value *OperandLifter::lift( const shadowinst::Operand &s_op,
                                      const remill::Operand &r_op )
    {
        if ( auto imm = s_op.immediate() )
            return lift( *imm );
        if ( auto reg = s_op.reg() )
            return lift( *reg, r_op.reg );
        if ( auto addr = s_op.address() )
            return lift( *addr, r_op.addr );
        check( !s_op.shift() );

        return nullptr;
    }

    llvm::Value *OperandLifter::lift( const shadowinst::Immediate &s_imm )
    {
        if ( s_imm.regions.marked_size() == 0 )
            return ctx.zero();
        check( s_imm.regions.marked_size() != 0 );
        // This maybe needs to be relaxed?
        check( s_imm.regions.areas.size() == 1 );

        auto [ from, size ] = *( s_imm.regions.areas.begin() );
        return irops::make_leaf< irops::InstExtractRaw >( irb, from, size );
    }

    llvm::Value *OperandLifter::lift( const shadowinst::Reg &s_reg,
                                      const remill::Operand::Register &r_reg )
    {
        if ( s_reg.tm().is_saturated_by_zeroes() )
        {
            auto size = static_cast< unsigned >( r_reg.size );
            return llvm::ConstantInt::get( irb.getIntNTy( size ), 0, false );
        }

        auto select = request( s_reg.tm() );
        check( select );

        return shadowinst::mask_shift_coerce( select, irb, s_reg, *ctx.arch() );
    }

    llvm::Value *OperandLifter::lift( const shadowinst::Address &s_addr,
                                      const remill::Operand::Address &r_addr )
    {
        // We are on purpose ignoring segments here, even though they are reflected in the
        // decoder check that gets emitted eventually.
        auto base = [ & ]() -> llvm::Value *
        {
            if ( auto s_base = s_addr.base_reg() )
                return lift( *s_base, r_addr.base_reg );

            if ( r_addr.base_reg.name.empty() )
                return ctx.zero();

            return make_reg( irb, ctx, r_addr.base_reg.name, is_read );
        }();

        auto index = [ & ]() -> llvm::Value *
        {
            if ( auto s_index = s_addr.index_reg() )
                return lift( *s_index, r_addr.index_reg );

            if ( r_addr.index_reg.name.empty() )
                return ctx.zero();

            return make_reg( irb, ctx, r_addr.index_reg.name, is_read );
        }();

        auto scale = [ & ]() -> llvm::Value *
        {
            if ( auto s_scale = s_addr.scale() )
                return lift( *s_scale );

            return llvm::ConstantInt::get( ctx.word_type(),
                                           static_cast< unsigned >( r_addr.scale ), true );
        }();

        auto displacement = [ & ]() -> llvm::Value *
        {
            if ( auto s_displacement = s_addr.displacement() )
               return lift( *s_displacement );

           return llvm::ConstantInt::get( ctx.word_type(),
                                          static_cast< unsigned >( r_addr.displacement ),
                                          true );
        }();

        auto resize = [ & ]( auto what )
        {
            if ( what->getType() == ctx.word_type() )
                return what;
            return irb.CreateSExt( what, ctx.word_type() );
        };

        auto scale_factor = irb.CreateShl( resize( index ), resize( scale ) );

        llvm::Value *out = resize( base );
        out = irb.CreateAdd( out, scale_factor );
        out = irb.CreateAdd( out, resize( displacement ) );

        return out;
    }


}  // namespace circ::build
