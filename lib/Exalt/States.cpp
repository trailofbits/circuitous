/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#include <circuitous/Exalt/States.hpp>

#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Check.hpp>

CIRCUITOUS_RELAX_WARNINGS
CIRCUITOUS_UNRELAX_WARNINGS

#include <sstream>

namespace circ::exalt
{
    wraps_remill_value::wraps_remill_value( llvm::Function *fn, llvm::Type *t )
    {
        check( !fn->isDeclaration() );
        storage = llvm::IRBuilder<>( &*fn->begin() ).CreateAlloca( t );
    }

    void State::store(llvm::IRBuilder<> &ir, const reg_ptr_t reg, value_t val)
    {
        auto bb = ir.GetInsertBlock();
        const auto &dl = bb->getModule()->getDataLayout();
        auto gep = reg->AddressOf(storage, bb);
        ir.SetInsertPoint(bb);

        // How much space does register occupy in form iN. There is an
        // optimization for flag registers.
        auto reg_type = irops::int_reg_type(*bb->getModule(), reg);
        auto store_type =
            ir.getIntNTy(static_cast<unsigned>(dl.getTypeAllocSize(reg_type) * 8u));
        auto coerced_type = ir.CreateBitCast(gep, llvm::PointerType::getUnqual(store_type));

        if (reg_type != store_type)
            val = ir.CreateZExt(val, store_type);
        ir.CreateStore(val, coerced_type);
    }

    value_t State::load(llvm::IRBuilder<> &ir, const reg_ptr_t reg)
    {
        check( reg );
        auto bb = ir.GetInsertBlock();
        const auto &dl = bb->getModule()->getDataLayout();
        auto gep = reg->AddressOf(storage, bb);
        ir.SetInsertPoint(bb);

        // How much space does register occupy in form iN. There is an
        // optimization for flag registers.
        auto reg_type = irops::int_reg_type(*bb->getModule(), reg);
        auto store_type =
            ir.getIntNTy(static_cast<unsigned>(dl.getTypeAllocSize(reg_type) * 8u));

        auto loaded = ir.CreateLoad(store_type, gep);
        if (reg_type != store_type)
            return ir.CreateTrunc(loaded, reg_type);

        return loaded;
    }

    value_t State::load_interrupt_vector( llvm::IRBuilder<> &irb )
    {
        // TODO: Right now we are guessing this is `i32` value based
        //       on the definition of `ArchState`.
        auto iN = [ & ]( auto type_size, auto val )
        {
            return irb.getIntN( static_cast< uint32_t >( type_size ),
                                static_cast< uint32_t >( val ) );
        };
        auto i32_t = irb.getIntNTy( 32u );

        auto gep = irb.CreateGEP( i32_t, **this, { iN( 64, 0 ), iN( 32, 2 ) } );
        return irb.CreateLoad( i32_t, gep );
    }


    void State::reset( llvm::IRBuilder<> &irb, const Ctx::regs_t &regs )
    {
        log_dbg() << "[state]: reset";
        for ( const auto &reg : regs )
            store( irb, reg, irops::input_reg( irb, reg ) );
    }

    void State::commit( llvm::IRBuilder<> &irb, CtxRef ctx )
    {
        std::vector< value_t  > args;
        for ( const auto &reg : ctx.regs() )
            args.push_back( load( irb, reg ) );
        irops::make< irops::Commit >( irb, args, 1u );

    }
} // namespace circ::exalt
