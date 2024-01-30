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

    RemillArchState::RemillArchState( builder_t &irb, CtxRef ctx_ref )
        : Base( irb, ctx_ref )
    {
        // Here we are extracting how remill does things. We are not calling
        // any of its native api since those methods expect very specific
        // state of the function (arguments etc.) which we are not adhering to.

        auto u8 = llvm::Type::getInt8Ty( *ctx.llvm_ctx() );

        auto mk = [ & ]( auto name, auto type )
        {
            auto var = irb.CreateAlloca( type, nullptr, name );
            pseudo_regs[ name ] = std::make_tuple( var, type );
            return var;
        };

        mk( "BRANCH_TAKEN", u8 );

        // For now we ignore `PC_NEXT` and other control flow related
        // pseudos, as lifter context aliases them with pc reg.
    }

    void RemillArchState::store( builder_t &irb, const reg_ptr_t where, value_t what )
    {
        auto it = pseudo_regs.find( where->name );
        if ( it == pseudo_regs.end() )
            return this->Base::store( irb, where, what );

        auto [ inst, _ ] = it->second;
        //auto ptr_type = llvm::PointerType::getUnqual( type );
        irb.CreateStore( what, inst );
    }

    value_t RemillArchState::load( builder_t &irb, const reg_ptr_t where )
    {
        auto it = pseudo_regs.find( where->name );
        if ( it == pseudo_regs.end() )
            return this->Base::load( irb, where );

        auto [ inst, type ] = it->second;
        return irb.CreateLoad( type, inst );
    }

    void RemillArchState::reset( builder_t &irb, const Ctx::regs_t &regs )
    {
        // First we reset all normal regs, since we need the original values.
        this->Base::reset( irb, regs );

        // BRANCH_TAKEN should not need any special resetting as it starts undefined?
    }

    void RemillArchState::store( builder_t &irb, const std::string &where, value_t what )
    {
        auto it = pseudo_regs.find( where );
        if ( it == pseudo_regs.end() )
            return this->Base::store( irb, ctx.reg( where ), what );

        auto [ inst, _ ] = it->second;
        irb.CreateStore( what, inst );
    }

    value_t RemillArchState::load( builder_t &irb, const std::string &where )
    {
        auto it = pseudo_regs.find( where );
        if ( it == pseudo_regs.end() )
        {
            return this->Base::load( irb, ctx.reg( where ) );
        }

        auto [ inst, type ] = it->second;
        return irb.CreateLoad( type, inst );
    }

    /* `ExtendedState` */

    void ExtendedState::add( builder_t &irb, state_value_t config )
    {
        check( !storage.count( config->key() ) );

        auto as_ptr = llvm::PointerType::get( config->type, 0 );
        auto mat = irb.CreateAlloca( as_ptr, nullptr, config->key() );
        storage.emplace( config->key(), std::make_tuple( mat, std::move( config ) ) );
    }

    void ExtendedState::add( builder_t &irb, std::vector< state_value_t > configs )
    {
        for ( auto &&c : std::move( configs ) )
            add( irb, std::move( c ) );
    }

    value_t ExtendedState::load( builder_t &irb, const std::string &name )
    {
        if ( auto it = storage.find( name ); it != storage.end())
        {
            auto &[ val, config ] = it->second;
            return irb.CreateLoad( config->type, val );
        }

        return this->base::load( irb, name );
    }

    void ExtendedState::store( builder_t &irb, const std::string &name, value_t what )
    {
        if ( auto it = storage.find( name ); it != storage.end())
        {
            auto &[ val, _ ] = it->second;
            irb.CreateStore( what, val );
        }

        return this->base::store( irb, name, what );
    }

    void ExtendedState::reset( builder_t &irb )
    {
        for ( const auto &[ _, mat ] : storage )
        {
            const auto &[ val, config ] = mat;
            irb.CreateStore( config->in(), val );
        }

        return base::reset( irb );
    }

    void ExtendedState::commit( builder_t &irb )
    {
        // I do not think `commit` is actually useful anymore.
        log_kill() << "Not implemented, should not be used?";
    }

    std::string ExtendedState::to_string() const
    {
        std::stringstream ss;
        ss << "ExtendedState:\n"
           << "{\n";
        for ( const auto &[ k, m ] : storage )
        {
            ss << "\t" << k << " -> ";
            const auto &[ _, config ] = m;
            ss << config->to_string() << "\n";
        }

        ss << "}\n";
        return ss.str();
    }

} // namespace circ::exalt
