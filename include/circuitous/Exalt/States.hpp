/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Exalt/Common.hpp>

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/Lifter.h>

#include <unordered_map>
#include <vector>

namespace circ::exalt
{


    // TODO(exalt): Can this be used as a generic base for
    //              `syscall_submodule`?
    struct wraps_remill_value
    {
      protected:
        value_t storage = nullptr;

      public:

        wraps_remill_value() = delete;
        wraps_remill_value( value_t storage ) : storage( storage ) {}

        wraps_remill_value( llvm::BasicBlock *where, type_t t )
            : storage( builder_t( where ).CreateAlloca( t ) )
        {}

        wraps_remill_value( builder_t &bld, type_t t )
            : storage( bld.CreateAlloca( t ) )
        {}

        wraps_remill_value( llvm::Function *fn, type_t t );

        value_t operator->() const { return storage; }
        value_t operator->() { return storage; }

        value_t operator*() const { return storage; }
        value_t operator*() { return storage; }
    };

    // TODO( next ): Rename
    struct State : wraps_remill_value, has_ctx_ref
    {
        using reg_ptr_t = const remill::Register *;

        State( builder_t &irb, CtxRef ctx_ref )
            : wraps_remill_value( irb, ctx_ref.state_type() ),
              has_ctx_ref( ctx_ref )
        {}

        State( llvm::BasicBlock *where, type_t t, CtxRef ctx_ref )
            : wraps_remill_value( where, t ),
              has_ctx_ref( ctx_ref )
        {}

        virtual ~State() = default;

        virtual void store(builder_t &ir, const reg_ptr_t where, value_t what);
        virtual value_t load(builder_t &ir, const reg_ptr_t where);

        virtual void store( builder_t &irb, const std::string &name, value_t what )
        {
            return this->store( irb, ctx.reg( name ), what );
        }

        virtual value_t load( builder_t &irb, const std::string &name )
        {
            return this->load( irb, ctx.reg( name ) );
        }

        // This is a very random function, but sadly the semantic do store this
        // into a state instead of properly using it as a value, so we do not
        // have much choice.
        virtual value_t load_interrupt_vector( builder_t &irb );

        virtual void reset( builder_t &irb, const Ctx::regs_t &regs );
        virtual void commit( builder_t &irb, CtxRef ctx );

        virtual std::string to_string() const
        {
            return {};
        }
    };

    struct ArchState : State
    {
        using Base = State;
        using Base::Base;

        virtual void reset( builder_t &irb ) { return this->reset( irb, ctx.regs() ); }
        using Base::reset;

        virtual void commit( builder_t &irb ) { return this->commit( irb, ctx ); }
        using Base::commit;
    };

    // We need to support storing/loading from pseudo-registers (those present
    // in semantics but not in the `State` structure itself in llvm).
    // These are represented as variable in `llvm::BasicBlock` in remill's lifters
    // but it is better to hide that behind one abstraction if we can.
    struct RemillArchState : ArchState
    {
        using Base = ArchState;
        using Base::Base;

        using entry_t = std::tuple< llvm::Instruction *, llvm::Type * >;
        using storage = std::unordered_map< std::string, entry_t >;
        storage pseudo_regs;

        // Create `State` and all pseudo-regs we need.
        RemillArchState( builder_t &irb, CtxRef ctx_ref );

        using Base::reset;
        using Base::commit;

        // We hijack all register accesors to also check the `pseudo_regs`.
        void store(builder_t &ir, const reg_ptr_t where, value_t what) override;
        value_t load(builder_t &ir, const reg_ptr_t where) override;

        void reset( builder_t &irb, const Ctx::regs_t &regs ) override;

        void store( builder_t &irb, const std::string &name, value_t what ) override;
        value_t load( builder_t &irb, const std::string &name ) override;

    };

    struct state_backed_value
    {
        std::string name;
        std::string prefix;

        llvm::Type *type = nullptr;
        values_t values = { nullptr, nullptr };

        state_backed_value( const std::string &name,
                            const std::string &prefix,
                            llvm::Type *type )
            : name( name ),
              prefix( prefix ),
              type( type )
        {}

        virtual ~state_backed_value() = default;

        // Primary key into state structure
        virtual std::string key() const
        {
            return prefix + name;
        }

        // If the value has trace entry, these will return those values
        // If not, `nullptr` is returned;
        virtual value_t in() const
        {
            return values[ 0 ];
        }

        virtual value_t out() const
        {
            return values[ 1 ];
        }

        template< typename I, typename ... Args >
        void bind_to_intrinsic( builder_t &irb, Args && ... args )
        {
            auto in  = irops::make_leaf< I >( irb, args ..., irops::io_type::in );
            auto out = irops::make_leaf< I >( irb, args ..., irops::io_type::out );

            values[ 0 ] = in;
            values[ 1 ] = out;
        }

        virtual void bind( builder_t &irb ) = 0;

        virtual std::string to_string() const = 0;
    };

    struct syscall_reg_value : state_backed_value
    {
        using base = state_backed_value;

        syscall_reg_value( const reg_ptr_t reg )
            : base( reg->name, "syscall.", reg->type )
        {}

        void bind( builder_t &irb ) override
        {
            return this->base::bind_to_intrinsic< irops::SyscallReg >( irb, 32u, name );
        }

        std::string to_string() const override
        {
            return "[syscall_reg_value]: " + this->key();
        }
    };

    struct syscall_state_value : state_backed_value
    {
        using base = state_backed_value;

        syscall_state_value( builder_t &irb )
            : base( "state", "syscall.", irb.getIntNTy( 8 ) )
        {}

        void bind( builder_t &irb ) override
        {
            return this->base::bind_to_intrinsic< irops::SyscallState >( irb );
        }

        std::string to_string() const override
        {
            return "[syscall_state_value]: " + this->key();
        }
    };

    struct ExtendedState : RemillArchState
    {
        using base = RemillArchState;
        using base::base;

        using state_value_t = std::unique_ptr< state_backed_value >;
        using materialized = std::tuple< value_t, state_value_t >;

        using key_t = std::string;
        std::unordered_map< key_t, materialized > storage;

        // Will take ownership of ptrs.
        void add( builder_t &irb, state_value_t config );
        void add( builder_t &irb, std::vector< state_value_t > configs );

        using base::load;
        value_t load( builder_t &irb, const std::string &name ) override;
        using base::store;
        void store( builder_t &irb, const std::string &name, value_t what ) override;

        using base::reset;
        void reset( builder_t &irb ) override;
        using base::commit;
        void commit( builder_t &irb ) override;

        value_t in( builder_t &irb, const key_t &key )
        {
            if ( auto it = storage.find( key ); it != storage.end() )
            {
                return std::get< 1 >( it->second )->in();
            }

            return irops::input_reg( irb, ctx.reg( key ) );
        }

        value_t out( builder_t &irb, const key_t &key )
        {
            if ( auto it = storage.find( key ); it != storage.end() )
            {
                return std::get< 1 >( it->second )->out();
            }

            return irops::output_reg( irb, ctx.reg( key ) );
        }

        gap::generator< key_t > trace_fields()
        {
            for ( const auto &[ name, _ ] : storage )
                co_yield key_t( name );

            for ( const auto &reg : ctx.regs() )
                co_yield key_t( reg->name );
        }

        std::string to_string() const override;

    };

    struct MemoryPtr : wraps_remill_value
    {
        MemoryPtr( type_t t )
            : wraps_remill_value( llvm::UndefValue::get( t ) )
        {}
    };

    struct Trace : has_ctx_ref
    {
        std::vector< State > storage;

        Trace( CtxRef ctx, State input, State output )
            : has_ctx_ref( ctx ),
              storage{ input, output }
        {}

        void reset( builder_t &bld, const Ctx::regs_t &regs )
        {
            for ( auto &state : storage )
                state.reset( bld, regs );
        }

        void commit( builder_t &bld )
        {
            for ( auto &state : storage )
                state.commit( bld, ctx );
        }

        auto &input() { return storage[ 0 ]; }
        auto &output() { return storage[ 1 ]; }
    };

}  // namespace circ::exalt
