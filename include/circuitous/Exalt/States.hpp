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
    // Provides wrapper around syscall related things similar to what we have for
    // remill State.
    struct syscall_submodule
    {
        // Technically we cannot return more values, therefore what we do instead is
        // `%T = submodule( %state_in, %eax, %ebx, %ecx )`
        // followed by a bunch of extracts from `%T` to have a nice api.
        value_t submodule;
        std::vector< value_t  > resutls;

        static const inline std::array< std::size_t, 4 > entries =
        {
            8, 32, 32, 32
        };

        static inline const std::vector< std::string > syscall_regs =
        {
            "EAX", "EBX", "ECX"
        };


        static inline const std::unordered_set< std::string > output_regs =
        {
            "EAX"
        };

        static bool has_syscall_variant( reg_ptr_t reg )
        {
            return std::ranges::count( syscall_regs, reg->name );
        }

        static bool is_output( reg_ptr_t reg ) { return output_regs.count( reg->name ); }

        static std::size_t bw()
        {
            std::size_t out = 0;
            for ( auto x : entries )
                out += x;
            return out;
        }

        static values_t get_all( builder_t &irb, value_t submodule )
        {
            values_t out;

            std::size_t offset = 0;
            for ( auto size : entries )
            {
                auto val = irops::ExtractRaw::make( irb, submodule, offset, size );
                offset += size;

                out.push_back( val );
            }
            return out;
        }

        syscall_submodule( builder_t &irb )
            : syscall_submodule( irb, make_args( irb ) )
        {}

        syscall_submodule( builder_t &irb, const std::vector< value_t > &args )
        {
            // I am for now initializing inside the body, as we most likely
            // will want to accept something better than a raw vector.
            submodule = irops::make< irops::SyscallSubmodule >( irb, args );
            resutls = get_all( irb, submodule );
        }

        static values_t make_args( builder_t &irb )
        {
            // Needs to an rvalue.
            // TODO( ir ): Fix the intrinsic code that forces this.
            auto in = [ & ] { return irops::io_type::in; };

            auto mk_reg = [ & ]( const auto &name )
            {
                return irops::make_leaf< irops::SyscallReg >( irb, 32u, name, in() );
            };

            // TODO( exalt ): Do we want to encode this as we do
            //                in let's say alien trace parsing?
            values_t out = { irops::make_leaf< irops::SyscallState >( irb, in() ) };
            for ( const auto &reg_name : syscall_regs )
                out.push_back( mk_reg( reg_name ) );

            return out;
        }

        value_t get( reg_ptr_t reg )
        {
            for ( std::size_t i = 0; i < syscall_regs.size(); ++i )
                if ( reg->name == syscall_regs[ i ] )
                    return resutls[ i + 1 ];
            log_kill() << "Could not fetch syscall arg" << reg->name << ".";
        }
    };

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
    };

    struct ArchState : State
    {
        using Base = State;
        using Base::Base;

        auto reset( builder_t &irb ) { return this->reset( irb, ctx.regs() ); }
        using Base::reset;

        auto commit( builder_t &irb ) { return this->commit( irb, ctx ); }
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
