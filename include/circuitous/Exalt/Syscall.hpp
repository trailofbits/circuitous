/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Util/Warnings.hpp>

#include <circuitous/Exalt/UnitComponents.hpp>


CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/IRBuilder.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <array>
#include <ranges>

namespace circ::exalt
{
    struct default_syscall_module
    {

        // EAX, EBX, ECX, State
        static const inline std::array< std::size_t, 4 > entries =
        {
            32, 32, 32, 8
        };

        static inline const std::vector< std::string > syscall_regs =
        {
            "EAX", "EBX", "ECX"
        };


        static inline const std::unordered_set< std::string > output_regs =
        {
            "EAX"
        };

        // TODO( exalt:syscall ): Currently manually synced with `syscall_state_value`.
        //                        Unify in one place instead.
        static inline const std::string state = "syscall";

    };

    struct syscall_decoder : decoder_base
    {
        using base = decoder_base;
        using base::base;

        using config = default_syscall_module;

        values_t checks;

        auto init( unit_t &unit ) -> exalted_value_buckets override
        {
            auto &irb = b_ctx.irb();
            auto &state = b_ctx.arch_state();

            auto sys_state = state.load( irb, "syscall." + config::state );
            auto zero = llvm::ConstantInt::get( sys_state->getType(), 0 );
            auto inactive = irb.CreateICmpEQ( sys_state, zero );

            exalted_value_buckets out;

            auto val = [ & ]()
            {
                if ( !is_active( unit ) )
                    return inactive;
                else
                    return irb.CreateXor( inactive, irb.getTrue() );
            }();

            checks.emplace_back( val );
            out[ place::ctx ].emplace( val );

            return out;
        }

        const values_t &atom_decoders() const override
        {
            return checks;
        }

        value_t unit_decoder() const override
        {
            check( !checks.empty() );
            return checks.front();
        }

        bool is_active( unit_t &unit ) const override
        {
            return !unit.external_submodule( "syscalls" );
        }

    };

    struct syscall_component : uc_with_b_ctx
    {
        using base = uc_with_b_ctx;
        using base::base;

        values_t args;
        value_t submodule;
        values_t parsed_results;

        // on_entry -> on_transition -> on_exit -> on_inactive
        std::array< value_t, 4 > dfa = { nullptr };

        // `syscall_active => ???`
        exalted_values_t checks;


        using config = default_syscall_module;

        /* Value helpers */

        auto state_input() { return args[ 0 ]; }
        auto state_runtime() { return parsed_results[ 0 ]; }
        auto state_output()
        {
            auto out = irops::io_type::out;;
            return irops::make_leaf< irops::SyscallState >( irb(), out );
        }

        auto state_transition_check()
        {
            return irops::OutputCheck::make( irb(), { state_runtime(), state_output() } );
        }

        value_t reg_runtime( const std::string &reg_name )
        {
            for ( std::size_t i = 0; i < config::syscall_regs.size(); ++i )
                if ( reg_name == config::syscall_regs[ i ] )
                    return parsed_results[ i + 1 ];
            log_kill() << "Could not fetch syscall arg" << reg_name << ".";
        }

        auto reg_output( const std::string &reg_name )
        {
            auto out = irops::io_type::out;;
            return irops::make_leaf< irops::SyscallReg >( irb(), 32u, reg_name, out );
        }

        /* `component_base` */

        bool is_persistent() const override { return true; }

        void inject_state()
        {
            auto &state = b_ctx.arch_state();

            for ( const auto &reg_name : config::syscall_regs )
            {
                auto reg = b_ctx.ctx.reg( reg_name );
                check( reg );
                auto config = std::make_unique< syscall_reg_value >( reg );
                config->bind( irb() );
                state.add( irb(), std::move( config ) );
            }

            auto sys_state_config = std::make_unique< syscall_state_value >( irb() );
            sys_state_config->bind( irb() );
            state.add( irb(), std::move( sys_state_config ) );
        }

        std::vector< component_t > spawn_on_unit( builder_context &b_ctx ) const override
        {
            return { std::make_shared< syscall_decoder >( b_ctx ) };
        }

        // Emit the call
        void init() override
        {
            inject_state();
            log_dbg() << "[exalt:syscall]:" << "Aux registers injected to state!";
            //args = make_args( irb() );
            //submodule = irops::make< irops::SyscallSubmodule >( irb(), args );
            //parsed_results = get_all( irb(), submodule );

            //// NEXT: Is this needed.
            //b_ctx.arch_state().reset( irb() );

            //build_on_entry();
            //build_on_transition();
            //build_on_exit();
            //build_on_inactive();
            log_dbg() << "[exalt:syscall]:" << "Syscall module initialization done!";
        };

        //value_t constant( llvm::Type *t, auto i )
        //{
        //    return llvm::ConstantInt::get( t, i );
        //}

        //value_t constant( value_t val, auto i ) { return constant( val->getType(), i ); }

        //value_t is_inactive( value_t val )
        //{
        //    auto zero = llvm::ConstantInt::get( val->getType(), 0 );
        //    return irb().CreateICmpEQ( val, zero );
        //}

        //value_t is_active( value_t val )
        //{
        //    return irb().CreateXor( is_inactive( val ), irb().getTrue() );
        //}

        //auto mk_check( value_t a, value_t b )
        //{
        //    return irops::OutputCheck::make( irb(), { a, b } );
        //}


        //void build_on_entry()
        //{
        //    values_t top;

        //    // Si == 0 && ( Sr == So =! 0)
        //    values_t ctx_partials =
        //    {
        //        irb().CreateICmpEQ( state_runtime(), state_output() ),
        //        is_inactive( state_input() ),
        //        is_active( state_output() )
        //    };
        //    top.emplace_back( irops::And::make( irb(), ctx_partials ) );

        //    // Now we we need to forward normal regs to syscall regs
        //    for ( const auto &reg_name : syscall_regs )
        //    {
        //        auto loaded = b_ctx.arch_state().load( irb(), reg_name );
        //        values_t check_args = { loaded, reg_output( reg_name ) };
        //        top.emplace_back( irops::OutputCheck::make( irb(), check_args ) );
        //    }

        //    // Output state is initialized to truncated EAX
        //    auto eax = b_ctx.arch_state().load( irb(), "EAX" );
        //    auto trunc = irb().CreateTrunc( eax, state_output()->getType() );
        //    top.emplace_back( mk_check( trunc, state_output() ) );

        //    dfa[ 0 ] = irops::And::make( irb(), top );

        //}

        //void build_on_transition()
        //{
        //    values_t top;

        //    // Si != 0 && ( Sr == So =! 0)
        //    values_t ctx_partials =
        //    {
        //        irb().CreateICmpEQ( state_runtime(), state_output() ),
        //        is_active( state_input() ),
        //        is_active( state_output() )
        //    };
        //    top.emplace_back( irops::And::make( irb(), ctx_partials ) );

        //    // Nothing special, returned values from the submodule must equal
        //    // to their output trace fields.
        //    // Same goes for the state itself.
        //    for ( const auto &reg_name : syscall_regs )
        //    {
        //        values_t check_args = { reg_runtime( reg_name ), reg_output( reg_name ) };
        //        top.emplace_back( irops::OutputCheck::make( irb(), check_args ) );
        //    }
        //    top.emplace_back( state_transition_check() );

        //    dfa[ 1 ] = irops::And::make( irb(), top );
        //}

        //void build_on_exit()
        //{
        //    values_t top;

        //    // Si != 0 && ( Sr == So == 0)
        //    values_t ctx_partials =
        //    {
        //        irb().CreateICmpEQ( state_runtime(), state_output() ),
        //        is_active( state_input() ),
        //        is_inactive( state_output() )
        //    };
        //    top.emplace_back( irops::And::make( irb(), ctx_partials ) );

        //    // We do not care? about syscall regs anymore. Only check needed
        //    // is that syscall outputs are forwarded to their respective real
        //    // registers.
        //    for ( const auto &reg_name : output_regs )
        //    {
        //        auto current = reg_output( reg_name );
        //        b_ctx.arch_state().store( irb(), reg_name, current );
        //    }
        //}

        //void build_on_inactive()
        //{
        //    values_t top;

        //    // Si == 0 && ( Sr == So == 0)
        //    values_t ctx_partials =
        //    {
        //        irb().CreateICmpEQ( state_runtime(), state_output() ),
        //        is_inactive( state_input() ),
        //        is_inactive( state_output() )
        //    };
        //    top.emplace_back( irops::And::make( irb(), ctx_partials ) );
        //}

        //// I think we do not need local init per unit

        //// We may need to tie some delayed computations
        //void finalize() override {};

        ///* `unit_component_base` */

        //// If the `unit` is interrupt.
        //// * Check number
        ////   + Set input syscall_regs => add output_checks.
        ////   + Set syscall state?
        ////   + Set syscall_computation on => add output_check.
        //exalted_values_t after_isem( unit_t &, isem_range_t ) override { return {}; }

        ///* Local helpers */

        //static bool has_syscall_variant( reg_ptr_t reg )
        //{
        //    return std::ranges::count( syscall_regs, reg->name );
        //}

        //static bool is_output( reg_ptr_t reg ) { return output_regs.count( reg->name ); }

        //static std::size_t bw()
        //{
        //    std::size_t out = 0;
        //    for ( auto x : entries )
        //        out += x;
        //    return out;
        //}

        //static values_t get_all( builder_t &irb, value_t submodule )
        //{
        //    values_t out;

        //    std::size_t offset = 0;
        //    for ( auto size : entries )
        //    {
        //        auto val = irops::ExtractRaw::make( irb, submodule, offset, size );
        //        offset += size;

        //        out.push_back( val );
        //    }
        //    return out;
        //}

        //static values_t make_args( builder_t &irb )
        //{
        //    // Needs to an rvalue.
        //    // TODO( ir ): Fix the intrinsic code that forces this.
        //    auto in = [ & ] { return irops::io_type::in; };

        //    auto mk_reg = [ & ]( const auto &name )
        //    {
        //        return irops::make_leaf< irops::SyscallReg >( irb, 32u, name, in() );
        //    };

        //    // TODO( exalt ): Do we want to encode this as we do
        //    //                in let's say alien trace parsing?
        //    values_t out = { irops::make_leaf< irops::SyscallState >( irb, in() ) };
        //    for ( const auto &reg_name : syscall_regs )
        //        out.push_back( mk_reg( reg_name ) );

        //    return out;
        //}

        //value_t get( reg_ptr_t reg )
        //{
        //    for ( std::size_t i = 0; i < syscall_regs.size(); ++i )
        //        if ( reg->name == syscall_regs[ i ] )
        //            return parsed_results[ i + 1 ];
        //    log_kill() << "Could not fetch syscall arg" << reg->name << ".";
        //}
    };

} // namespace circ::exalt
