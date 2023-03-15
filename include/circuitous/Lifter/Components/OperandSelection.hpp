/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Lifter/Instruction.hpp>
#include <circuitous/Lifter/Component.hpp>

#include <circuitous/Lifter/SReg.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/IntrinsicTable.h>

#include <string>
#include <unordered_map>
#include <vector>

namespace circ::isem
{
    struct ISem;
} // namespace circ::isem

namespace circ::build
{
    using values_t = std::vector< llvm::Value * >;
    using value_gen_t = gap::generator< llvm::Value * >;

    llvm::Type *zero_reg_type( llvm::IRBuilder<> &irb, llvm::StringRef name );

    // At each index in returned vector there is either
    //  * value if shadow can be materialized via one of the chosen selects
    //  * no value if shadow is not present and instead some hardcoded value
    //    is used. Is this an error?
    using maybe_values_t = std::vector< std::optional< llvm::Value * > >;
    // [ write, read ]
    using lifted_operands_t = std::tuple< maybe_values_t, maybe_values_t >;

    using gen_maybe_val_t = gap::generator< std::optional< llvm::Value * > >;

    struct OperandSelection : has_ctx_ref
    {
        using tms_t = std::vector< shadowinst::TM_t >;
      private:

        struct Builder
        {
            tms_t saturated;

            void add_sat( const shadowinst::TM_t &tm );
            void add_unsat( const shadowinst::TM_t &tm );
        };

      public:

        struct Requester
        {
            OperandSelection &storage;
            bool is_read;

            std::unordered_map< std::size_t, std::size_t > processed;

            Requester( OperandSelection &storage, bool is_read )
                : storage( storage ), is_read( is_read )
            {}

            llvm::Value *request( llvm::IRBuilder<> &irb, std::size_t idx );
            llvm::Value *request( llvm::IRBuilder<> &irb, const shadowinst::TM_t &tm );
        };

        const std::vector< shadowinst::TM_t > saturated;

        // [ index into `saturated` -> llvm instruction that realised the selection ]
        std::map< std::size_t, std::vector< llvm::Value * > > read_map = {};
        std::map< std::size_t, std::vector< llvm::Value * > > write_map = {};
        std::size_t next_id = 0;

        OperandSelection( CtxRef ctx, const tms_t &saturated )
            : has_ctx_ref( ctx ), saturated( saturated )
        {}

        auto &get( bool is_read )
        {
            if ( is_read )
                return read_map;
            return write_map;
        }

        static llvm::Value *make_select( llvm::IRBuilder<> &irb,
                                         const shadowinst::TM_t &tm,
                                         Ctx &ctx,
                                         llvm::Value *selector,
                                         bool is_read );

        llvm::Value *materialize( llvm::IRBuilder<> &irb, std::size_t idx, bool is_read );
        llvm::Value *request( llvm::IRBuilder<> &irb, std::size_t idx,
                              std::size_t nth, bool is_read );

        std::size_t match( const shadowinst::TM_t &other )
        {
            for ( std::size_t i = 0; i < saturated.size(); ++i )
                if ( other.is_subset_of( saturated[ i ] ) )
                    return i;
            log_kill() << "Was not able to recover select template!";
        }

        // Does not allow additions once this is build.
        // TODO(lifter): Currently there is no extra value in allowing to add extras later.
        static OperandSelection build( CtxRef ctx, const InstructionBatch &batch );

        values_t assign( llvm::IRBuilder<> &irb, const InstructionInfo &info );

        shadowinst::TM_t operator[]( std::size_t idx ) const
        {
            check( idx < saturated.size() );
            return saturated[ idx ];
        }

        std::string to_string() const;
    };

    template< typename RegState >
    struct StatelessRequester
    {
        using state_t = RegState;
        using value_t = llvm::Value *;
        using values_t = std::vector< llvm::Value * >;

        Ctx &ctx;
        state_t &state;

        std::size_t next_idx = 0;

        StatelessRequester( Ctx &ctx, state_t &state )
            : ctx( ctx ), state( state )
        {}

        value_t make_select( llvm::IRBuilder<> &irb,
                             const shadowinst::TM_t &tm,
                             llvm::Value *selector,
                             bool is_read,
                             auto &&coerce )
        {
            std::vector< llvm::Value * > args( ( 1 << tm.bitsize ) + 1, nullptr );
            args[ 0 ] = selector;

            for ( const auto &[ str, reg_name ] : tm.reverse_bitmap() )
            {
                if ( reg_name.starts_with( "__remill_zero" ) )
                    continue;

                auto idx = llvm::APInt( static_cast< uint32_t >( tm.bitsize ), str, 2 )
                    .getLimitedValue();

                check( args.size() > idx + 1 );
                args[ idx + 1 ] = coerce( state.load( irb, ctx.reg( reg_name ) ) );
            }

            auto trg_type = [ & ]()
            {
                for ( std::size_t i = 1; i < args.size(); ++i)
                    if ( args[ i ] )
                        return args[ i ]->getType();
                log_kill() << "Trying to create select without values";
            }();

            auto zero = llvm::ConstantInt::get( trg_type, 0, false );
            for ( std::size_t i = 0; i < args.size(); ++i )
                if ( !args[ i ] )
                    args[ i ] = zero;

            return irops::Select::make( irb, args );
        }


        llvm::Value *request( llvm::IRBuilder<> &irb, const shadowinst::TM_t &tm,
                              bool is_read, auto &&coerce )
        {
            auto selector = irops::make_leaf< irops::OpSelector >( irb, tm.bitsize,
                                                                   next_idx++ );
            return make_select( irb, tm, selector, is_read, coerce );
        }

        llvm::Value *request( llvm::IRBuilder<> &irb, const shadowinst::Reg &s_reg,
                              bool is_read, auto &&coerce )
        {
            auto selector = shadowinst::Materializer( irb, s_reg ).region_selector();
            return make_select( irb, s_reg.tm(), selector, is_read, coerce );
        }
    };

    template< typename State >
    StatelessRequester( Ctx, State ) -> StatelessRequester< State >;

    template< typename Req >
    struct OperandLifter : has_ctx_ref
    {
        using requester_t = Req;

        llvm::IRBuilder<> &irb;
        requester_t &requester;
        bool is_read;

        OperandLifter( CtxRef &ctx,
                       llvm::IRBuilder<> &irb,
                       requester_t &requester,
                       bool is_read )
            : has_ctx_ref( ctx ), irb( irb ),
              requester( requester ),
              is_read( is_read )
        {}

        template< typename S >
        llvm::Value *request( S &&arg )
        {
           auto identity = [ & ]( auto v ) { return v; };
           return requester.request( irb, std::forward< S >( arg ), is_read, identity );
        }

        template< bool is_signed, typename S >
        llvm::Value *request_word( S &&arg )
        {
            auto extend = [ & ]( llvm::Value *v )
            {
                if ( ctx.bw( v ) < ctx.ptr_size )
                {
                    if constexpr ( is_signed )
                        return irb.CreateSExt( v, ctx.word_type() );
                    else
                        return irb.CreateZExt( v, ctx.word_type() );
                }
                return v;
            };
            return requester.request( irb, std::forward< S >( arg ), is_read, extend );
        }

        llvm::Value *lift( const shadowinst::Operand &s_op,
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
                return ctx.zero();
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

            if ( s_reg->tm().is_saturated_by_zeroes() || s_reg->tm().empty() )
            {
                log_info() << "[operand-lifter]:" << "Register tm resolves as zero.";
                return ctx.zero();
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

}  // namespace circ::build
