/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Lifter/Instruction.hpp>
#include <circuitous/Lifter/Component.hpp>

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

        llvm::Value *make_select( llvm::IRBuilder<> &irb, const shadowinst::TM_t &tm,
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

    struct OperandLifter : has_ctx_ref
    {
        llvm::IRBuilder<> &irb;
        OperandSelection::Requester read_requester;
        OperandSelection::Requester write_requester;
        // TODO: Not sure how to do this clenaer, the whole thing is stateful anyway.
        bool is_read = false;

        // We may need to access some global information when lifting specific
        // operands.
        const InstructionInfo &info;

        OperandLifter( CtxRef ctx,
                       llvm::IRBuilder<> &irb, OperandSelection &selection,
                       const InstructionInfo &info )
            : has_ctx_ref( ctx ),
              irb( irb ),
              read_requester( selection, true ), write_requester( selection, false ),
              info( info )
        {}

        values_t lift() &&;
        llvm::Value *request( const shadowinst::TM_t &tm );

        llvm::Value *lift( const shadowinst::Operand &s_op, const remill::Operand &r_op );

        llvm::Value *lift( const shadowinst::Reg &s_reg,
                           const remill::Operand::Register &r_reg );

        llvm::Value *lift( const shadowinst::Immediate &s_imm );

        llvm::Value *lift( const shadowinst::Address &s_addr,
                           const remill::Operand::Address &r_addr );
    };

}  // namespace circ::build
