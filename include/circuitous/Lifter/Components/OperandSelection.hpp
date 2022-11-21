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
        value_gen_t request( llvm::IRBuilder<> &irb, std::size_t idx,
                             std::size_t count, bool is_read );

        std::size_t match( const shadowinst::TM_t &other )
        {
            for ( std::size_t i = 0; i < saturated.size(); ++i )
                if ( other.is_subset_of( saturated[ i ] ) )
                    return i;
            log_fatal() << "Was not able to recover select template!";
        }

        // Does not allow additions once this is build.
        // TODO(lifter): Currently there is no extra value in allowing to add extras later.
        static OperandSelection build( CtxRef ctx, const InstructionBatch &batch );

        lifted_operands_t assign( llvm::IRBuilder<> &irb, InstructionInfo &info );

        shadowinst::TM_t operator[]( std::size_t idx ) const
        {
            check( idx < saturated.size() );
            return saturated[ idx ];
        }

        std::string to_string() const;
    };

    struct OperandLifted
    {
        llvm::IRBuilder<> &irb;
        OperandSelection &pristines;
    };

}  // namespace circ::build
