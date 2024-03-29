/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Verify.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <circuitous/IR/Shapes.hpp>

#include <circuitous/Run/Trace.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>
#include <circuitous/Util/Warnings.hpp>

#include <circuitous/Run/State.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/ADT/APInt.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ::run
{

    struct BaseSemantics
    {
        StateOwner *state = nullptr;
        Circuit *circuit = nullptr;

        BaseSemantics(StateOwner *state, Circuit *circuit)
            : state(state), circuit(circuit)
        {}

        BaseSemantics() = delete;

        template< typename ...Args >
        bool valid_values(Args &&... args)
        {
            return (has_value(args) && ...);
        }

        bool valid_children_values(Operation *op)
        {
            for (auto child : op->operands())
                if (!state->has_value(child) || !state->get_node_val(child))
                    return false;
            return true;
        }

        value_type undef() { return {}; }
        raw_value_type true_val() const { return llvm::APInt(1, 1); }
        raw_value_type false_val() const { return llvm::APInt(1, 0); }

        raw_value_type value(bool v) const { return (v) ? true_val() : false_val(); }

        raw_value_type zero(uint32_t size) { return llvm::APInt( size, 0, false ); }

        bool has_value(Operation *op) const
        {
            return state->has_value(op);
        }

        void set_node_val(Operation *op, const value_type &val)
        {
            check( state );
            state->set_node_val(op, val);
        }

        template< typename I > requires std::is_integral_v< I >
        auto set_node_val(Operation *op, I &&i)
        {
            return set_node_val(op, llvm::APInt(op->size, i, false));
        }

        auto get_node_val(Operation *op) const { return state->get_node_val(op); }
        auto get_node_val(Operation *op, std::size_t idx) const
        {
            return this->get_node_val(op->operand(idx));
        }

        template< typename Op, typename F >
        auto safe(Op *op, F &&f)
        {
            if (!valid_children_values(op))
                return set_node_val(op, undef());
            return set_node_val(op, f(op));
        }

        template<typename T>
        std::unordered_map<T *, value_type> get_derived() const
        {
            log_kill() << "BaseSemantics cannot export derived values.";
        }

        std::vector< Memory::Parsed > get_derived_mem()
        {
            log_kill() << "BaseSemantics cannot export derived mem.";
        }

        template< gap::ranges::range R >
        value_type compute_or( R from, raw_value_type init )
        {
            for ( auto op : from )
            {
                if ( !has_value( op ) )
                    return {};
                init |= *(this->get_node_val( op ));
            }
            return { init };
        }

        void visit(URem *)            { not_implemented(); }
        void visit(SRem *)            { not_implemented(); }
        void visit(circ::Memory *)    { not_implemented(); }
        void visit(InputTimestamp *)  { not_implemented(); }
        void visit(OutputTimestamp *) { not_implemented(); }

        void visit(Switch *) { not_implemented(); }
        void visit(Option *) { not_implemented(); }

        void visit(InputSyscallState *) { not_implemented(); }
        void visit(OutputSyscallState *) { not_implemented(); }

        void visit(InputSyscallReg *) { not_implemented(); }
        void visit(OutputSyscallReg *) { not_implemented(); }

        void visit(SyscallModule *) { not_implemented(); }

        void init() {};
    };

    // Tags that tells us about which visits are implemented by a layer
    // eventually we want to have all included to be able to interpret
    // the circuit properly.
    // Semantics of input/output nodes
    struct io_sem {};
    // Semantics of operations (addition, and, xor, ...)
    struct op_sem {};
    // Semantics of conditions (hint check, output check, ...)
    struct c_sem {};

    template< typename T, typename ... Kinds >
    static inline constexpr bool valid_interpreter_()
    {
        return (std::is_base_of_v<Kinds, T> && ...);
    }

    template< typename T >
    static inline constexpr bool valid_interpreter()
    {
        return valid_interpreter_<T, io_sem, op_sem, c_sem>();
    }

    template< typename Next >
    struct OpSem : Next, op_sem
    {
        using Next::visit;
        using Next::safe;

        using Next::Next;

        // Constant semantics
        void visit(Constant *op);
        void visit(Undefined *op);

        // Operations
        void visit(Concat *op);
        void visit(Extract *op);
        void visit(Not *op);
        void visit(Select *op);
        void visit(Parity *op);
        void visit(PopulationCount *op);
        void visit(CountLeadingZeroes *op)
        {
            safe(op, [&](auto o){ return this->get_node_val(o, 0)->countLeadingZeros(); } );
        }
        void visit(CountTrailingZeroes *op)
        {
            safe(op, [&](auto o){ return this->get_node_val(o, 0)->countTrailingZeros(); } );
        }
        void visit(Or *op);
        void visit(And *op);
        void visit(DecoderResult *op);

        // Must be called in `safe` context.
        auto lhs(Operation *op) { return *this->get_node_val(op, 0); }
        auto rhs(Operation *op) { return *this->get_node_val(op, 1); }
        bool is_zero(const llvm::APInt &i) { return i.isNullValue(); }

        void visit(Add *op) { safe(op, [&](auto o){ return lhs(o) + rhs(o); } ); }
        void visit(Sub *op) { safe(op, [&](auto o){ return lhs(o) - rhs(o); } ); }
        void visit(Mul *op) { safe(op, [&](auto o){ return lhs(o) * rhs(o); } ); }

        void visit(UDiv *op)
        {
            auto div = [&](auto o)
            {
                return (is_zero(rhs(o))) ? rhs(o) : std::make_optional( lhs(o).udiv(rhs(o)) );
            };
            safe(op, div);
        }
        void visit(SDiv *op)
        {
            auto div = [&](auto o)
            {
                return (is_zero(rhs(o))) ? rhs(o) : std::make_optional( lhs(o).sdiv(rhs(o)) );
            };
            safe(op, div);
        }

        void visit(SRem *op)
        {
            auto rem = [&](auto o)
            {
                return (is_zero(rhs(o))) ? rhs(o) : std::make_optional( lhs(o).srem(rhs(o)) );
            };
            safe(op, rem);
        }
        void visit(URem *op)
        {
            auto rem = [&](auto o)
            {
                return (is_zero(rhs(o))) ? rhs(o) : std::make_optional( lhs(o).urem(rhs(o)) );
            };
            safe(op, rem);
        }

        void visit(Xor *op) { safe(op, [&](auto o){ return lhs(o) ^ rhs(o); } ); }

        void visit(Shl *op) { safe(op, [&](auto o){ return lhs(o) << rhs(o); } ); }
        void visit(LShr *op) { safe(op, [&](auto o){ return lhs(o).lshr(rhs(o)); } ); }
        void visit(AShr *op) { safe(op, [&](auto o){ return lhs(o).ashr(rhs(o)); } ); }

        void visit(Trunc *op)
        {
            safe(op, [&](auto o){ return lhs(o).trunc(o->size); } );
        }
        void visit(ZExt *op) { safe(op, [&](auto o){ return lhs(o).zext(o->size);   } ); }
        void visit(SExt *op) { safe(op, [&](auto o){ return lhs(o).sext(o->size);  } ); }

        auto bv(bool b) { return this->value(b); }

        void visit(Icmp_ult *op) { safe(op, [&](auto o){ return bv(lhs(o).ult(rhs(o))); } ); }
        void visit(Icmp_slt *op) { safe(op, [&](auto o){ return bv(lhs(o).slt(rhs(o))); } ); }
        void visit(Icmp_ugt *op) { safe(op, [&](auto o){ return bv(lhs(o).ugt(rhs(o))); } ); }

        void visit(Icmp_uge *op) { safe(op, [&](auto o){ return bv(lhs(o).uge(rhs(o))); } ); }
        void visit(Icmp_ule *op) { safe(op, [&](auto o){ return bv(lhs(o).ule(rhs(o))); } ); }
        void visit(Icmp_sgt *op) { safe(op, [&](auto o){ return bv(lhs(o).sgt(rhs(o))); } ); }
        void visit(Icmp_sge *op) { safe(op, [&](auto o){ return bv(lhs(o).sge(rhs(o))); } ); }
        void visit(Icmp_sle *op) { safe(op, [&](auto o){ return bv(lhs(o).sle(rhs(o))); } ); }

        void visit(Icmp_eq *op) { safe(op, [&](auto o){ return bv(lhs(o) == rhs(o)); } ); }
        void visit(Icmp_ne *op) { safe(op, [&](auto o){ return bv(lhs(o) != rhs(o)); } ); }

        void visit( Switch * );
        void visit( Option * );
    };

    #include "Base.tpp"

    template< typename Next >
    struct Derive : Next
    {
        using Next::safe;
        using Next::visit;

        using Next::Next;
        using Next::get_node_val;

        std::unordered_map< Operation *, Operation * > cond_to_value;

        void derive_cond(Operation *op);
        void derive_cond(ReadConstraint *op);
        void derive_cond(WriteConstraint *op);
        void derive_cond(UnusedConstraint *op);

        void to_derive(Operation *op, Operation *define_cond)
        {
            check(!cond_to_value.count(define_cond));
            cond_to_value[ define_cond ] = op;
        }

        template< typename T >
        void handle_cond(T *op)
        {
            if (cond_to_value.count(op))
                return derive_cond(op);
            return Next::visit(op);
        }

        template< typename T >
        auto get_derived() const
        {
            std::unordered_map< T *, value_type > out;
            for (auto [_, op] : cond_to_value)
                if (op->op_code == T::kind)
                    out[dynamic_cast< T * >(op)] = this->get_node_val(op);
            return out;
        }

        void visit(AdviceConstraint *op){ return handle_cond( op ); }
        void visit(ReadConstraint *op)  { return handle_cond( op ); }
        void visit(RegConstraint *op)   { return handle_cond( op ); }
        void visit(WriteConstraint *op) { return handle_cond( op ); }
        void visit(UnusedConstraint *op){ return handle_cond( op ); }
    };

    template< typename Next >
    struct IOSem : Next, io_sem
    {
        using Next::safe;
        using Next::visit;

        using Next::Next;

        // Input semantics
        void visit(InputRegister *op);
        void visit(InputImmediate *op);
        void visit(InputErrorFlag *op);
        void visit(InputInstructionBits *op);

        // Output semantics
        void visit(OutputRegister *op);
        void visit(OutputErrorFlag *op);
        void visit(Advice *op);
    };

    template< typename Next >
    struct CSem : Next, c_sem
    {
        using Next::safe;
        using Next::visit;

        using Next::Next;

        // Condition semantics
        void visit(DecodeCondition *op);
        void visit(RegConstraint *op);
        void visit(AdviceConstraint *op);

        void visit(VerifyInstruction *op);
        void visit(OnlyOneCondition *op);

        void visit(ReadConstraint *op);
        void visit(WriteConstraint *op);
        void visit(UnusedConstraint *op);
    };

    template< typename N >
    using SemBase = IOSem< CSem< OpSem< N > > >;

    #include <circuitous/Run/Derive.tpp>

    template< typename N >
    struct SemanticsAdapter : N, NonDefaultingVisitor< SemanticsAdapter< N > >
    {
        using N::visit;
        using N::N;
        using NonDefaultingVisitor< SemanticsAdapter< N > >::dispatch;
    };

    using Base = SemanticsAdapter< Derive< SemBase< BaseSemantics > > >;
    using verifier_semantics = SemanticsAdapter< SemBase< BaseSemantics > >;

} // namespace circ::run
