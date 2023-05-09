/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Memory.hpp>
#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Visitors.hpp>

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Check.hpp>


CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <sstream>
#include <unordered_map>

namespace circ
{

    std::string Operation::name() const
    {
        unreachable() << util::to_underlying(op_code)
                      << " does not provide name() method override.";
    }

    std::string Constant::name() const
    {
        std::stringstream ss;
        ss << "CONST_" << size << "_";
        for (auto i = 0U; i < size; ++i)
            ss << bits[size - i - 1];
        return ss.str();
    }

    uint32_t Memory::expected_size(uint32_t ptr_size)
    {
        return irops::memory::size(ptr_size);
    }

    std::optional< DecoderResult * > VerifyInstruction::decoder()
    {
        for ( auto op : this->operands() )
            if ( auto decoder_res = dyn_cast< DecoderResult >( op ) )
                return decoder_res;
        return {};
    }


    bool Select::is_extension_of( const Select *other ) const
    {
        if ( operands_size() < other->operands_size() )
            return false;

        return other->can_be_extended_to( this );
    }

    bool Select::can_be_extended_to( const Select *other ) const
    {
        if ( operands_size() > other->operands_size() )
            return false;

        auto size = other->operands_size();

        // Correctly select operand (wrt to stride).
        auto op = [ & ]( auto from, auto i )
        {
            auto total = from->operands_size();
            auto idx = i + ( i - 1 ) * ( ( total - 1 ) / ( size - 1 ) - 1);
            return from->operand( idx );
        };

        auto is_undef = []( auto x ) { return isa< Undefined >( x ); };

        for ( std::size_t i = 1; i < size; ++i )
        {
            auto small = op( this, i );
            auto big = op( other, i );

            // Undefs can be freely changed as we expect them to be
            // "invalid execution paths" anyway.
            if ( is_undef( small ) )
                continue;

            if ( small != big )
            {
                return false;
            }
        }
        return true;
    }

    struct AdviceValueVisitor : UniqueVisitor< AdviceValueVisitor >
    {
        Advice *advice;
        Operation *result = nullptr;

        explicit AdviceValueVisitor( Advice *advice ) : advice( advice ) { }
        void visit( AdviceConstraint *ac )
        {
            check( ac->operands_size() == 2 )
                << "advice constraint does not contain 2 children";
            check( ac->operand( 0 ) != ac->operand( 1 ) )
                << "advice constraint points to same child twice, left id:"
                << ac->operand( 0 )->id() << ", id right:" << ac->operand( 1 )->id();

            if ( ac->operand( 0 ) == advice )
                result = ac->operand( 1 );

            if ( ac->operand( 1 ) == advice )
                result = ac->operand( 0 );

        }

        void visit( Operation *op ) { op->traverse( *this ); }
    };

    Operation *Advice::value( VerifyInstruction *vi )
    {
        AdviceValueVisitor vis( this );
        vi->traverse( vis );
        check( vis.result != nullptr ) << "could not find value";
        check( vis.result != this ) << "returned advice to itself";
        check( isa< Advice >( vis.result ) == false ) << "transitive advice found";
        return vis.result;
    }

}  // namespace circ
