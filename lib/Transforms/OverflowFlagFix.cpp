/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.hpp>
#include <circuitous/Transforms.hpp>

#include <algorithm>

namespace circ
{

    bool is_xor_with_two_lshr(Operation *op)
    {
        auto op_xor = dynamic_cast<Xor *>(op);
        if ( op_xor == nullptr )
            return false;

        return op_xor->operands_size() == 2 && isa< LShr >( op_xor->operand( 0 ) ) &&
               isa< LShr >( op_xor->operand( 1 ) );
    }

    std::string string_of_value_2(std::size_t bit_length)
    {
        if ( bit_length < 2 )
            circ::unreachable() << "Can't represent a string with less than 2 characters";

        return "01" + std::string( bit_length - 2, '0' );
    }

    bool has_remill_overflow_flag_semantics(RegConstraint *op)
    {
        if ( op->operands_size() != 2 || !isa< Icmp_eq >( op->operand( 0 ) ))
            return false;

        auto cmp = dynamic_cast<Icmp_eq *>(op->operand( 0 ));
        if ( cmp->operands_size() != 2 || !isa< Add >( cmp->operand( 0 ) ) ||
             !isa< Constant >( cmp->operand( 1 ) ))
            return false;

        auto constant = dynamic_cast<Constant *>(cmp->operand( 1 ));
        if ( constant->bits != string_of_value_2(32) && constant->bits != string_of_value_2(64) )
            return false;

        auto add = dynamic_cast<Add *>(cmp->operand( 0 ));
        if ( add->operands_size() != 2 )
            return false;

        return is_xor_with_two_lshr(add->operand(0)) &&  is_xor_with_two_lshr(add->operand(1));
    }

}  // namespace circ
