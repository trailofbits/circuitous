/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.hpp>
#include <circuitous/Transforms.hpp>

#include <algorithm>

namespace circ {

bool has_remill_overflow_flag_semantics(RegConstraint* op){
    if(op->operands.size() != 2 || !isa<Icmp_eq>(op->operands[0])){
        return false;
    }
    auto cmp = dynamic_cast<Icmp_eq*>(op->operands[0]);
    if(cmp->operands.size() != 2 || !isa<Add>(cmp->operands[0]) || !isa<Constant>(cmp->operands[1]))
        return false;

    auto constant = dynamic_cast<Constant*>(cmp->operands[1]);
    auto str_2_32_bit = "01" + std::string(30, '0');
    auto str_2_64_bit = "01" + std::string(62, '0');
    if(constant->bits != str_2_32_bit && constant->bits != str_2_64_bit )
        return false;

    auto add = dynamic_cast<Add*>(cmp->operands[0]);
    if(add->operands.size() != 2 || !isa<Xor>(add->operands[0]) || !isa<Xor>(add->operands[1]))
        return false;

    auto xor1 = dynamic_cast<Xor*>(add->operands[0]);
    auto xor2 = dynamic_cast<Xor*>(add->operands[1]);
    return xor1->operands.size() == 2 && isa<LShr>(xor1->operands[0]) && isa<LShr>(xor1->operands[1]) &&
      xor2->operands.size() == 2 && isa<LShr>(xor2->operands[0]) && isa<LShr>(xor2->operands[1]);
}

}  // namespace circ
