/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Transforms/EGraphBuilder.hpp>

namespace circ
{
    //
    // Visits
    //
    node_template node_template_builder::visit(InputRegister *op) {
        return regop(op);
    }

    node_template node_template_builder::visit(OutputRegister *op) {
        return regop(op);
    }

    node_template node_template_builder::visit(InputTimestamp *op) {
        return sized(op);
    }

    node_template node_template_builder::visit(OutputTimestamp *op) {
        return sized(op);
    }

    node_template node_template_builder::visit(InputErrorFlag *op) {
        return sized(op);
    }

    node_template node_template_builder::visit(OutputErrorFlag *op) {
        return sized(op);
    }

    node_template node_template_builder::visit(Undefined *op) {
        return sized(op);
    }

    node_template node_template_builder::visit(Memory *op) {
        return memop(op);
    }

    node_template node_template_builder::visit(Constant *op) {
        return constop(op);
    }

    node_template node_template_builder::visit(Advice *op) {
        return advice(op);
    }

    node_template node_template_builder::visit(InputInstructionBits *op) {
        return sized(op);
    }

    node_template node_template_builder::visit(RegConstraint *op) {
        return opcode(op);
    }

    node_template node_template_builder::visit(AdviceConstraint *op) {
        return opcode(op);
    }

    node_template node_template_builder::visit(WriteConstraint *op) {
        return opcode(op);
    }

    node_template node_template_builder::visit(ReadConstraint *op) {
        return opcode(op);
    }

    node_template node_template_builder::visit(UnusedConstraint *op) {
        return opcode(op);
    }

    node_template node_template_builder::visit(Add *op)  { return sized(op); }
    node_template node_template_builder::visit(Sub *op)  { return sized(op); }
    node_template node_template_builder::visit(Mul *op)  { return sized(op); }
    node_template node_template_builder::visit(UDiv *op) { return sized(op); }
    node_template node_template_builder::visit(SDiv *op) { return sized(op); }
    node_template node_template_builder::visit(SRem *op) { return sized(op); }
    node_template node_template_builder::visit(URem *op) { return sized(op); }

    node_template node_template_builder::visit(Shl *op)   { return sized(op); }
    node_template node_template_builder::visit(LShr *op)  { return sized(op); }
    node_template node_template_builder::visit(AShr *op)  { return sized(op); }
    node_template node_template_builder::visit(Trunc *op) { return sized(op); }
    node_template node_template_builder::visit(ZExt *op)  { return sized(op); }
    node_template node_template_builder::visit(SExt *op)  { return sized(op); }

    node_template node_template_builder::visit(Icmp_ult *op) { return sized(op); }
    node_template node_template_builder::visit(Icmp_slt *op) { return sized(op); }
    node_template node_template_builder::visit(Icmp_ugt *op) { return sized(op); }
    node_template node_template_builder::visit(Icmp_eq *op)  { return sized(op); }
    node_template node_template_builder::visit(Icmp_ne *op)  { return sized(op); }
    node_template node_template_builder::visit(Icmp_uge *op) { return sized(op); }
    node_template node_template_builder::visit(Icmp_ule *op) { return sized(op); }
    node_template node_template_builder::visit(Icmp_sgt *op) { return sized(op); }
    node_template node_template_builder::visit(Icmp_sge *op) { return sized(op); }
    node_template node_template_builder::visit(Icmp_sle *op) { return sized(op); }

    node_template node_template_builder::visit(InputImmediate *op) {
        return sized(op);
    }

    node_template node_template_builder::visit(Extract *op) {
        return extract(op);
    }

    node_template node_template_builder::visit(Concat *op) {
        return sized(op);
    }

    node_template node_template_builder::visit(PopulationCount *op) {
        return sized(op);
    }

    node_template node_template_builder::visit(CountLeadingZeroes *op) {
        return sized(op);
    }

    node_template node_template_builder::visit(CountTrailingZeroes *op) {
        return sized(op);
    }

    node_template node_template_builder::visit(Not *op) {
        return sized(op);
    }

    node_template node_template_builder::visit(Parity *op) {
        return opcode(op);
    }

    node_template node_template_builder::visit(Select *op) {
        return select(op);
    }

    node_template node_template_builder::visit(DecodeCondition *op) {
        return opcode(op);
    }

    node_template node_template_builder::visit(DecoderResult *op) {
        return opcode(op);
    }

    node_template node_template_builder::visit(VerifyInstruction *op) {
        return opcode(op);
    }

    node_template node_template_builder::visit(OnlyOneCondition *op) {
        return opcode(op);
    }

    node_template node_template_builder::visit(Or *op)  { return sized(op); }
    node_template node_template_builder::visit(And *op) { return sized(op); }
    node_template node_template_builder::visit(Xor *op) { return sized(op); }

    node_template node_template_builder::visit(Switch *op) { return sized(op); }
    node_template node_template_builder::visit(Option *op) { return sized(op); }

} // namespace circ
