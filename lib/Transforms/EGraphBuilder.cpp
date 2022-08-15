/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Transforms/EGraphBuilder.hpp>

namespace circ
{
    //
    // Visits
    //
    NodeTemplate NodeTemplateBuilder::visit(InputRegister *op) {
        return regop(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(OutputRegister *op) {
        return regop(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(InputTimestamp *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(OutputTimestamp *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(InputErrorFlag *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(OutputErrorFlag *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(Undefined *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(Memory *op) {
        return memop(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(Constant *op) {
        return constop(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(Advice *op) {
        return advice(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(InputInstructionBits *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(RegConstraint *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(AdviceConstraint *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(WriteConstraint *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(ReadConstraint *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(UnusedConstraint *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(Add *op)  { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(Sub *op)  { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(Mul *op)  { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(UDiv *op) { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(SDiv *op) { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(SRem *op) { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(URem *op) { return sized(op); }

    NodeTemplate NodeTemplateBuilder::visit(Shl *op)   { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(LShr *op)  { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(AShr *op)  { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(Trunc *op) { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(ZExt *op)  { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(SExt *op)  { return sized(op); }

    NodeTemplate NodeTemplateBuilder::visit(Icmp_ult *op) { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(Icmp_slt *op) { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(Icmp_ugt *op) { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(Icmp_eq *op)  { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(Icmp_ne *op)  { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(Icmp_uge *op) { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(Icmp_ule *op) { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(Icmp_sgt *op) { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(Icmp_sge *op) { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(Icmp_sle *op) { return sized(op); }

    NodeTemplate NodeTemplateBuilder::visit(InputImmediate *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(Extract *op) {
        return extract(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(Concat *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(PopulationCount *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(CountLeadingZeroes *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(CountTrailingZeroes *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(Not *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(Parity *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(Select *op) {
        return select(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(DecodeCondition *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(DecoderResult *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(VerifyInstruction *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(OnlyOneCondition *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBuilder::visit(Or *op)  { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(And *op) { return sized(op); }
    NodeTemplate NodeTemplateBuilder::visit(Xor *op) { return sized(op); }

    NodeTemplate NodeTemplateBuilder::visit(Circuit *op) { return opcode(op); }

} // namespace circ
