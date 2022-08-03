/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Transforms/EGraphBuilder.hpp>

namespace circ::eqsat
{
    //
    // Visits
    //
    NodeTemplate NodeTemplateBulder::visit(InputRegister *op) {
        return regop(op);
    }

    NodeTemplate NodeTemplateBulder::visit(OutputRegister *op) {
        return regop(op);
    }

    NodeTemplate NodeTemplateBulder::visit(InputTimestamp *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBulder::visit(OutputTimestamp *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBulder::visit(InputErrorFlag *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBulder::visit(OutputErrorFlag *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBulder::visit(Undefined *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBulder::visit(Memory *op) {
        return memop(op);
    }

    NodeTemplate NodeTemplateBulder::visit(Constant *op) {
        return constop(op);
    }

    NodeTemplate NodeTemplateBulder::visit(Advice *op) {
        return advice(op);
    }

    NodeTemplate NodeTemplateBulder::visit(InputInstructionBits *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBulder::visit(RegConstraint *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBulder::visit(AdviceConstraint *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBulder::visit(WriteConstraint *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBulder::visit(ReadConstraint *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBulder::visit(UnusedConstraint *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBulder::visit(Add *op)  { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(Sub *op)  { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(Mul *op)  { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(UDiv *op) { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(SDiv *op) { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(SRem *op) { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(URem *op) { return sized(op); }

    NodeTemplate NodeTemplateBulder::visit(Shl *op)   { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(LShr *op)  { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(AShr *op)  { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(Trunc *op) { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(ZExt *op)  { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(SExt *op)  { return sized(op); }

    NodeTemplate NodeTemplateBulder::visit(Icmp_ult *op) { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(Icmp_slt *op) { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(Icmp_ugt *op) { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(Icmp_eq *op)  { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(Icmp_ne *op)  { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(Icmp_uge *op) { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(Icmp_ule *op) { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(Icmp_sgt *op) { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(Icmp_sge *op) { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(Icmp_sle *op) { return sized(op); }

    NodeTemplate NodeTemplateBulder::visit(InputImmediate *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBulder::visit(Extract *op) {
        return extract(op);
    }

    NodeTemplate NodeTemplateBulder::visit(Concat *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBulder::visit(PopulationCount *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBulder::visit(CountLeadingZeroes *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBulder::visit(CountTrailingZeroes *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBulder::visit(Not *op) {
        return sized(op);
    }

    NodeTemplate NodeTemplateBulder::visit(Parity *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBulder::visit(Select *op) {
        return select(op);
    }

    NodeTemplate NodeTemplateBulder::visit(DecodeCondition *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBulder::visit(DecoderResult *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBulder::visit(VerifyInstruction *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBulder::visit(OnlyOneCondition *op) {
        return opcode(op);
    }

    NodeTemplate NodeTemplateBulder::visit(Or *op)  { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(And *op) { return sized(op); }
    NodeTemplate NodeTemplateBulder::visit(Xor *op) { return sized(op); }

    NodeTemplate NodeTemplateBulder::visit(Circuit *) {
        unreachable() << "Unexpected case encountered in visit.";
    }

} // namespace circ::eqsat
