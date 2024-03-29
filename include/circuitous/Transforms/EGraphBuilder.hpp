/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Visitors.hpp>
#include <circuitous/Transforms/EGraph.hpp>

#include <eqsat/pattern/pattern.hpp>

#include <spdlog/spdlog.h>

#include <unordered_map>

namespace circ
{
    struct node_template_builder
        : NonDefaultingVisitor< node_template_builder >
        , circuit_egraph_builder_base
    {
        //
        // node_template constructors
        //
        using circuit_egraph_builder_base::opcode;
        using circuit_egraph_builder_base::sized;
        using circuit_egraph_builder_base::advice;
        using circuit_egraph_builder_base::regop;
        using circuit_egraph_builder_base::constop;
        using circuit_egraph_builder_base::memop;
        using circuit_egraph_builder_base::extract;
        using circuit_egraph_builder_base::select;

        //
        // Visits
        //
        node_template visit(InputRegister  *op);
        node_template visit(OutputRegister *op);

        node_template visit(InputTimestamp  *op);
        node_template visit(OutputTimestamp *op);
        node_template visit(InputErrorFlag  *op);
        node_template visit(OutputErrorFlag *op);

        node_template visit(OutputSyscallState *op);
        node_template visit(InputSyscallState *op);

        node_template visit(OutputSyscallReg *op);
        node_template visit(InputSyscallReg *op);

        node_template visit(SyscallModule *op);

        node_template visit(Undefined *op);

        node_template visit(Memory *op);

        node_template visit(Constant *op);

        node_template visit(Advice *op);

        node_template visit(InputInstructionBits *op);

        node_template visit(RegConstraint *op);
        node_template visit(AdviceConstraint *op);
        node_template visit(WriteConstraint *op);
        node_template visit(ReadConstraint *op);
        node_template visit(UnusedConstraint *op);

        node_template visit(Add  *op);
        node_template visit(Sub  *op);
        node_template visit(Mul  *op);
        node_template visit(UDiv *op);
        node_template visit(SDiv *op);
        node_template visit(SRem *op);
        node_template visit(URem *op);

        node_template visit(Shl  *op);
        node_template visit(LShr  *op);
        node_template visit(AShr  *op);
        node_template visit(Trunc *op);
        node_template visit(ZExt  *op);
        node_template visit(SExt  *op);

        node_template visit(Icmp_ult *op);
        node_template visit(Icmp_slt *op);
        node_template visit(Icmp_ugt *op);
        node_template visit(Icmp_eq  *op);
        node_template visit(Icmp_ne  *op);
        node_template visit(Icmp_uge *op);
        node_template visit(Icmp_ule *op);
        node_template visit(Icmp_sgt *op);
        node_template visit(Icmp_sge *op);
        node_template visit(Icmp_sle *op);

        node_template visit(InputImmediate *op);

        node_template visit(Extract *op);

        node_template visit(Concat *op);

        node_template visit(PopulationCount *op);
        node_template visit(CountLeadingZeroes *op);
        node_template visit(CountTrailingZeroes *op);

        node_template visit(Not *op);

        node_template visit(Parity *op);

        node_template visit(Select *op);

        node_template visit(DecodeCondition *op);
        node_template visit(DecoderResult *op);
        node_template visit(VerifyInstruction *op);
        node_template visit(OnlyOneCondition *op);

        node_template visit(Or  *op);
        node_template visit(And *op);
        node_template visit(Xor *op);

        node_template visit(Switch *);
        node_template visit(Option *);
    };

    using nodes_map = std::unordered_map< Operation*, enode_handle >;

    struct egraph_builder_state {
        circuit_egraph egraph;
        nodes_map nodes_map;
    };

    struct circuit_egraph_builder : node_template_builder
    {
        using node_template_builder::opcode;
        using node_template_builder::dispatch;
        using operation = Operation *;

        enode_handle add_nodes_recurse(operation op, egraph_builder_state &state) {
            auto &nodes = state.nodes_map;
            if (auto it = nodes.find(op); it != nodes.end()) {
                return it->second;
            }

            std::vector< enode_handle > children;
            for (const auto &child : op->operands()) {
                children.push_back(add_nodes_recurse(child, state));
            }

            auto node = state.egraph.insert(make_template(op), children);
            nodes.emplace(op, node);
            return node;
        }

        node_template make_template(operation op) { return dispatch(op); }

        egraph_builder_state build(circuit_ref_t circuit) {
            egraph_builder_state state;
            add_nodes_recurse(circuit->root, state);
            return state;
        }
    };

} // namespace circ
