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
    struct node_template_builder : NonDefaultingVisitor< node_template_builder > {
        //
        // node_template constructors
        //
        op_code_node  opcode(auto *op) const;
        sized_node    sized(auto *op) const;
        advice_node   advice(Advice *op) const;
        register_node regop(auto *op) const;
        constant_node constop(Constant *op) const;
        memory_node   memop(Memory *op) const;
        extract_node  extract(Extract *op) const;
        select_node   select(Select *op) const;

        //
        // Visits
        //
        node_template visit(InputRegister  *op);
        node_template visit(OutputRegister *op);

        node_template visit(InputTimestamp  *op);
        node_template visit(OutputTimestamp *op);
        node_template visit(InputErrorFlag  *op);
        node_template visit(OutputErrorFlag *op);

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

        node_template visit(Circuit *);
    };

    using nodes_map = std::unordered_map< Operation*, circuit_enode::node_pointer >;

    struct egraph_builder_state {
        circuit_egraph egraph;
        nodes_map nodes_map;
    };

    struct circuit_egraph_builder : node_template_builder
    {
        using node_template_builder::opcode;
        using node_template_builder::dispatch;

        enode_handle add_nodes_recurse(Operation *op, egraph_builder_state &state) {
            if (!state.nodes_map.contains(op)) {
                auto node = make_node(op, state);
                state.nodes_map[op] = node;
                for (const auto &child : op->operands) {
                    // TODO fix parents
                    node->add_child( add_nodes_recurse(child, state) );
                }
            }

            return state.egraph.find(state.nodes_map[op]);
        }

        node_template make_template(Circuit *ci) { return opcode(ci); }

        node_template make_template(Operation *op) { return dispatch(op); }

        circuit_enode::node_pointer make_node(auto *op, egraph_builder_state &state) {
            return state.egraph.add_node(make_template(op));
        }

        egraph_builder_state build(Circuit *circuit) {
            egraph_builder_state state;
            add_nodes_recurse(circuit, state);
            return state;
        }
    };

} // namespace circ
