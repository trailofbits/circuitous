/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Visitors.hpp>
#include <circuitous/Transforms/EGraph.hpp>

#include <spdlog/spdlog.h>

#include <unordered_map>

namespace circ
{
    struct NodeTemplateBuilder : NonDefaultingVisitor< NodeTemplateBuilder > {
        //
        // NodeTemplate constructors
        //
        OpCodeNode opcode(auto *op) const {
            return { op->op_code_str() };
        }

        SizedNode sized(auto *op) const {
            return { op->op_code_str(), op->size };
        }

        AdviceNode advice(Advice *op) const {
            return { op->op_code_str(), op->size, op->advice_idx };
        }

        RegisterNode regop(auto *op) const {
            return { op->op_code_str(), op->size, op->reg_name };
        }

        ConstantNode constop(Constant *op) const {
            return { op->op_code_str(), op->size, op->bits };
        }

        MemoryNode memop(Memory *op) const {
            return { op->op_code_str(), op->mem_idx };
        }

        ExtractNode extract(Extract *op) const {
            return { op->op_code_str(), op->low_bit_inc, op->high_bit_exc };
        }

        SelectNode select(Select *op) const {
            return { op->op_code_str(), op->size, op->bits };
        }

        //
        // Visits
        //
        NodeTemplate visit(InputRegister  *op);
        NodeTemplate visit(OutputRegister *op);

        NodeTemplate visit(InputTimestamp  *op);
        NodeTemplate visit(OutputTimestamp *op);
        NodeTemplate visit(InputErrorFlag  *op);
        NodeTemplate visit(OutputErrorFlag *op);

        NodeTemplate visit(Undefined *op);

        NodeTemplate visit(Memory *op);

        NodeTemplate visit(Constant *op);

        NodeTemplate visit(Advice *op);

        NodeTemplate visit(InputInstructionBits *op);

        NodeTemplate visit(RegConstraint *op);
        NodeTemplate visit(AdviceConstraint *op);
        NodeTemplate visit(WriteConstraint *op);
        NodeTemplate visit(ReadConstraint *op);
        NodeTemplate visit(UnusedConstraint *op);

        NodeTemplate visit(Add  *op);
        NodeTemplate visit(Sub  *op);
        NodeTemplate visit(Mul  *op);
        NodeTemplate visit(UDiv *op);
        NodeTemplate visit(SDiv *op);
        NodeTemplate visit(SRem *op);
        NodeTemplate visit(URem *op);

        NodeTemplate visit(Shl  *op);
        NodeTemplate visit(LShr  *op);
        NodeTemplate visit(AShr  *op);
        NodeTemplate visit(Trunc *op);
        NodeTemplate visit(ZExt  *op);
        NodeTemplate visit(SExt  *op);

        NodeTemplate visit(Icmp_ult *op);
        NodeTemplate visit(Icmp_slt *op);
        NodeTemplate visit(Icmp_ugt *op);
        NodeTemplate visit(Icmp_eq  *op);
        NodeTemplate visit(Icmp_ne  *op);
        NodeTemplate visit(Icmp_uge *op);
        NodeTemplate visit(Icmp_ule *op);
        NodeTemplate visit(Icmp_sgt *op);
        NodeTemplate visit(Icmp_sge *op);
        NodeTemplate visit(Icmp_sle *op);

        NodeTemplate visit(InputImmediate *op);

        NodeTemplate visit(Extract *op);

        NodeTemplate visit(Concat *op);

        NodeTemplate visit(PopulationCount *op);
        NodeTemplate visit(CountLeadingZeroes *op);
        NodeTemplate visit(CountTrailingZeroes *op);

        NodeTemplate visit(Not *op);

        NodeTemplate visit(Parity *op);

        NodeTemplate visit(Select *op);

        NodeTemplate visit(DecodeCondition *op);
        NodeTemplate visit(DecoderResult *op);
        NodeTemplate visit(VerifyInstruction *op);
        NodeTemplate visit(OnlyOneCondition *op);

        NodeTemplate visit(Or  *op);
        NodeTemplate visit(And *op);
        NodeTemplate visit(Xor *op);

        NodeTemplate visit(Circuit *);
    };

    using ENodePtr = CircuitENode::node_pointer;
    using NodesMap = std::unordered_map< Operation*, ENodePtr >;

    struct EGraphBuilderState {
        CircuitEGraph egraph;
        NodesMap nodes_map;
    };

    struct EGraphBuilder : NodeTemplateBuilder
    {
        using NodeTemplateBuilder::opcode;
        using NodeTemplateBuilder::dispatch;

        ENodeHandle add_nodes_recurse(Operation *op, EGraphBuilderState &state) {
            if (!state.nodes_map.contains(op)) {
                auto node = make_node(op, state);
                state.nodes_map[op] = node;
                for (const auto &child : op->operands) {
                    node->add_child( add_nodes_recurse(child, state) );
                }
            }

            return state.egraph.find(state.nodes_map[op]);
        }

        NodeTemplate make_template(Circuit *ci) { return opcode(ci); }

        NodeTemplate make_template(Operation *op) { return dispatch(op); }

        ENodePtr make_node(auto *op, EGraphBuilderState &state) {
            return state.egraph.add_node(make_template(op));
        }

        EGraphBuilderState build(Circuit *circuit) {
            EGraphBuilderState state;
            add_nodes_recurse(circuit, state);
            return state;
        }
    };

} // namespace circ
