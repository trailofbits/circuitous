/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Transforms/PassBase.hpp>
#include <circuitous/Transforms/EGraph.hpp>

#include <gap/core/graph.hpp>
#include <gap/core/concepts.hpp>
#include <gap/core/memoize.hpp>

#include <eqsat/core/cost_graph.hpp>

#include <unordered_map>

namespace circ {

    template< gap::graph::graph_like base_graph, typename cost_function_t >
    struct extract_circuit_from_egraph {
        using optimal_graph = eqsat::optimal_graph_view< base_graph, cost_function_t >;
        using optimal_node  = typename optimal_graph::optimal_node;
        using node_pointer = typename optimal_graph::node_pointer;
        using operation = Operation *;

        explicit extract_circuit_from_egraph(const optimal_graph &graph)
            : circuit(nullptr), graph(graph)
        {}

        circuit_owner_t extract(enode_handle root_handle, std::uint32_t ptr_size) {
            spdlog::debug("[eqsat] start extraction");
            circuit = std::make_unique< Circuit >(ptr_size);
            circuit->root = extract(graph.node(root_handle));
            spdlog::debug("[eqsat] end extraction");
            return std::move(circuit);
        }

        using storage_type = eqsat::graph::storage_node< node_template >;

        const node_template& unwrap(const auto &node) {
            return std::visit( gap::overloaded {
                [] (const storage_type &node) -> const node_template& { return node; },
                [] (const auto &) -> const node_template& { log_kill() << "node without data\n"; }
            }, node);
        }

        template< typename ...variants >
        bool holds_alternative(const auto &v) {
            return (std::holds_alternative< variants >(v) || ...);
        }

        bool needs_compute_bitwidth(const node_template &op) {
            return holds_alternative< sized_node, advice_node >(op) && !bitwidth(op).has_value();
        }

        operation extract(const optimal_node &node) {
            auto node_ptr = node.node;
            if (cached.count(node_ptr)) {
                return cached.at(node_ptr);
            }

            const auto &data = unwrap(node_ptr->data);

            if (needs_compute_bitwidth(data)) {
                log_kill() << "Not implemented: update bitwidths of " << node_name(data);
            }

            auto op = make_operation(data);
            cached.emplace(node_ptr, op);

            for (const auto &ch : node.children()) {
                op->add_operand(extract(ch));
            }

            return op;
        }

        operation make_operation(const op_code_node &op) {
            return llvm::StringSwitch< operation >(op.op_code_name)
                .Case("register_constraint",  circuit->create< RegConstraint >())
                .Case("advice_constraint",    circuit->create< AdviceConstraint >())
                .Case("write_constraint",     circuit->create< WriteConstraint >())
                .Case("read_constraint",      circuit->create< ReadConstraint >())
                .Case("unused_constraint",    circuit->create< UnusedConstraint >())

                .Case("parity",               circuit->create< Parity >())

                .Case("DecodeCondition",      circuit->create< DecodeCondition >())
                .Case("DecoderResult",        circuit->create< DecoderResult >())
                .Case("VerifyInstruction",    circuit->create< VerifyInstruction >())
                .Case("OnlyOneCondition",     circuit->create< OnlyOneCondition >())

                .Case("DecoderResult", circuit->create< DecoderResult >())

                .Default( nullptr );
        }

        operation make_operation(const sized_node &op) {
            check(op.size.has_value());
            auto size = op.size.value();
            return llvm::StringSwitch< Operation* >(op.op_code_name)
                .Case("in.timestamp",     circuit->create< InputTimestamp >( size ))
                .Case("out.timestamp",    circuit->create< OutputTimestamp >( size ))
                .Case("in.error_flag",    circuit->create< InputErrorFlag >( size ))
                .Case("out.error_flag",   circuit->create< OutputErrorFlag >( size ))
                .Case("undefined",        circuit->create< Undefined >( size ))
                .Case("instruction_bits", circuit->create< InputInstructionBits >( size ))

                .Case("Add",   circuit->create< Add >( size ))
                .Case("Sub",   circuit->create< Sub >( size ))
                .Case("Mul",   circuit->create< Mul >( size ))
                .Case("UDiv",  circuit->create< UDiv >( size ))
                .Case("SDiv",  circuit->create< SDiv >( size ))
                .Case("URem",  circuit->create< URem >( size ))
                .Case("Xor",  circuit->create< Xor >( size ))
                .Case("SRem",  circuit->create< SRem >( size ))
                .Case("Shl",   circuit->create< Shl >( size ))
                .Case("LShr",  circuit->create< LShr >( size ))
                .Case("AShr",  circuit->create< AShr >( size ))
                .Case("Trunc", circuit->create< Trunc >( size ))
                .Case("ZExt",  circuit->create< ZExt >( size ))
                .Case("SExt",  circuit->create< SExt >( size ))

                .Case("Icmp_ult", circuit->create< Icmp_ult >( size ))
                .Case("Icmp_slt", circuit->create< Icmp_slt >( size ))
                .Case("Icmp_ugt", circuit->create< Icmp_ugt >( size ))
                .Case("Icmp_eq",  circuit->create< Icmp_eq >( size ))
                .Case("Icmp_ne",  circuit->create< Icmp_ne >( size ))
                .Case("Icmp_uge", circuit->create< Icmp_uge >( size ))
                .Case("Icmp_ule", circuit->create< Icmp_ule >( size ))
                .Case("Icmp_sgt", circuit->create< Icmp_sgt >( size ))
                .Case("Icmp_sge", circuit->create< Icmp_sge >( size ))
                .Case("Icmp_sle", circuit->create< Icmp_sle >( size ))

                .Case("input_immediate", circuit->create< InputImmediate >( size ))

                .Case("concat", circuit->create< Concat >( size ))

                .Case("Or",                    circuit->create< Or >( size ))
                .Case("And",                   circuit->create< And >( size ))
                .Case("Xor",                   circuit->create< Xor >( size ))

                .Case("pop_count",             circuit->create< PopulationCount >( size ))
                .Case("count_lead_zeroes",     circuit->create< CountLeadingZeroes >( size ))
                .Case("count_trailing_zeroes", circuit->create< CountTrailingZeroes >( size ))
                .Case("not",                   circuit->create< Not >( size ))

                .Case("Switch", circuit->create< Switch >( size ))
                .Case("Option", circuit->create< Option >( size ))

                .Default( nullptr );
        }

        operation make_operation(const advice_node &op) {
            return circuit->create< Advice >( op.size.value(), op.idx.value() );
        }

        operation make_operation(const register_node &op) {
            return llvm::StringSwitch< Operation* >(op.op_code_name)
                .Case("in.register",  circuit->create< InputRegister >( op.reg_name, op.size ))
                .Case("out.register", circuit->create< OutputRegister >( op.reg_name, op.size ))
                .Default( nullptr );
        }

        operation make_operation(const constant_node &op) {
            return circuit->create< Constant >( op.bits, op.size );
        }

        operation make_operation(const memory_node &) {
            throw std::runtime_error("make memory_node not implemented");
        }

        operation make_operation(const extract_node &op) {
            return circuit->create< Extract >( op.low_bit_inc, op.high_bit_exc );
        }

        operation make_operation(const select_node &op) {
            return circuit->create< Select >( op.bits, op.size );
        }

        operation make_operation(const node_template &node) {
            auto make = [&] (const auto &op) { return make_operation(op); };

            if (auto value = std::visit(make, node)) {
                return value;
            }

            unreachable() << "unhadled opcode " << to_string(node);
        }

        circuit_owner_t circuit;
        std::unordered_map< node_pointer, operation > cached;
        const optimal_graph &graph;
    };

} // namespace circ
