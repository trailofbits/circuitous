/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/core/common.hpp>
#include <eqsat/core/egraph.hpp>

#include <cstdint>
#include <optional>
#include <string>
#include <variant>

namespace circ
{
    using eqsat::bitwidth_t;

    using maybe_bitwidth_t = std::optional< bitwidth_t >;

    //
    // Node Templates keep data required to rebuild circuitous IR from EGraph
    //
    struct OpCodeNode {
        std::string op_code_name;
    };

    struct SizedNode {
        std::string op_code_name;
        maybe_bitwidth_t size;
    };

    struct AdviceNode {
        std::string op_code_name;
        maybe_bitwidth_t size;
        std::optional< std::uint32_t > idx;
    };

    struct RegisterNode {
        std::string op_code_name;
        bitwidth_t size;
        std::string reg_name;
    };

    struct ConstantNode {
        std::string op_code_name;
        bitwidth_t size;
        std::string bits;
    };

    struct MemoryNode {
        std::string op_code_name;
        bitwidth_t mem_idx;
    };

    struct ExtractNode {
        std::string op_code_name;
        std::uint32_t low_bit_inc, high_bit_exc;
    };

    struct SelectNode {
        std::string op_code_name;
        bitwidth_t size;
        std::uint32_t bits;
    };

    using NodeTemplate = std::variant<
        OpCodeNode,
        SizedNode,
        AdviceNode,
        RegisterNode,
        ConstantNode,
        MemoryNode,
        ExtractNode,
        SelectNode
    >;

    //
    // Circuit EGraph Representation
    //
    using CircuitENode = eqsat::graph::node< NodeTemplate >;
    static_assert(gap::graph::node_like< CircuitENode >);

    using ENodeHandle = eqsat::graph::node_handle;

    using CircuitEdge = eqsat::graph::edge< CircuitENode >;
    static_assert(gap::graph::edge_like< CircuitEdge >);

    using CircuitEGraph = eqsat::graph::egraph< CircuitENode >;
    static_assert(gap::graph::graph_like< CircuitEGraph >);

    std::string node_name(const NodeTemplate &op);

    std::optional< gap::bigint > extract_constant(const NodeTemplate &op);

    // std::string to_string(const NodeTemplate &op);

    // maybe_bitwidth bitwidth(const NodeTemplate &op);

    // std::string name(const circuit_enode *enode);

} // namespace circ
