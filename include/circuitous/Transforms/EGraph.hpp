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
    struct op_code_node {
        std::string op_code_name;
    };

    struct sized_node {
        std::string op_code_name;
        maybe_bitwidth_t size;
    };

    struct advice_node {
        std::string op_code_name;
        maybe_bitwidth_t size;
        std::optional< std::uint32_t > idx;
    };

    struct register_node {
        std::string op_code_name;
        bitwidth_t size;
        std::string reg_name;
    };

    struct constant_node {
        std::string op_code_name;
        bitwidth_t size;
        std::string bits;
    };

    struct memory_node {
        std::string op_code_name;
        bitwidth_t mem_idx;
    };

    struct extract_node {
        std::string op_code_name;
        std::uint32_t low_bit_inc, high_bit_exc;
    };

    struct select_node {
        std::string op_code_name;
        bitwidth_t size;
        std::uint32_t bits;
    };

    using node_template = std::variant<
        op_code_node,
        sized_node,
        advice_node,
        register_node,
        constant_node,
        memory_node,
        extract_node,
        select_node
    >;

    //
    // Circuit EGraph Representation
    //
    using circuit_enode = eqsat::graph::node< node_template >;
    static_assert(gap::graph::node_like< circuit_enode >);

    using enode_handle = eqsat::graph::node_handle;

    using circuit_edge = eqsat::graph::edge< circuit_enode >;
    static_assert(gap::graph::edge_like< circuit_edge >);

    using circuit_egraph = eqsat::graph::egraph< circuit_enode >;
    static_assert(gap::graph::graph_like< circuit_egraph >);

    std::string node_name(const node_template &op);

    std::optional< gap::bigint > extract_constant(const node_template &op);

    // std::string to_string(const node_template &op);

    // maybe_bitwidth bitwidth(const node_template &op);

    // std::string name(const circuit_enode *enode);

} // namespace circ
