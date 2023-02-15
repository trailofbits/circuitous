/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <eqsat/core/common.hpp>
#include <eqsat/core/egraph.hpp>

#include <circuitous/IR/Visitors.hpp>

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
        maybe_bitwidth_t size;
        std::optional< std::uint32_t > idx;
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

    struct circuit_egraph_builder_base {

        static op_code_node opcode(auto *op) {
            return { op->op_code_str() };
        }

        static op_code_node opcode(const std::string &name) {
            return { name };
        }

        static sized_node sized(auto *op) {
            return { op->op_code_str(), op->size };
        }

        static sized_node sized(const std::string &name, maybe_bitwidth_t size) {
            return { name, size };
        }

        static advice_node advice(Advice *op) {
            return { op->op_code_str(), op->size, op->advice_idx };
        }

        static advice_node advice(const std::string &name, bitwidth_t size, std::uint32_t idx) {
            return { name, size, idx };
        }

        static register_node regop(auto *op) {
            return { op->op_code_str(), op->size, op->reg_name };
        }

        static register_node regop(const std::string &name, bitwidth_t size, const std::string &reg_name) {
            return { name, size, reg_name };
        }

        static constant_node constop(Constant *op) {
            return { op->op_code_str(), op->size, op->bits };
        }

        static memory_node memop(Memory *op) {
            return { op->op_code_str(), op->size, op->mem_idx };
        }

        static extract_node extract(Extract *op) {
            return { op->op_code_str(), op->low_bit_inc, op->high_bit_exc };
        }

        static select_node select(Select *op) {
            return { op->op_code_str(), op->size, op->bits };
        }
    };

    //
    // Circuit from pattern builder
    //
    template< typename egraph >
    struct circuit_graph_from_pattern_builder
        : circuit_egraph_builder_base
    {
        using storage_type = typename egraph::storage_type;

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

        static storage_type make(const eqsat::constant_t &con) {
            // TODO build storage
            throw std::runtime_error("not implemented constant synthesis");
        }

        static storage_type make(const eqsat::atom_t &op) {
            // TODO build storage
            auto name = atom_name(op);
            auto res = llvm::StringSwitch< storage_type, std::optional< storage_type > >(name)
                .Case("register_constraint", opcode(name))
                .Case("advice_constraint"  , opcode(name))
                .Case("write_constraint"   , opcode(name))
                .Case("read_constraint"    , opcode(name))
                .Case("unused_constraint"  , opcode(name))
                .Case("parity"             , opcode(name))
                .Case("DecodeCondition"    , opcode(name))
                .Case("DecoderResult"      , opcode(name))
                .Case("VerifyInstruction"  , opcode(name))
                .Case("OnlyOneCondition"   , opcode(name))
                .Case("DecoderResult"      , opcode(name))
                // sized
                .Case("in.timestamp"       , sized(name, op.bitwidth()))
                .Case("out.timestamp"      , sized(name, op.bitwidth()))
                .Case("in.error_flag"      , sized(name, op.bitwidth()))
                .Case("out.error_flag"     , sized(name, op.bitwidth()))
                .Case("undefined"          , sized(name, op.bitwidth()))
                .Case("instruction_bits"   , sized(name, op.bitwidth()))
                // binary sized
                .Case("Add"     , sized(name, op.bitwidth()))
                .Case("Sub"     , sized(name, op.bitwidth()))
                .Case("Mul"     , sized(name, op.bitwidth()))
                .Case("UDiv"    , sized(name, op.bitwidth()))
                .Case("SDiv"    , sized(name, op.bitwidth()))
                .Case("URem"    , sized(name, op.bitwidth()))
                .Case("Xor"     , sized(name, op.bitwidth()))
                .Case("SRem"    , sized(name, op.bitwidth()))
                .Case("Shl"     , sized(name, op.bitwidth()))
                .Case("LShr"    , sized(name, op.bitwidth()))
                .Case("AShr"    , sized(name, op.bitwidth()))
                .Case("Trunc"   , sized(name, op.bitwidth()))
                .Case("ZExt"    , sized(name, op.bitwidth()))
                .Case("SExt"    , sized(name, op.bitwidth()))
                // binary relational
                .Case("Icmp_ult", sized(name, 1))
                .Case("Icmp_slt", sized(name, 1))
                .Case("Icmp_ugt", sized(name, 1))
                .Case("Icmp_eq" , sized(name, 1))
                .Case("Icmp_ne" , sized(name, 1))
                .Case("Icmp_uge", sized(name, 1))
                .Case("Icmp_ule", sized(name, 1))
                .Case("Icmp_sgt", sized(name, 1))
                .Case("Icmp_sge", sized(name, 1))
                .Case("Icmp_sle", sized(name, 1))
                // concat
                .Case("concat", sized(name, op.bitwidth()))
                // bitops
                .Case("Or"  , sized(name, op.bitwidth()))
                .Case("And" , sized(name, op.bitwidth()))
                .Case("Xor" , sized(name, op.bitwidth()))
                // input
                .Case("input_immediate" , sized(name, op.bitwidth()))
                // other
                .Case("pop_count"               , sized(name, op.bitwidth()))
                .Case("count_lead_zeroes"       , sized(name, op.bitwidth()))
                .Case("count_trailing_zeroes"   , sized(name, op.bitwidth()))
                .Case("not"                     , sized(name, op.bitwidth()))
                .Case("Switch"                  , sized(name, op.bitwidth()))
                .Case("Option"                  , sized(name, op.bitwidth()))
                .Default({});

            if (!res) {
                throw std::runtime_error(std::string("not implemented operation synthesis: ") + name);
            }

            return res.value();
        }
    };

    //
    // Circuit EGraph Representation
    //
    using circuit_enode = eqsat::graph::node< node_template >;
    static_assert(gap::graph::node_like< circuit_enode >);

    using enode_handle = eqsat::graph::node_handle;

    using circuit_edge = eqsat::graph::edge< circuit_enode >;
    static_assert(gap::graph::edge_like< circuit_edge >);

    using circuit_egraph = eqsat::graph::egraph_pattern_buildable<
        eqsat::graph::egraph< circuit_enode >, circuit_graph_from_pattern_builder
    >;
    static_assert(gap::graph::graph_like< circuit_egraph >);

    std::string node_name(const node_template &op);

    std::optional< gap::bigint > extract_constant(const node_template &op);

    std::string to_string(const node_template &op);

    using maybe_bitwidth = std::optional< bitwidth_t >;
    maybe_bitwidth bitwidth(const node_template &op);

} // namespace circ
