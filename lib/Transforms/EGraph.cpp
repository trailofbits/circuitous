/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Transforms/EGraph.hpp>

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/APSInt.h>
#include <llvm/ADT/Twine.h>

#include <optional>
#include <variant>

namespace circ
{
    std::string node_name( const node_template &op ) {
        return std::visit( [](const auto &o) { return o.op_code_name; }, op );
    }

    std::optional< gap::bigint > extract_constant( const node_template &op ) {
        if (auto con = std::get_if< constant_node >(&op) ) {
            return gap::bigint(con->size, con->bits, 2);
        }
        return std::nullopt;
    }

    std::string to_string(const node_template &op) {
        return std::visit( gap::overloaded {
            [] (const op_code_node  &o) {
                return o.op_code_name;
            },
            [] (const sized_node    &o) {
                return o.op_code_name + "." + std::to_string(o.size.value());
            },
            [] (const advice_node   &o) {
                return o.op_code_name + "." + std::to_string(o.size.value()) + "." + std::to_string(o.idx.value());
            },
            [] (const register_node &o) {
                return o.op_code_name + "." + o.reg_name;
            },
            [] (const memory_node   &o) {
                return o.op_code_name + "." + std::to_string(o.mem_idx);
            },
            [] (const extract_node  &o) {
                return o.op_code_name + "." + std::to_string(o.low_bit_inc) + "." + std::to_string(o.high_bit_exc);
            },
            [] (const select_node   &o) {
                return o.op_code_name + "." + std::to_string(o.size) + "." + std::to_string(o.bits);
            },
            [] (const constant_node &o) {
                llvm::SmallVector< char > str;
                llvm::APSInt(o.bits).toStringUnsigned(str);
                return llvm::Twine(str).str();
            }
        }, op );
    }

    maybe_bitwidth bitwidth(const node_template &op) {
        return std::visit( gap::overloaded {
            [] (const op_code_node  &o) -> maybe_bitwidth { return std::nullopt; },
            [] (const sized_node    &o) -> maybe_bitwidth { return o.size; },
            [] (const advice_node   &o) -> maybe_bitwidth { return o.size; },
            [] (const register_node &o) -> maybe_bitwidth { return o.size; },
            [] (const memory_node   &o) -> maybe_bitwidth { return std::nullopt; },
            [] (const extract_node  &o) -> maybe_bitwidth { return o.high_bit_exc - o.low_bit_inc; },
            [] (const select_node   &o) -> maybe_bitwidth { return o.size; },
            [] (const constant_node &o) -> maybe_bitwidth { return o.size; }
        }, op);
    }

  // maybe_bitwidth bitwidth(const circuit_enode *node)
  // {
  //   if (node->is_bond_node()) {
  //     return std::nullopt;
  //   }
  //   return bitwidth(node->data());
  // }

  // bool is_context_node(const circuit_enode *node)
  // {
  //   return name(node) == "VerifyInstruction";
  // }

  // std::optional<llvm::APInt>extract_constant(const circuit_enode *node)
  // {
  //   if (auto con = std::get_if< constant_node >(&node->data())) {
  //     return llvm::APInt(con->size, con->bits, 10);
  //   }
  //   return std::nullopt;
  // }

} // namespace cird::eqsat
