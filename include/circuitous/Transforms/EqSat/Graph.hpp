/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/ADT/EGraph.hpp>
#include <circuitous/ADT/UnionFind.hpp>

#include <string>
#include <optional>
#include <variant>
#include <cstdint>

namespace circ::eqsat {

  using Id = UnionFind::Id;

  struct OpCode
  {
    std::string op_code_name;
  };

  struct SizedOp
  {
    std::string op_code_name;
    std::optional< std::uint32_t > size;
  };

  struct AdviceOp
  {
    std::string op_code_name;
    std::optional< std::uint32_t > size;
    std::optional< std::uint32_t > idx;
  };

  struct RegOp
  {
    std::string op_code_name;
    std::uint32_t size;
    std::string reg_name;
  };

  struct ConstOp
  {
    std::string op_code_name;
    std::uint32_t size;
    std::string bits;
  };

  struct MemOp
  {
    std::string op_code_name;
    std::uint32_t mem_idx;
  };

  struct ExtractOp
  {
    std::string op_code_name;
    std::uint32_t low_bit_inc, high_bit_exc;
  };

  struct SelectOp
  {
    std::string op_code_name;
    std::uint32_t size;
    std::uint32_t bits;
  };

  using OpTemplate = std::variant< OpCode, SizedOp, AdviceOp, RegOp, ConstOp, MemOp, ExtractOp, SelectOp >;

  std::string node_name(const OpTemplate &op);

  std::string to_string(const OpTemplate &op);

  template< typename stream >
  auto operator<<(stream &os, const OpTemplate &op) -> decltype(os << "")
  {
    return os << to_string(op);
  }

  inline bool operator==(const OpTemplate &lhs, const OpTemplate &rhs)
  {
    return to_string(lhs) == to_string(rhs);
  }

  using CircuitENode  = ENode< OpTemplate >;
  using CircuitEGraph = EGraph< CircuitENode >;

  std::string name(const CircuitENode *node);

  bool is_context_node(const CircuitENode *node);

  std::optional<std::int64_t> extract_constant(const CircuitENode *node);

} // namespace circ::eqsat
