/*
 * Copyright (c) 2021, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <doctest/doctest.h>
#include <support/EGraph.hpp>
#include <circuitous/Transforms/Pattern.hpp>
#include <optional>
#include <variant>
#include "circuitous/IR/IR.h"

namespace circuitous::eqsat {

  using Parser = PatternParser;
  using Constant = ASTNode::Constant;
  using Place    = ASTNode::Place;
  using Op       = ASTNode::Op;

  template< typename T, typename Parsed  >
  T get(const Parsed &node)
  {
    return std::get<T>(node->value);
  }

  template< typename T, typename Parsed, unsigned I, unsigned ...Is >
  T get(const Parsed &val)
  {
    return get< T, Parsed, Is... >(val->children[I]);
  }

  template< typename T, unsigned ...Indices >
  T parse(Parser &p, std::string_view str)
  {
    auto val = p.parse(str);
    return get< T, decltype(val), Indices... >(val);
  }

  bool fails(Parser &parser, std::string_view str)
  {
    return parser.parse(str) == nullptr;
  }

  TEST_CASE("Pattern Simple Parse") {
    Parser parser;

    CHECK(parse< Constant >(parser, "5") == 5);
    CHECK(parse< Constant >(parser, "-15") == -15);

    CHECK(parse< Op >(parser, "(op_add 1 2)") == "add");
    auto a = parse< Constant, 0 >(parser, "(op_add 1 2)");
    CHECK(a == 1);
    auto b = parse< Constant, 1 >(parser, "(op_add 1 2)");
    CHECK(b == 2);

    CHECK(parse< Place >(parser, "?x") == 0);

    CHECK(parse< Op >(parser, "(op_add (op_mul 1 ?x) 3)") == "add" );

    auto c = parse< Op, 0 >(parser, "(op_add (op_mul 1 ?x) 3)");
    CHECK(c == "mul");

    auto d = parse< Constant, 0, 0 >(parser, "(op_add (op_mul 1 ?x) 3)");
    CHECK(d == 1);

    auto e = parse< Place, 0, 1 >(parser, "(op_add (op_mul 1 ?x) 3)");
    CHECK(e == 0);

    auto f = parse< Constant, 1 >(parser, "(op_add (op_mul 1 ?x) 3)");
    CHECK(f == 3);

    CHECK(fails(parser, "?-7"));
    CHECK(fails(parser, "?7"));

    CHECK(fails(parser, "("));
    CHECK(fails(parser, ")"));
    CHECK(fails(parser, "())"));

    CHECK(fails(parser, "5val"));
  }

  TEST_CASE("Pattern Places") {
    using Places = Pattern::Places;
    CHECK(Pattern("?x").places.size() == 1);
    CHECK(Pattern("(op_mul ?x ?y)").places.size() == 2);
    CHECK(Pattern("(op_mul ?x ?x)").places.size() == 1);
    CHECK(Pattern("(?x ?y ?z)").places.size() == 3);
    CHECK(Pattern("(op_add (op_mul 1 ?x) ?y)").places.size() == 2);
    CHECK(Pattern("(op_add (op_mul 1 ?x) ?x)").places.size() == 1);
  }

} // namespace circuitous::eqsat