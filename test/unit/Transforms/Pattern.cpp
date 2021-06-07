/*
 * Copyright (c) 2021, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <doctest/doctest.h>
#include <circuitous/Transforms/Pattern.hpp>
#include <optional>
#include <variant>

namespace circuitous {

  using Parser = PatternParser<int>;
  using Place = Parser::Place;
  using Op = Parser::Op;

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
  T parse(const Parser &p, std::string_view str)
  {
    auto val = p.parse(str);
    return get< T, decltype(val), Indices... >(val);
  }

  bool fails(const Parser &parser, std::string_view str)
  {
    return parser.parse(str) == nullptr;
  }

  TEST_CASE("Pattern Simple Parse") {
    Parser parser;

    CHECK(parse< int >(parser, "5") == 5);
    CHECK(parse< int >(parser, "-15") == -15);

    CHECK(parse< Op >(parser, "(op_add 1 2)").name == "add");
    CHECK(parse< int, 0 >(parser, "(op_add 1 2)") == 1 );
    CHECK(parse< int, 1 >(parser, "(op_add 1 2)") == 2 );

    CHECK(parse< Place >(parser, "?x").name == "x");

    CHECK(parse< Op >(parser, "(op_add (op_mul 1 ?x) 3)").name == "add" );
    CHECK(parse< Op, 0 >(parser, "(op_add (op_mul 1 ?x) 3)").name == "mul" );
    CHECK(parse< int, 0, 0 >(parser, "(op_add (op_mul 1 ?x) 3)") == 1 );
    CHECK(parse< Place, 0, 1 >(parser, "(op_add (op_mul 1 ?x) 3)").name == "x" );
    CHECK(parse< int, 1 >(parser, "(op_add (op_mul 1 ?x) 3)") == 3 );

    CHECK(fails(parser, "?-7"));
    CHECK(fails(parser, "?7"));

    CHECK(fails(parser, "("));
    CHECK(fails(parser, ")"));
    CHECK(fails(parser, "())"));

    CHECK(fails(parser, "5val"));
  }
} // namespace circuitous