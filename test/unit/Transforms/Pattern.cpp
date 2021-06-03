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

  using parser_t = PatternParser<int>;
  using list_t = parser_t::List;
  using error_t = parser_t::Error;
  using place_t = parser_t::Place;
  using op_t = parser_t::Op;

  template< typename T, typename Parsed  >
  T get(const Parsed &val)
  {
    CHECK(std::holds_alternative< T >(val));
    return std::get<T>(val);
  }

  template< typename T, typename Parsed, unsigned I, unsigned ...Is >
  T get(const Parsed &val)
  {
    CHECK(std::holds_alternative<list_t>(val));
    auto list = std::get<list_t>(val);
    return get< T >(list[I], Is...);
  }
  template< typename T, unsigned ...Indices >
  T parse(const parser_t &p, std::string_view str)
  {
    auto val = p.parse(str);
    return get< T, decltype(val), Indices... >(val);
  }

  bool fails(const parser_t &parser, std::string_view str)
  {
    auto val = parser.parse(str);
    return std::holds_alternative<error_t>(val);
  }

  TEST_CASE("Pattern Simple Parse") {
    parser_t parser;

    CHECK(parse<int>(parser, "5") == 5);
    CHECK(parse<int>(parser, "-15") == -15);

    CHECK(parse<int, 0>(parser, "(1 2 3)") == 1);
    CHECK(parse<int, 1>(parser, "(1 2 3)") == 2);
    CHECK(parse<int, 2>(parser, "(1 2 3)") == 3);

    CHECK(parse<op_t, 0>(parser, "(op_add 1 2)").name == "add");

    CHECK(parse<place_t>(parser, "?x").name == "x");
    CHECK(fails(parser, "?-7"));
    CHECK(fails(parser, "?7"));

    CHECK(fails(parser, "("));
    CHECK(fails(parser, ")"));
    CHECK(fails(parser, "())"));

    CHECK(fails(parser, "5val"));
  }
} // namespace circuitous