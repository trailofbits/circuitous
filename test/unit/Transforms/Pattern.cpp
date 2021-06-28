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

namespace circ::eqsat {

  TEST_CASE("Expr Parser") {
    auto parser = expr_parser::parser();
    CHECK(parser("(op_add ?x ?y)"));
    CHECK(parser("(op_add ?x (op_mul 2 ?y))"));

    {
      auto expr = result(parser("(op_add ?x (op_mul 2 ?y))"));
      CHECK(root(expr) == atom(operation("add")));
    }

    {
      auto expr = result(parser("(op_add (op_mul 1 ?x) 3)"));
      CHECK(root(expr) == atom(operation("add")));
      auto ch = children(expr);
      CHECK(std::get<atom>(ch[1]) == atom(constant(3)));

      auto subexpr = ch[0];
      CHECK(root(subexpr) == atom(operation("mul")));
      auto subch = children(subexpr);
      CHECK(std::get<atom>(subch[1]) == atom(place("x")));

      CHECK(places(expr).size() == 1);
    }

    // missing closing parenthesis
    CHECK(!parser("(op_add ?x (op_mul 2 ?y)"));
  }

  TEST_CASE("Pattern Places") {
    auto parser = expr_parser::parser();

    auto count_places = [&parser] (std::string_view in) {
      auto res = parser(in);
      return places(result(res)).size();
    };

    CHECK(count_places("(?x)") == 1);
    CHECK(count_places("(op_mul ?x ?y)") == 2);
    CHECK(count_places("(op_mul ?x ?x)") == 1);
    CHECK(count_places("(?x ?y ?z)") == 3);
    CHECK(count_places("(op_add (op_mul 1 ?x) ?y)") == 2);
    CHECK(count_places("(op_add (op_mul 1 ?x) ?x)") == 1);
  }

} // namespace circ::eqsat
