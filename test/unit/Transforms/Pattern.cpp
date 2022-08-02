/*
 * Copyright (c) 2021, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

// #include <doctest/doctest.h>
// #include <support/EGraph.hpp>
// #include <circuitous/Transforms/eqsat/pattern.hpp>
// #include <circuitous/IR/IR.hpp>
// #include <optional>
// #include <variant>

// namespace circ::eqsat {

//   TEST_CASE("Expr Parser") {
//     auto parser = pattern_parser();
//     CHECK(parser("(op_add ?x ?y)"));
//     CHECK(parser("(op_add ?x (op_mul 2 ?y))"));

//     {
//       auto expr = result(parser("(op_add ?x (op_mul 2 ?y))"));
//       CHECK_EQ(root(expr), atom(operation("add")));
//     }

//     {
//       auto expr = result(parser("(op_add (op_mul 1 ?x) 3)"));
//       CHECK_EQ(root(expr), atom(operation("add")));
//       auto ch = children(expr);
//       CHECK_EQ(std::get<atom>(ch[1]), atom(constant(3)));

//       auto subexpr = ch[0];
//       CHECK_EQ(root(subexpr), atom(operation("mul")));
//       auto subch = children(subexpr);
//       CHECK_EQ(std::get<atom>(subch[1]), atom(place("x")));

//       CHECK_EQ(places(expr).size(), 1);
//     }

//     // missing closing parenthesis
//     CHECK(!parser("(op_add ?x (op_mul 2 ?y)"));
//   }

//   TEST_CASE("Pattern Places") {
//     auto parser = pattern_parser();

//     auto count_places = [&parser] (std::string_view in) {
//       auto res = parser(in);
//       return places(result(res)).size();
//     };

//     CHECK_EQ(count_places("(?x)"), 1);
//     CHECK_EQ(count_places("(op_mul ?x ?y)"), 2);
//     CHECK_EQ(count_places("(op_mul ?x ?x)"), 1);
//     CHECK_EQ(count_places("(?x ?y ?z)"), 3);
//     CHECK_EQ(count_places("(op_add (op_mul 1 ?x) ?y)"), 2);
//     CHECK_EQ(count_places("(op_add (op_mul 1 ?x) ?x)"), 1);
//   }

//   TEST_CASE("Named Expr") {
//     auto parser = named_expr_parser();

//     {
//       auto p = parser("(let place (?x))");
//       CHECK(p);
//       CHECK_EQ(label_name(result(p).name), "place");
//       CHECK_EQ(root(result(p)), atom(place("x")));
//     }
//   }

//   TEST_CASE("Pattern With Named Subexpressions") {
//     auto parser = pattern_parser();

//     // only a named subexpression is not a pattern
//     CHECK(!parser("((let place (?x)))"));

//     {
//       auto p = parser("((let place (?x)) ($place))");
//       CHECK(p);
//       CHECK_EQ(result(p).subexprs.size(), 1);
//       CHECK_EQ(root(result(p)), atom(unary_label("place")));
//     }

//     {
//       auto p = parser("((let X (?x)) (let Y (?y)) (op_add $X $Y))");
//       CHECK(p);
//       CHECK_EQ(result(p).subexprs.size(), 2);
//       auto res = result(p);
//       CHECK_EQ(root(res), atom(operation("add")));

//       auto ch = children(res);
//       CHECK_EQ(std::get<atom>(ch[0]), atom(unary_label("X")));
//       CHECK_EQ(std::get<atom>(ch[1]), atom(unary_label("Y")));
//     }
//   }

//   TEST_CASE("Multi-match") {

//     CHECK(match_expr_parser()("(match $A)"));
//     CHECK(match_expr_parser()("(match $A $B)"));
//     CHECK(match_expr_parser()("(match $A $B $C $D)"));

//     auto parser = pattern_parser();

//     {
//       auto p = parser("((let A (op_add ?x ?y)) (let B (op_add ?x ?y)) (match $A $B))");
//       CHECK(p);
//     }
//   }

//   TEST_CASE("Context Declaration") {

//     CHECK(disjoint_expr_parser()("(disjoint C1 C2)"));
//     CHECK(disjoint_expr_parser()("(disjoint CA CB CD)"));

//     auto parser = pattern_parser();

//     {
//       auto p = parser("((let X (?x)) (let Y (?y)) (disjoint C1 C2) (match $X $Y))");
//       CHECK(p);
//     }

//     {
//       auto p = parser("((let X (?x)) (let Y (?y)) (disjoint C1 C2) (op_add $X $Y))");
//       CHECK(p);
//     }

//     {
//       auto p = parser("((let X (?x):C1) (let Y (?y):C2) (disjoint C1 C2) (match $X $Y))");
//       CHECK(p);
//     }
//   }

//   TEST_CASE("Union") {
//     auto parser = pattern_parser();
//     CHECK(parser("(union $A)"));
//     CHECK(parser("(union $A $B)"));
//     CHECK(parser("(union $A $B $C $D)"));
//   }

//   TEST_CASE("Bitwidth") {
//     auto parser = operation_parser();
//     CHECK(result(parser("op_add:32")).bitwidth == 32);
//     CHECK(result(parser("op_add:64")).bitwidth == 64);
//     CHECK(!result(parser("op_add")).bitwidth.has_value());
//   }

//   TEST_CASE("Variadic") {
//     CHECK( match_expr_parser()("(match $M...)") );

//     auto parser = pattern_parser();
//     CHECK(parser("((let M (op_mul)) (op_bond $M...))"));
//     CHECK(parser("((let M (op_mul)) (match $M...))"));
//     CHECK(parser("((let M (op_mul)) (commutative-match $M...))"));
//     CHECK(parser("((let M (op_mul)) (let A (op_add)) (match $M... $A...))"));

//     CHECK_THROWS(parser("((let M (op_mul ?a ?b)) (op_bond $M...))"));
//     CHECK_THROWS(parser("((let M (op_mul ?a ?b)) (match $M...))"));
//     CHECK_THROWS(parser("((let M (op_mul ?a ?b)) (commutative-match $M...))"));
//     CHECK_THROWS(parser("((let M (op_mul ?a ?b)) (let A (op_add)) (match $M... $A...))"));

//     CHECK(parser("(union $M...)"));
//     CHECK(parser("(bond $M...)"));

//     CHECK(parser("((let M (op_mul):C) (disjoint C...) (match $M...))"));
//   }

// } // namespace circ::eqsat
