/*
 * Copyright (c) 2022, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <doctest/doctest.h>
#include <eqsat/pattern/pattern.hpp>
#include <eqsat/pattern/parser.hpp>

namespace eqsat::test {

    atom_t operation(std::string name) { return { operation_t{ std::move(name) } }; }

    atom_t constant(std::int64_t value) { return { constant_t{ value } }; }

    atom_t place(std::string name) { return { place_t{ std::move(name) } }; }

    atom_t label(std::string name) { return { unary_label{ std::move(name) } }; }

    TEST_SUITE("eqsat::pattern-parser") {

    TEST_CASE("Expr Parser") {
        CHECK(parse_simple_expr("(op_add ?x ?y)"));
        CHECK(parse_simple_expr("(op_add ?x (op_mul 2 ?y))"));

        {
            auto expr = parse_simple_expr("(op_add ?x (op_mul 2 ?y))");
            CHECK(expr);
            CHECK_EQ(root(expr.value()), operation("add"));
        }

        {
            auto expr = parse_simple_expr("(op_add (op_mul 1 ?x) 3)");
            CHECK(expr);
            CHECK_EQ(root(expr.value()), operation("add"));
            auto ch = children(expr.value());
            CHECK_EQ(std::get< atom_t >(ch[1]), constant(3));

            auto subexpr = ch[0];
            CHECK_EQ(root(subexpr), operation("mul"));
            auto subch = children(subexpr);
            CHECK_EQ(std::get< atom_t >(subch[1]), place("x"));

            CHECK_EQ(gather_places(expr.value()).size(), 1);
        }

        CHECK(!parse_simple_expr("(op_add ?x (op_mul 2 ?y)"));
    }

    TEST_CASE("Pattern Places") {
        auto count_places = [](std::string_view in) {
            return gather_places(parse_simple_expr(in).value()).size();
        };

        CHECK_EQ(count_places("(?x)"), 1);
        CHECK_EQ(count_places("(op_mul ?x ?y)"), 2);
        CHECK_EQ(count_places("(op_mul ?x ?x)"), 1);
        CHECK_EQ(count_places("(?x ?y ?z)"), 3);
        CHECK_EQ(count_places("(op_add (op_mul 1 ?x) ?y)"), 2);
        CHECK_EQ(count_places("(op_add (op_mul 1 ?x) ?x)"), 1);
    }

    TEST_CASE("Named Expr") {
        auto expr = parse_named_expr("(let place (?x))");
        CHECK(expr);
        CHECK_EQ(expr->name, "place");
        CHECK_EQ(root(expr.value()), place("x"));
    }

    TEST_CASE("Pattern With Named Subexpressions") {
        // named subexpression without action is not a pattern
        CHECK(!parse_match_pattern("((let place (?x)))"));

        {
            auto expr = parse_match_pattern("((let place (?x)) ($place))");
            CHECK(expr);

            CHECK_EQ(expr->list.size(), 1);
            CHECK_EQ(root(expr->action), label("place"));
        }

        {
            auto expr = parse_match_pattern("((let X (?x)) (let Y (?y)) (op_add $X $Y))");
            CHECK(expr);
            CHECK_EQ(expr->list.size(), 2);
            CHECK_EQ(root(expr->action), operation("add"));

            auto ch = children(expr->action);
            CHECK_EQ(std::get< atom_t >(ch[0]), label("X"));
            CHECK_EQ(std::get< atom_t >(ch[1]), label("Y"));
        }
    }

    TEST_CASE("Multi-match") {
        CHECK(parse_match_expr("(match $A)"));
        CHECK(parse_match_expr("(match $A $B)"));
        CHECK(parse_match_expr("(match $A $B $C $D)"));

        CHECK(parse_match_pattern(
            "((let A (op_add ?x ?y)) (let B (op_add ?x ?y)) (match $A $B))"
        ));
    }

    TEST_CASE("Context Declaration") {
        CHECK(parse_constraint("(disjoint C1 C2)"));
        CHECK(parse_constraint("(disjoint CA CB CD)"));

        CHECK(parse_match_pattern(
            "((let X (?x)) (let Y (?y)) (disjoint C1 C2) (match $X $Y))"
        ));

        CHECK(parse_match_pattern(
            "((let X (?x)) (let Y (?y)) (disjoint C1 C2) (op_add $X $Y))"
        ));

        CHECK(parse_match_pattern(
            "((let X (?x):C1) (let Y (?y):C2) (disjoint C1 C2) (match $X $Y))"
        ));
    }

    TEST_CASE("Union") {
        CHECK(parse_apply_pattern("(union $A)"));
        CHECK(parse_apply_pattern("(union $A $B)"));
        CHECK(parse_apply_pattern("(union $A $B $C $D)"));
    }

    TEST_CASE("Bitwidth") {
        CHECK(parse_atom("op_add:32")->bitwidth == 32);
        CHECK(parse_atom("op_add:64")->bitwidth == 64);
        CHECK(!parse_atom("op_add")->bitwidth.has_value());
    }

    TEST_CASE("Variadic") {
        CHECK(parse_match_expr("(match $M...)"));

        CHECK(parse_match_pattern("((let M (op_mul)) (op_bond $M...))"));
        CHECK(parse_match_pattern("((let M (op_mul)) (match $M...))"));
        CHECK(parse_match_pattern("((let M (op_mul)) (commutative-match $M...))"));
        CHECK(parse_match_pattern("((let M (op_mul)) (let A (op_add)) (match $M... $A...))"));

        CHECK(parse_apply_action("(union $M...)"));
        CHECK(parse_apply_action("(bond $M...)"));

        CHECK(parse_match_pattern("((let M (op_mul):C) (disjoint C...) (match $M...))"));
    }
    } // test suite: eqsat::pattern-parser

} // namespace eqsat::test
