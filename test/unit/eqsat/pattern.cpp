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

#include <gap/core/concepts.hpp>

namespace eqsat::test {

    template< gap::unsigned_integral value_type >
    constexpr std::size_t bit_size() noexcept {
        return sizeof(value_type) * CHAR_BIT;
    }

    atom_t operation(std::string name) { return { operation_t{ std::move(name) } }; }

    atom_t constant(gap::bigint value) { return atom_t( constant_t{ value } ); }

    template< gap::unsigned_integral value_type >
    atom_t constant(value_type value) {
        return constant(gap::bigint{ bit_size< value_type >(), value });
    }

    atom_t place(std::string name) { return { place_t{ std::move(name) } }; }

    atom_t label(std::string name) { return { unary_label{ std::move(name) } }; }

    std::string as_value_string(const auto &atom) {
        return std::get< constant_t >(atom).ref().to_string(10);
    }

    TEST_SUITE("eqsat::pattern-parser") {

    TEST_CASE("Constant Parser") {
        {
            auto atom = parse_atom("4:64");
            CHECK(atom);
            CHECK(std::holds_alternative< constant_t >(atom.value()));
            CHECK_EQ(as_value_string(atom.value()), "4");

            CHECK(atom->bitwidth());
            CHECK_EQ(atom->bitwidth().value(), 64);
        }

        CHECK(parse_constant("400:64"));
    }

    TEST_CASE("Expr Parser") {
        CHECK(parse_simple_expr("(op_add ?x ?y)"));
        CHECK(parse_simple_expr("(op_add ?x (op_mul 2:64 ?y))"));

        {
            auto expr = parse_simple_expr("(op_add ?x (op_mul 2:64 ?y))");
            CHECK(expr);
            CHECK_EQ(root(expr.value()), operation("add"));
        }

        {
            auto expr = parse_simple_expr("(op_add (op_mul 1:32 ?x) 3:32)");
            CHECK(expr);
            CHECK_EQ(root(expr.value()), operation("add"));
            auto ch = children(expr.value());
            CHECK_EQ(std::get< atom_t >(ch[1]), constant(3u));

            auto subexpr = ch[0];
            CHECK_EQ(root(subexpr), operation("mul"));
            auto subch = children(subexpr);
            CHECK_EQ(std::get< atom_t >(subch[1]), place("x"));
        }

        CHECK(!parse_simple_expr("(op_add ?x (op_mul 2:64 ?y)"));
    }

    TEST_CASE("Pattern Places") {
        auto count_places = [](std::string_view in) {
            auto match = parse_match_pattern(in);
            CHECK(match);

            return gather_places(match.value()).size();
        };

        CHECK_EQ(count_places("(?x)"), 1);
        CHECK_EQ(count_places("(op_mul ?x ?y)"), 2);
        CHECK_EQ(count_places("(op_mul ?x ?x)"), 1);
        CHECK_EQ(count_places("(?x ?y ?z)"), 3);
        CHECK_EQ(count_places("(op_add (op_mul 1:64 ?x) ?y)"), 2);
        CHECK_EQ(count_places("(op_add (op_mul 1:64 ?x) ?x)"), 1);
        CHECK_EQ(count_places("((let X (?x)) (let Y (?y)) (op_mul $X $Y))"), 2);
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

    TEST_CASE("Nested Apply Pattern") {
        CHECK(parse_apply_pattern("((let X (?x)) (op_add ?y $X))"));
    }

    TEST_CASE("Union") {
        CHECK(parse_apply_pattern("(union $A)"));
        CHECK(parse_apply_pattern("(union $A $B)"));
        CHECK(parse_apply_pattern("(union $A $B $C $D)"));
    }

    TEST_CASE("Bitwidth") {
        CHECK(parse_atom("op_add:32")->bitwidth() == 32);
        CHECK(parse_atom("op_add:64")->bitwidth() == 64);
        CHECK(!parse_atom("op_add")->bitwidth().has_value());
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
