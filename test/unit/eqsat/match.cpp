/*
 * Copyright (c) 2022, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <doctest/doctest.h>

#include <eqsat/pattern/rewrite_rule.hpp>
#include <eqsat/algo/ematch.hpp>

#include <support/egraph.hpp>

namespace eqsat::test {

    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wunused-variable"
    auto count_matches(auto &&matches) {
        std::size_t count = 0;
        for (auto _ : matches)
            count++;
        return count;
    }
    #pragma GCC diagnostic pop

    TEST_SUITE("eqsat::pattern-matching") {
    TEST_CASE("basic") {
        test_graph egraph;

        auto idx = make_node(egraph, "x");
        auto idy = make_node(egraph, "1:64");
        make_node(egraph, "mul", idx, idy);

        SUBCASE("multiplication identity") {
            auto rule = rewrite_rule("multiplication identity", "(op_mul ?x 1:64)", "(?x)");
            CHECK(count_matches(match(rule, egraph)) == 1);
        }

        SUBCASE("zero multiplication") {
            auto rule = rewrite_rule("zero multiplication", "(op_mul ?x 0:64)", "(0:64)");
            CHECK(count_matches(match(rule, egraph)) == 0);
        }

        SUBCASE("commutativity multiplication") {
            auto rule = rewrite_rule("commutativity multiplication", "(op_mul ?x ?y)", "(op_mul ?y ?x)");
            CHECK(count_matches(match(rule, egraph)) == 1);
        }

        SUBCASE("identity") {
            auto rule = rewrite_rule("identity", "(?x)", "(?x)");
            CHECK(count_matches(match(rule, egraph)) == 3);
        }

        SUBCASE("named commutativity") {
            auto rule = rewrite_rule("named commutativity", "((let X (?x)) (let Y (?y)) (op_mul $X $Y))", "(op_mul ?y ?x)");
            CHECK(count_matches(match(rule, egraph)) == 1);
        }
    }
    } // test suite: eqsat::pattern-patching

} // namespace eqsat::test
