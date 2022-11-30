/*
 * Copyright (c) 2022, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <doctest/doctest.h>

#include <eqsat/pattern/rewrite_rule.hpp>
#include <eqsat/algo/saturation.hpp>
#include <eqsat/algo/ematch.hpp>
#include <eqsat/algo/print.hpp>

#include <support/egraph.hpp>

namespace eqsat::test {

    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wunused-variable"
    auto count_matches(auto &&matches) {
        std::size_t count = 0;
        for (auto _ : matches) {
            count++;
        }
        return count;
    }
    #pragma GCC diagnostic pop

    TEST_SUITE("eqsat::pattern-matching") {
    TEST_CASE("basic") {
        test_graph egraph;

        auto idx = make_node(egraph, "x");
        auto idy = make_node(egraph, "1:64");
        make_node(egraph, "mul", {idx, idy});

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

    TEST_CASE("addition") {
        test_graph egraph;

        auto idx  = make_node(egraph, "x");
        auto idy  = make_node(egraph, "y");
        auto add1 = make_node(egraph, "add", {idx, idy});

        auto idu  = make_node(egraph, "u");
        auto idv  = make_node(egraph, "v");
        auto add2 = make_node(egraph, "add", {idu, idv});

        auto rule = rewrite_rule("commutativity", "(op_add ?x ?y)", "(op_add ?y ?x)");
        CHECK(count_matches(match(rule, egraph)) == 2);

        auto saturable = saturable_egraph(std::move(egraph));

        saturable.merge(add1, add2);
        saturable.rebuild();

        CHECK(count_matches(match(rule, saturable)) == 2);
    }

    TEST_CASE("nested additions") {
        test_graph egraph;

        auto ida  = make_node(egraph, "a:64");
        auto idb  = make_node(egraph, "b:64");
        auto idz  = make_node(egraph, "0:64");
        auto add1 = make_node(egraph, "add", {idz, ida});
        /* auto add2 = */ make_node(egraph, "add", {idb, add1});

        auto rule = rewrite_rule("addition", "(op_add ?x (op_add 0:64 ?y))", "(op_add ?x ?y)");
        CHECK(count_matches(match(rule, egraph)) == 1);
    }

    TEST_CASE("same arguments") {
        test_graph egraph;
        auto ida  = make_node(egraph, "a:64");
        auto idb  = make_node(egraph, "b:64");
        /* auto add1 = */ make_node(egraph, "add", {ida, idb});

        auto rule = rewrite_rule("twice", "(op_add ?x ?x)", "(op_mul 2:64 ?x)");
        CHECK(count_matches(match(rule, egraph)) == 0);

        auto saturable = saturable_egraph(std::move(egraph));

        saturable.merge(ida, idb);
        saturable.rebuild();

        CHECK(count_matches(match(rule, saturable)) == 4);
    }

    TEST_CASE("nested same arguments") {
        test_graph egraph;
        auto ida  = make_node(egraph, "a:64");
        auto idb  = make_node(egraph, "b:64");
        auto idc  = make_node(egraph, "c:64");
        auto add1 = make_node(egraph, "add", {ida, idb});
        /* auto add2 = */ make_node(egraph, "add", {idc, add1});

        auto rule = rewrite_rule("twice", "(op_add ?x ?x)", "(op_mul 2:64 ?x)");
        CHECK(count_matches(match(rule, egraph)) == 0);

        auto saturable = saturable_egraph(std::move(egraph));

        saturable.merge(ida, idb);
        saturable.merge(idc, add1);
        saturable.rebuild();

        CHECK(count_matches(match(rule, saturable)) == 8);
    }

    } // test suite: eqsat::pattern-patching

} // namespace eqsat::test
