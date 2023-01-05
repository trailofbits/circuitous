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

    TEST_SUITE("eqsat::pattern-rewrite") {
    TEST_CASE("commutativity") {
        test_graph egraph;
        auto ida = make_node(egraph, "x:64");
        auto idb = make_node(egraph, "y:64");
        auto add = make_node(egraph, "add", {ida, idb});

        auto rule = rewrite_rule("commutativity", "(op_add ?x ?y)", "(op_add ?y ?x)");
        CHECK_EQ(count_matches(match(rule, egraph)), 1);

        auto result = saturable_egraph(std::move(egraph))
            | action::match_and_apply{rule}
            | action::rebuild();

        auto additions = result.eclass(add);
        CHECK_EQ(additions.nodes.size(), 2);

        CHECK_EQ(additions.nodes[0]->child(0), ida);
        CHECK_EQ(additions.nodes[0]->child(1), idb);

        CHECK_EQ(additions.nodes[1]->child(0), idb);
        CHECK_EQ(additions.nodes[1]->child(1), ida);

        CHECK_EQ(result.eclass(ida).parents.size(), 2);
        CHECK_EQ(result.eclass(idb).parents.size(), 2);
    }

    TEST_CASE("identity elimination") {
        test_graph egraph;
        auto ida = make_node(egraph, "x:64");
        auto idz = make_node(egraph, "0:64");
        auto add = make_node(egraph, "add", {ida, idz});

        auto rule = rewrite_rule("identity", "(op_add ?x 0:64)", "(?x)");
        CHECK_EQ(count_matches(match(rule, egraph)), 1);

        auto result = saturable_egraph(std::move(egraph))
            | action::match_and_apply{rule}
            | action::rebuild();

        auto additions = result.eclass(add);
        CHECK_EQ(additions.nodes.size(), 2);
    }

    TEST_CASE("nested identity elimination") {
        test_graph egraph;
        auto ida = make_node(egraph, "x:64");
        auto idb = make_node(egraph, "y:64");
        auto idz = make_node(egraph, "0:64");
        auto add1 = make_node(egraph, "add", {idz, ida});
        auto add2 = make_node(egraph, "add", {idb, add1});

        auto rule = rewrite_rule("identity", "(op_add ?x (op_add 0:64 ?y))", "(op_add ?x ?y)");
        CHECK_EQ(count_matches(match(rule, egraph)), 1);

        auto result = saturable_egraph(std::move(egraph))
            | action::match_and_apply{rule}
            | action::rebuild();

        auto additions = result.eclass(add2);
        CHECK_EQ(additions.nodes.size(), 2);
    }

    TEST_CASE("chain identity with commutativity") {
        test_graph egraph;
        auto ida = make_node(egraph, "a:64");
        auto idz = make_node(egraph, "0:64");
        auto add1 = make_node(egraph, "add", {ida, idz});
        auto add2 = make_node(egraph, "add", {add1, idz});

        auto identity      = rewrite_rule("identity", "(op_add 0:64 ?x)", "(?x)");
        auto commutativity = rewrite_rule("commutativity", "(op_add ?x ?y)", "(op_add ?y ?x)");
        CHECK_EQ(count_matches(match(commutativity, egraph)), 2);
        CHECK_EQ(count_matches(match(identity, egraph)), 0);

        auto result = saturable_egraph(std::move(egraph))
            | action::match_and_apply{commutativity}
            | action::match_and_apply{identity}
            | action::rebuild();

        auto root1 = result.eclass(add2);

        CHECK_EQ(root1.nodes.size(), 4);
        CHECK_EQ(result.eclass(add2), result.eclass(add1));

        result = saturable_egraph(std::move(result))
            | action::match_and_apply{identity}
            | action::rebuild();

        auto root2 = result.eclass(add2);

        CHECK_EQ(root2.nodes.size(), 5);
        CHECK_EQ(result.eclass(ida), result.eclass(add2));
        CHECK_EQ(result.eclass(add1), result.eclass(add2));
    }

    TEST_CASE("trivial and") {
        test_graph egraph;
        auto ida = make_node(egraph, "1:1");
        auto idb = make_node(egraph, "1:1");
        auto op = make_node(egraph, "and", {ida, idb});

        auto identity = rewrite_rule("identity", "(op_and 1:1 1:1)", "(1:1)");
        CHECK_EQ(count_matches(match(identity, egraph)), 1);

        auto result = saturable_egraph(std::move(egraph))
            | action::match_and_apply{identity}
            | action::rebuild();

        auto root = result.eclass(op);
        CHECK_EQ(root.nodes.size(), 2);
    }

    TEST_CASE("operation synthesis") {
        test_graph egraph;

        auto idx = make_node(egraph, "x:64");
        auto op  = make_node(egraph, "add", {idx, idx});

        auto rule = rewrite_rule("addition to multiplication", "(op_add ?x:64 ?x:64)", "(op_mul ?x:64 2:64)");
        CHECK_EQ(count_matches(match(rule, egraph)), 1);

        auto result = saturable_egraph(std::move(egraph))
            | action::match_and_apply{rule}
            | action::rebuild();

        auto root = result.eclass(op);
        CHECK_EQ(root.nodes.size(), 2);
    }

    TEST_CASE("named subexpression") {
        test_graph egraph;

        auto idx = make_node(egraph, "x:64");
        auto idy = make_node(egraph, "y:64");
        auto op  = make_node(egraph, "add", {idx, idy});

        auto rule = rewrite_rule(
            "commutativity",
            "((let X (?x)) (let Y (?y)) (op_add $X $Y))",
            "(op_add ?y ?x)"
        );

        CHECK_EQ(count_matches(match(rule, egraph)), 1);

        auto result = saturable_egraph(std::move(egraph))
            | action::match_and_apply{rule}
            | action::rebuild();

        auto root = result.eclass(op);
        CHECK_EQ(root.nodes.size(), 2);

        CHECK_EQ(root.nodes[0]->child(0), idx);
        CHECK_EQ(root.nodes[0]->child(1), idy);
        CHECK_EQ(root.nodes[1]->child(0), idy);
        CHECK_EQ(root.nodes[1]->child(1), idx);
    }

    TEST_CASE("named subexpression rewrite") {
        test_graph egraph;

        auto idx = make_node(egraph, "x:64");
        auto idy = make_node(egraph, "y:64");
        auto op  = make_node(egraph, "add", {idx, idy});

        auto rule = rewrite_rule(
            "commutativity",
            "(op_add ?x ?y)",
            "((let X (?x)) (op_add ?y $X))"
        );

        CHECK_EQ(count_matches(match(rule, egraph)), 1);

        auto result = saturable_egraph(std::move(egraph))
            | action::match_and_apply{rule}
            | action::rebuild();

        auto root = result.eclass(op);
        CHECK_EQ(root.nodes.size(), 2);

        CHECK_EQ(root.nodes[0]->child(0), idx);
        CHECK_EQ(root.nodes[0]->child(1), idy);
        CHECK_EQ(root.nodes[1]->child(0), idy);
        CHECK_EQ(root.nodes[1]->child(1), idx);
    }

    // TEST_CASE("multimatch union") {
    //     test_graph egraph;

    //     auto idx = make_node(egraph, "x:64");
    //     /* auto add = */ make_node(egraph, "add", {idx, idx});

    //     auto con = make_node(egraph, "2:64");
    //     /* auto mul = */ make_node(egraph, "mul", {idx, con});

    //     auto rule = rewrite_rule(
    //         "mul-add-equality",
    //         "((let A (op_add ?x ?x)) (let B (op_mul ?x 2:64)) (match $A $B))",
    //         "(union $A $B)"
    //     );

    //     CHECK_EQ(count_matches(match(rule, egraph)), 1);
    // }

    } // test suite: eqsat::pattern-rewrite
} // namespace eqsat::test
