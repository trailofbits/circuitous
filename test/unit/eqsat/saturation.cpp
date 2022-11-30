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
        CHECK(count_matches(match(rule, egraph)) == 1);

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
        CHECK(count_matches(match(rule, egraph)) == 1);

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
        CHECK(count_matches(match(rule, egraph)) == 1);

        auto result = saturable_egraph(std::move(egraph))
            | action::match_and_apply{rule}
            | action::rebuild();

        auto additions = result.eclass(add2);
        CHECK_EQ(additions.nodes.size(), 2);
    }

    TEST_CASE("chain identity with commutativity") {
        test_graph egraph;
        auto ida = make_node(egraph, "x:64");
        auto idb = make_node(egraph, "y:64");
        auto idz = make_node(egraph, "0:64");
        auto add1 = make_node(egraph, "add", {idz, ida});
        /* auto add2 = */ make_node(egraph, "add", {idb, add1});

        auto identity      = rewrite_rule("identity", "(op_add 0:64 ?x))", "(?x)");
        auto commutativity = rewrite_rule("commutativity", "(op_add ?x ?y)", "(op_add ?y ?x)");
        // CHECK(count_matches(match(commutativity, egraph)) == 2);
        // CHECK(count_matches(match(identity, egraph)) == 1);

        eqsat::to_dot(egraph, "before.dot");

        // auto result = saturable_egraph(std::move(egraph))
        //     | action::match_and_apply{commutativity}
        //     | action::match_and_apply{identity}
        //     | action::rebuild();

        // eqsat::to_dot(result, "test.dot");
    }
    } // test suite: eqsat::pattern-rewrite
} // namespace eqsat::test
