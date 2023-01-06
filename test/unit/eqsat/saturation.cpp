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

        CHECK_EQ(root1.nodes.size(), 5);
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

    TEST_CASE("multimatch union") {
        test_graph egraph;

        auto idx = make_node(egraph, "x:64");
        auto add = make_node(egraph, "add", {idx, idx});

        auto con = make_node(egraph, "2:64");
        auto mul = make_node(egraph, "mul", {idx, con});

        auto rule = rewrite_rule(
            "mul-add-equality",
            "((let A (op_add ?x ?x)) (let B (op_mul ?x 2:64)) (match $A $B))",
            "(union $A $B)"
        );

        CHECK_EQ(count_matches(match(rule, egraph)), 1);

        auto result = saturable_egraph(std::move(egraph))
            | action::match_and_apply{rule}
            | action::rebuild();

        CHECK_EQ(result.eclass(add).size(), 2);
        CHECK_EQ(result.eclass(add), result.eclass(mul));
    }

    // TEST_CASE("Constrained Contexts Rewrite")
    // {
    //   TestGraph egraph;
    //   TestGraphBuilder builder(&egraph);

    //   auto x = egraph.make_leaf("x");
    //   auto add = egraph.make_node("add", {x, x});

    //   auto con = egraph.make_leaf("2");
    //   auto mul1 = egraph.make_node("mul", {x, con});
    //   auto mul2 = egraph.make_node("mul", {x, con});

    //   egraph.make_node("CTX1", {add, mul1});
    //   egraph.make_node("CTX2", {mul2});

    //   auto addctx = contexts(egraph, add);

    //   auto any_of = [] (const auto &contexts, std::string_view target) {
    //     return std::any_of(contexts.begin(), contexts.end(), [&] (const auto &ctx) {
    //       return name(ctx) == target;
    //     });
    //   };

    //   CHECK(addctx.size() == 1);
    //   CHECK(any_of(addctx, "CTX1"));

    //   auto conctx = contexts(egraph, con);
    //   CHECK(conctx.size() == 2);
    //   CHECK(any_of(conctx, "CTX1"));
    //   CHECK(any_of(conctx, "CTX2"));

    //   auto xctx = contexts(egraph, x);
    //   CHECK(xctx.size() == 2);
    //   CHECK(any_of(xctx, "CTX1"));
    //   CHECK(any_of(xctx, "CTX2"));

    //   CHECK(contexts(egraph, add) == contexts(egraph, mul1));

    //   auto rule = TestRule(
    //     "mul-add-equality",
    //     "((let A (op_add ?x ?x):C1) (let B (op_mul ?x 2):C2) (disjoint C1 C2) (match $A $B))",
    //     "(union $A $B)"
    //   );

    //   auto matches = rule.match(egraph);
    //   CHECK(matches.size() == 1);

    //   rule.apply(egraph, builder);
    //   egraph.rebuild();

    //   CHECK(egraph.eclass(add).size() == 2);
    //   CHECK(egraph.eclass(add) == egraph.eclass(mul2));
    // }

    // TEST_CASE("Disjoint Match")
    // {
    //   TestGraph egraph;
    //   TestGraphBuilder builder(&egraph);

    //   auto a = egraph.make_leaf("a");
    //   auto b = egraph.make_leaf("b");
    //   egraph.make_node("mul", {a, b});

    //   auto rule = TestRule(
    //     "unify-multiplication",
    //     "((let A (op_mul ?a ?b)) (let B (op_mul ?c ?d)) (match $A $B))",
    //     "(union $A $B)"
    //   );

    //   auto matches = rule.match(egraph);
    //   CHECK(matches.size() == 0);

    //   rule.apply(egraph, builder);
    //   egraph.rebuild();
    // }

    // TEST_CASE("Commutative Match")
    // {
    //   TestGraph egraph;
    //   TestGraphBuilder builder(&egraph);

    //   auto a = egraph.make_leaf("a");
    //   auto b = egraph.make_leaf("b");
    //   auto m1 = egraph.make_node("mul", {a, b});

    //   auto c = egraph.make_leaf("c");
    //   auto d = egraph.make_leaf("d");
    //   auto m2 = egraph.make_node("mul", {c, d});

    //   auto rule = TestRule(
    //     "mul-add-equality",
    //     "((let A (op_mul ?a ?b)) (let B (op_mul ?c ?d)) (commutative-match $A $B))",
    //     "(union $A $B)"
    //   );

    //   auto matches = rule.match(egraph);
    //   CHECK(matches.size() == 1);

    //   rule.apply(egraph, builder);
    //   egraph.rebuild();

    //   CHECK(egraph.eclass(m1) == egraph.eclass(m2));
    // }

    // TEST_CASE("Bond nodes")
    // {
    //   TestGraph egraph;
    //   TestGraphBuilder builder(&egraph);

    //   auto a = egraph.make_leaf("a");
    //   auto b = egraph.make_leaf("b");
    //   auto c = egraph.make_leaf("c");
    //   auto d = egraph.make_leaf("d");

    //   auto mul1 = egraph.make_node("mul", {a, b});
    //   auto mul2 = egraph.make_node("mul", {c, d});

    //   egraph.make_node("CTX1", {mul1});
    //   egraph.make_node("CTX2", {mul2});

    //   auto rule = TestRule(
    //     "bond-multiplications",
    //     "((let A (op_mul ?a ?b):C1) (let B (op_mul ?c ?d):C2) (disjoint C1 C2) (commutative-match $A $B))",
    //     "(bond $A $B)"
    //   );

    //   auto matches = rule.match(egraph);
    //   CHECK(matches.size() == 1);

    //   rule.apply(egraph, builder);
    //   egraph.rebuild();

    //   CHECK(egraph.bonded({mul1, mul2}));

    //   auto bond = egraph.bonded({mul1, mul2}).value();

    //   auto bondctx = contexts(egraph, bond);
    //   CHECK(bondctx == contexts(egraph, mul1));
    //   CHECK(bondctx == contexts(egraph, mul2));
    // }

    // TEST_CASE("Variadic Match")
    // {
    //   TestGraph egraph;
    //   TestGraphBuilder builder(&egraph);

    //   auto a = egraph.make_leaf("a");
    //   auto b = egraph.make_leaf("b");
    //   auto c = egraph.make_leaf("c");
    //   auto d = egraph.make_leaf("d");
    //   auto e = egraph.make_leaf("e");
    //   auto f = egraph.make_leaf("f");

    //   auto mul1 = egraph.make_node("mul", {a, b});
    //   auto mul2 = egraph.make_node("mul", {c, d});
    //   auto mul3 = egraph.make_node("mul", {e, f});

    //   egraph.make_node("CTX1", {mul1});
    //   egraph.make_node("CTX2", {mul2});
    //   egraph.make_node("CTX3", {mul3});

    //   auto rule = TestRule(
    //     "variadic-bond-multiplications",
    //     "((let M (op_mul)) (commutative-match $M...))",
    //     "(bond $M...)"
    //   );

    //   auto matches = rule.match(egraph);
    //   CHECK_EQ(matches.size(), 1);

    //   rule.apply(egraph, builder);
    //   egraph.rebuild();

    //   CHECK(egraph.bonded({mul1, mul2, mul3}));

    //   auto bond = egraph.bonded({mul1, mul2, mul3}).value();
    //   auto bondctx = contexts(egraph, bond);
    //   CHECK(bondctx == contexts(egraph, mul1));
    //   CHECK(bondctx == contexts(egraph, mul2));
    //   CHECK(bondctx == contexts(egraph, mul3));
    // }

    // TEST_CASE("Variadic Contexts") {
    //   TestGraph egraph;
    //   TestGraphBuilder builder(&egraph);

    //   auto a = egraph.make_leaf("a");
    //   auto b = egraph.make_leaf("b");
    //   auto c = egraph.make_leaf("c");
    //   auto d = egraph.make_leaf("d");
    //   auto e = egraph.make_leaf("e");
    //   auto f = egraph.make_leaf("f");
    //   auto g = egraph.make_leaf("g");
    //   auto h = egraph.make_leaf("h");

    //   auto mul1 = egraph.make_node("mul", {a, b});
    //   auto mul2 = egraph.make_node("mul", {c, d});
    //   auto mul3 = egraph.make_node("mul", {e, f});

    //   egraph.make_node("CTX1", {mul1});
    //   egraph.make_node("CTX2", {mul2});
    //   egraph.make_node("CTX3", {mul3});

    //   auto rule = TestRule(
    //       "variadic-bond-operations",
    //       "((let M (op_mul):C) (disjoint C...) (commutative-match $M...))",
    //       "(bond $M...)");
    //   CHECK_EQ(rule.match(egraph).size(), 1);

    //   auto mul4 = egraph.make_node("mul", {g, h});
    //   egraph.make_node("CTX4", {mul3, mul4});
    //   CHECK_EQ(rule.match(egraph).size(), 1);
    //   rule.apply(egraph, builder);
    //   egraph.rebuild();
    //   egraph.dump("egraph-after.dot");
    // }

    // TEST_CASE("Variadic Unify")
    // {
    //   TestGraph egraph;
    //   TestGraphBuilder builder(&egraph);

    //   auto a = egraph.make_leaf("a");
    //   auto b = egraph.make_leaf("b");
    //   auto c = egraph.make_leaf("c");
    //   auto d = egraph.make_leaf("d");
    //   auto e = egraph.make_leaf("e");
    //   auto f = egraph.make_leaf("f");
    //   auto g = egraph.make_leaf("e");
    //   auto h = egraph.make_leaf("f");

    //   auto mul1 = egraph.make_node("mul", {a, b});
    //   auto mul2 = egraph.make_node("mul", {c, d});
    //   auto add1 = egraph.make_node("add", {e, f});
    //   auto add2 = egraph.make_node("add", {g, h});

    //   egraph.make_node("CTX1", {mul1});
    //   egraph.make_node("CTX2", {mul2});
    //   egraph.make_node("CTX3", {add1});
    //   egraph.make_node("CTX4", {add2});

    //   auto rule = TestRule(
    //     "variadic-bond-operations",
    //     "((let M (op_mul)) (let A (op_add)) (commutative-match $M... $A...))",
    //     "(union $M... $A...)"
    //   );

    //   auto matches = rule.match(egraph);
    //   CHECK_EQ(matches.size(), 1);

    //   rule.apply(egraph, builder);
    //   egraph.rebuild();

    //   CHECK(egraph.eclass(mul1) == egraph.eclass(mul2));
    //   CHECK(egraph.eclass(mul2) == egraph.eclass(add1));
    //   CHECK(egraph.eclass(add1) == egraph.eclass(add2));
    // }

    // TEST_CASE("Advice Lowering")
    // {
    //   TestGraph egraph;
    //   TestGraphBuilder builder(&egraph);

    //   auto a = egraph.make_leaf("a");
    //   auto b = egraph.make_leaf("b");
    //   auto c = egraph.make_leaf("c");
    //   auto d = egraph.make_leaf("d");

    //   auto mul1 = egraph.make_node("mul", {a, b});
    //   auto mul2 = egraph.make_node("mul", {c, d});

    //   egraph.make_node("CTX1", {mul1});
    //   egraph.make_node("CTX2", {mul2});

    //   auto rule = TestRule( "advice-and-bond",
    //     "((let Muls (op_mul):C) (disjoint C...) (commutative-match $Muls...))",
    //     "((let Bond (bond $Muls...)) (let Adviced (op_mul op_Advice op_Advice)) (union $Bond $Adviced))"
    //   );

    //   auto matches = rule.match(egraph);
    //   CHECK(matches.size() == 1);

    //   rule.apply(egraph, builder);
    //   egraph.rebuild();

    //   CHECK(egraph.bonded({mul1, mul2}));
    //   lower_advices(egraph, builder);
    // }

    } // test suite: eqsat::pattern-rewrite
} // namespace eqsat::test
