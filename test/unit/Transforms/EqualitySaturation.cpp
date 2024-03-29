/*
 * Copyright (c) 2021, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

// #include <doctest/doctest.h>

// #include <circuitous/ADT/EGraph.hpp>
// #include <circuitous/Transforms/EqualitySaturation.hpp>
// #include <circuitous/Transforms/eqsat/pattern.hpp>
// #include <cstddef>
// #include <fstream>
// #include <numeric>
// #include <support/EGraph.hpp>

// namespace circ {

//   auto count_matches(const Matches &matches)
//   {
//     auto count = [] (auto res, const auto &match) { return res + match.substitutions.size(); };
//     return std::accumulate(matches.begin(), matches.end(), std::size_t(0), count);
//   }

//   using TestRule = Rule< TestGraph >;

//   TEST_SUITE("EGraph Pattern Matching")
//   {
//     TEST_CASE("Basic")
//     {
//       TestGraph egraph;

//       auto x = egraph.make_leaf("x");
//       auto y = egraph.make_leaf("1");
//       egraph.make_node("mul", {x, y});

//       SUBCASE("multiplication identity") {
//         auto rule = TestRule("multiplication identity", "(op_mul ?x 1)", "(?x)");
//         CHECK(count_matches(rule.match(egraph)) == 1);
//       }

//       SUBCASE("zero multiplication") {
//         auto rule = TestRule("zero multiplication", "(op_mul ?x 0)", "(0)");
//         CHECK(count_matches(rule.match(egraph)) == 0);
//       }

//       SUBCASE("commutativity") {
//         auto rule = TestRule("commutativity multiplication", "(op_mul ?x ?y)", "(op_mul ?y ?x)");
//         CHECK(count_matches(rule.match(egraph)) == 1);
//       }

//       SUBCASE("identity") {
//         auto rule = TestRule("id", "(?x)", "(?x)");
//         CHECK(count_matches(rule.match(egraph)) == 3);
//       }

//       SUBCASE("named commutativity") {
//         auto rule = TestRule("commutativity", "((let X (?x)) (let Y (?y)) (op_mul $X $Y))", "(op_mul ?y ?x)");
//         auto m = rule.match(egraph);
//         CHECK(count_matches(m) == 1);
//       }
//     }

//     TEST_CASE("Addition")
//     {
//       TestGraph egraph;

//       auto x  = egraph.make_leaf("x");
//       auto y  = egraph.make_leaf("y");
//       auto a1 = egraph.make_node("add", {x, y});

//       auto u  = egraph.make_leaf("u");
//       auto v  = egraph.make_leaf("v");
//       auto a2 = egraph.make_node("add", {u, v});

//       auto rule = TestRule("commutativity", "(op_add ?x ?y)", "(op_add ?y ?x)");
//       CHECK(count_matches(rule.match(egraph)) == 2);

//       egraph.merge(a1, a2);
//       egraph.rebuild();

//       auto matches = rule.match(egraph);
//       auto c = count_matches(matches);
//       CHECK(c == 2);
//     }

//     TEST_CASE("Multilayer")
//     {
//       TestGraph egraph;

//       auto a  = egraph.make_leaf("a");
//       auto b  = egraph.make_leaf("b");
//       auto z  = egraph.make_leaf("0");
//       auto add1 = egraph.make_node("add", {z, a});
//       egraph.make_node("add", {b, add1});

//       auto rule = TestRule("addition", "(op_add ?x (op_add 0 ?y))", "(op_add ?x ?y)");
//       CHECK(count_matches(rule.match(egraph)) == 1);
//     }

//     TEST_CASE("Same Arguments")
//     {
//       TestGraph egraph;

//       auto a  = egraph.make_leaf("a");
//       auto b  = egraph.make_leaf("b");
//       egraph.make_node("add", {a, b});

//       auto rule = TestRule("twice", "(op_add ?x ?x)", "(op_mul 2 ?x)");
//       CHECK(count_matches(rule.match(egraph)) == 0);

//       egraph.merge(a, b);
//       egraph.rebuild();

//       CHECK(count_matches(rule.match(egraph)) == 1);
//     }
//   }

//   TEST_SUITE("EGraph Pattern Rewrite")
//   {
//     TEST_CASE("Commutativity")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto x = egraph.make_leaf("x");
//       auto y = egraph.make_leaf("y");
//       auto a = egraph.make_node("add", {x, y});

//       auto rule = TestRule("commutativity", "(op_add ?x ?y)", "(op_add ?y ?x)");

//       rule.apply(egraph, builder);
//       egraph.rebuild();

//       auto additions = egraph.eclass(a);
//       CHECK(additions.size() == 2);

//       CHECK(additions.nodes[0]->child(0) == x);
//       CHECK(additions.nodes[0]->child(1) == y);

//       CHECK(additions.nodes[1]->child(0) == y);
//       CHECK(additions.nodes[1]->child(1) == x);
//     }

//     TEST_CASE("Operation")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto x = egraph.make_leaf("x");
//       auto a = egraph.make_node("add", {x, x});

//       auto rule = TestRule("addition to multiplication", "(op_add ?x ?x)", "(op_mul ?x 2)");
//       CHECK(count_matches(rule.match(egraph)) == 1);

//       rule.apply(egraph, builder);
//       egraph.rebuild();

//       auto ops = egraph.eclass(a);

//       CHECK(ops.size() == 2);
//       CHECK(ops.nodes[0]->data() == "add");
//       CHECK(ops.nodes[0]->child(0) == x);
//       CHECK(ops.nodes[0]->child(1) == x);

//       CHECK(ops.nodes[1]->data() == "mul");
//       CHECK(ops.nodes[1]->child(0) == x);
//       CHECK(ops.nodes[1]->child(1) != x);
//     }

//     TEST_CASE("Simplification")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto x = egraph.make_leaf("x");
//       auto y = egraph.make_leaf("1");
//       auto root = egraph.make_node("mul", {x, y});

//       auto rule = TestRule("multiplicative identity", "(op_mul ?x 1)", "(?x)");
//       CHECK(rule.match(egraph).size() == 1);

//       rule.apply(egraph, builder);
//       egraph.rebuild();

//       auto ops = egraph.eclass(root);

//       CHECK(ops.size() == 2);
//       CHECK(ops.nodes[0]->data() == "x");

//       CHECK(ops.nodes[1]->data() == "mul");
//       CHECK(ops.nodes[1]->child(0) == x);
//       CHECK(ops.nodes[1]->child(1) == y);
//     }

//     TEST_CASE("Chained Rules")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto x = egraph.make_leaf("x");
//       auto y = egraph.make_leaf("1");
//       auto mul = egraph.make_node("mul", {x, y});
//       auto root = egraph.make_node("add", {x, mul});

//       auto imul = TestRule("multiplicative identity", "(op_mul ?x 1)", "(?x)");
//       CHECK(imul.match(egraph).size() == 1);
//       imul.apply(egraph, builder);

//       auto atom = TestRule("twice", "(op_add ?x ?x)", "(op_mul ?x 2)");
//       CHECK(atom.match(egraph).size() == 1);
//       atom.apply(egraph, builder);

//       egraph.rebuild();

//       auto ops = egraph.eclass(root);

//       CHECK(ops.size() == 2);
//       CHECK(ops.nodes[0]->data() == "add");
//       CHECK(ops.nodes[0]->child(0) == x);
//       CHECK(ops.nodes[0]->child(1) == x);

//       CHECK(ops.nodes[1]->data() == "mul");
//       CHECK(ops.nodes[1]->child(0) == x);
//       CHECK(ops.nodes[1]->child(1) != x);
//     }

//     TEST_CASE("Named Subexpressions")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto x = egraph.make_leaf("x");
//       auto y = egraph.make_leaf("y");
//       auto a = egraph.make_node("add", {x, y});

//       auto rule = TestRule("commutativity", "((let X (?x)) (let Y (?y)) (op_add $X $Y))", "(op_add ?y ?x)");

//       rule.apply(egraph, builder);
//       egraph.rebuild();

//       auto additions = egraph.eclass(a);
//       CHECK(additions.size() == 2);

//       CHECK(additions.nodes[0]->child(0) == x);
//       CHECK(additions.nodes[0]->child(1) == y);

//       CHECK(additions.nodes[1]->child(0) == y);
//       CHECK(additions.nodes[1]->child(1) == x);
//     }

//     TEST_CASE("Multi-match Union")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto x = egraph.make_leaf("x");
//       auto add = egraph.make_node("add", {x, x});

//       auto con = egraph.make_leaf("2");
//       auto mul = egraph.make_node("mul", {x, con});

//       auto y = egraph.make_leaf("x");
//       egraph.make_node("mul", {y, con});

//       auto rule = TestRule( "mul-add-equality"
//                 , "((let A (op_add ?x ?x)) (let B (op_mul ?x 2)) (match $A $B))"
//                 , "(union $A $B)"
//       );

//       auto matches = rule.match(egraph);
//       CHECK(matches.size() == 1);

//       rule.apply(egraph, builder);
//       egraph.rebuild();

//       CHECK(egraph.eclass(add).size() == 2);
//       CHECK(egraph.eclass(add) == egraph.eclass(mul));
//     }

//     TEST_CASE("Named Subexpressions in Rewrite")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto x = egraph.make_leaf("x");
//       auto y = egraph.make_leaf("y");
//       auto a = egraph.make_node("add", {x, y});

//       auto rule = TestRule("commutativity", "(op_add ?x ?y)", "((let X (?x)) (op_add ?y $X))");

//       rule.apply(egraph, builder);
//       egraph.rebuild();

//       auto additions = egraph.eclass(a);
//       CHECK(additions.size() == 2);

//       CHECK(additions.nodes[0]->child(0) == x);
//       CHECK(additions.nodes[0]->child(1) == y);

//       CHECK(additions.nodes[1]->child(0) == y);
//       CHECK(additions.nodes[1]->child(1) == x);
//     }

//     TEST_CASE("Constrained Contexts Rewrite")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto x = egraph.make_leaf("x");
//       auto add = egraph.make_node("add", {x, x});

//       auto con = egraph.make_leaf("2");
//       auto mul1 = egraph.make_node("mul", {x, con});
//       auto mul2 = egraph.make_node("mul", {x, con});

//       egraph.make_node("CTX1", {add, mul1});
//       egraph.make_node("CTX2", {mul2});

//       auto addctx = contexts(egraph, add);

//       auto any_of = [] (const auto &contexts, std::string_view target) {
//         return std::any_of(contexts.begin(), contexts.end(), [&] (const auto &ctx) {
//           return name(ctx) == target;
//         });
//       };

//       CHECK(addctx.size() == 1);
//       CHECK(any_of(addctx, "CTX1"));

//       auto conctx = contexts(egraph, con);
//       CHECK(conctx.size() == 2);
//       CHECK(any_of(conctx, "CTX1"));
//       CHECK(any_of(conctx, "CTX2"));

//       auto xctx = contexts(egraph, x);
//       CHECK(xctx.size() == 2);
//       CHECK(any_of(xctx, "CTX1"));
//       CHECK(any_of(xctx, "CTX2"));

//       CHECK(contexts(egraph, add) == contexts(egraph, mul1));

//       auto rule = TestRule(
//         "mul-add-equality",
//         "((let A (op_add ?x ?x):C1) (let B (op_mul ?x 2):C2) (disjoint C1 C2) (match $A $B))",
//         "(union $A $B)"
//       );

//       auto matches = rule.match(egraph);
//       CHECK(matches.size() == 1);

//       rule.apply(egraph, builder);
//       egraph.rebuild();

//       CHECK(egraph.eclass(add).size() == 2);
//       CHECK(egraph.eclass(add) == egraph.eclass(mul2));
//     }

//     TEST_CASE("Disjoint Match")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto a = egraph.make_leaf("a");
//       auto b = egraph.make_leaf("b");
//       egraph.make_node("mul", {a, b});

//       auto rule = TestRule(
//         "unify-multiplication",
//         "((let A (op_mul ?a ?b)) (let B (op_mul ?c ?d)) (match $A $B))",
//         "(union $A $B)"
//       );

//       auto matches = rule.match(egraph);
//       CHECK(matches.size() == 0);

//       rule.apply(egraph, builder);
//       egraph.rebuild();
//     }

//     TEST_CASE("Commutative Match")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto a = egraph.make_leaf("a");
//       auto b = egraph.make_leaf("b");
//       auto m1 = egraph.make_node("mul", {a, b});

//       auto c = egraph.make_leaf("c");
//       auto d = egraph.make_leaf("d");
//       auto m2 = egraph.make_node("mul", {c, d});

//       auto rule = TestRule(
//         "mul-add-equality",
//         "((let A (op_mul ?a ?b)) (let B (op_mul ?c ?d)) (commutative-match $A $B))",
//         "(union $A $B)"
//       );

//       auto matches = rule.match(egraph);
//       CHECK(matches.size() == 1);

//       rule.apply(egraph, builder);
//       egraph.rebuild();

//       CHECK(egraph.eclass(m1) == egraph.eclass(m2));
//     }

//     TEST_CASE("Bond nodes")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto a = egraph.make_leaf("a");
//       auto b = egraph.make_leaf("b");
//       auto c = egraph.make_leaf("c");
//       auto d = egraph.make_leaf("d");

//       auto mul1 = egraph.make_node("mul", {a, b});
//       auto mul2 = egraph.make_node("mul", {c, d});

//       egraph.make_node("CTX1", {mul1});
//       egraph.make_node("CTX2", {mul2});

//       auto rule = TestRule(
//         "bond-multiplications",
//         "((let A (op_mul ?a ?b):C1) (let B (op_mul ?c ?d):C2) (disjoint C1 C2) (commutative-match $A $B))",
//         "(bond $A $B)"
//       );

//       auto matches = rule.match(egraph);
//       CHECK(matches.size() == 1);

//       rule.apply(egraph, builder);
//       egraph.rebuild();

//       CHECK(egraph.bonded({mul1, mul2}));

//       auto bond = egraph.bonded({mul1, mul2}).value();

//       auto bondctx = contexts(egraph, bond);
//       CHECK(bondctx == contexts(egraph, mul1));
//       CHECK(bondctx == contexts(egraph, mul2));
//     }

//     TEST_CASE("Variadic Match")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto a = egraph.make_leaf("a");
//       auto b = egraph.make_leaf("b");
//       auto c = egraph.make_leaf("c");
//       auto d = egraph.make_leaf("d");
//       auto e = egraph.make_leaf("e");
//       auto f = egraph.make_leaf("f");

//       auto mul1 = egraph.make_node("mul", {a, b});
//       auto mul2 = egraph.make_node("mul", {c, d});
//       auto mul3 = egraph.make_node("mul", {e, f});

//       egraph.make_node("CTX1", {mul1});
//       egraph.make_node("CTX2", {mul2});
//       egraph.make_node("CTX3", {mul3});

//       auto rule = TestRule(
//         "variadic-bond-multiplications",
//         "((let M (op_mul)) (commutative-match $M...))",
//         "(bond $M...)"
//       );

//       auto matches = rule.match(egraph);
//       CHECK_EQ(matches.size(), 1);

//       rule.apply(egraph, builder);
//       egraph.rebuild();

//       CHECK(egraph.bonded({mul1, mul2, mul3}));

//       auto bond = egraph.bonded({mul1, mul2, mul3}).value();
//       auto bondctx = contexts(egraph, bond);
//       CHECK(bondctx == contexts(egraph, mul1));
//       CHECK(bondctx == contexts(egraph, mul2));
//       CHECK(bondctx == contexts(egraph, mul3));
//     }

//     TEST_CASE("Variadic Contexts") {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto a = egraph.make_leaf("a");
//       auto b = egraph.make_leaf("b");
//       auto c = egraph.make_leaf("c");
//       auto d = egraph.make_leaf("d");
//       auto e = egraph.make_leaf("e");
//       auto f = egraph.make_leaf("f");
//       auto g = egraph.make_leaf("g");
//       auto h = egraph.make_leaf("h");

//       auto mul1 = egraph.make_node("mul", {a, b});
//       auto mul2 = egraph.make_node("mul", {c, d});
//       auto mul3 = egraph.make_node("mul", {e, f});

//       egraph.make_node("CTX1", {mul1});
//       egraph.make_node("CTX2", {mul2});
//       egraph.make_node("CTX3", {mul3});

//       auto rule = TestRule(
//           "variadic-bond-operations",
//           "((let M (op_mul):C) (disjoint C...) (commutative-match $M...))",
//           "(bond $M...)");
//       CHECK_EQ(rule.match(egraph).size(), 1);

//       auto mul4 = egraph.make_node("mul", {g, h});
//       egraph.make_node("CTX4", {mul3, mul4});
//       CHECK_EQ(rule.match(egraph).size(), 1);
//       rule.apply(egraph, builder);
//       egraph.rebuild();
//       egraph.dump("egraph-after.dot");
//     }

//     TEST_CASE("Variadic Unify")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto a = egraph.make_leaf("a");
//       auto b = egraph.make_leaf("b");
//       auto c = egraph.make_leaf("c");
//       auto d = egraph.make_leaf("d");
//       auto e = egraph.make_leaf("e");
//       auto f = egraph.make_leaf("f");
//       auto g = egraph.make_leaf("e");
//       auto h = egraph.make_leaf("f");

//       auto mul1 = egraph.make_node("mul", {a, b});
//       auto mul2 = egraph.make_node("mul", {c, d});
//       auto add1 = egraph.make_node("add", {e, f});
//       auto add2 = egraph.make_node("add", {g, h});

//       egraph.make_node("CTX1", {mul1});
//       egraph.make_node("CTX2", {mul2});
//       egraph.make_node("CTX3", {add1});
//       egraph.make_node("CTX4", {add2});

//       auto rule = TestRule(
//         "variadic-bond-operations",
//         "((let M (op_mul)) (let A (op_add)) (commutative-match $M... $A...))",
//         "(union $M... $A...)"
//       );

//       auto matches = rule.match(egraph);
//       CHECK_EQ(matches.size(), 1);

//       rule.apply(egraph, builder);
//       egraph.rebuild();

//       CHECK(egraph.eclass(mul1) == egraph.eclass(mul2));
//       CHECK(egraph.eclass(mul2) == egraph.eclass(add1));
//       CHECK(egraph.eclass(add1) == egraph.eclass(add2));
//     }

//     TEST_CASE("Advice Lowering")
//     {
//       TestGraph egraph;
//       TestGraphBuilder builder(&egraph);

//       auto a = egraph.make_leaf("a");
//       auto b = egraph.make_leaf("b");
//       auto c = egraph.make_leaf("c");
//       auto d = egraph.make_leaf("d");

//       auto mul1 = egraph.make_node("mul", {a, b});
//       auto mul2 = egraph.make_node("mul", {c, d});

//       egraph.make_node("CTX1", {mul1});
//       egraph.make_node("CTX2", {mul2});

//       auto rule = TestRule( "advice-and-bond",
//         "((let Muls (op_mul):C) (disjoint C...) (commutative-match $Muls...))",
//         "((let Bond (bond $Muls...)) (let Adviced (op_mul op_Advice op_Advice)) (union $Bond $Adviced))"
//       );

//       auto matches = rule.match(egraph);
//       CHECK(matches.size() == 1);

//       rule.apply(egraph, builder);
//       egraph.rebuild();

//       CHECK(egraph.bonded({mul1, mul2}));
//       lower_advices(egraph, builder);
//     }
//   }

// } // namespace circ
