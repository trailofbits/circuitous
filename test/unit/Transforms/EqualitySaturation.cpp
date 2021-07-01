/*
 * Copyright (c) 2021, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <doctest/doctest.h>
#include <numeric>
#include <support/EGraph.hpp>
#include <circuitous/Transforms/Pattern.hpp>
#include <circuitous/Transforms/EqualitySaturation.hpp>
#include <circuitous/ADT/EGraph.hpp>

#include <fstream>

namespace circ::eqsat {

  unsigned count_matches(auto matches) {
    auto eclass_match_count = [] (unsigned sum, auto &eclass_match) {
      const auto &[id, matches] = eclass_match;
      return sum + matches.size();
    };
    return std::accumulate(matches.begin(), matches.end(), unsigned(0), eclass_match_count);
  }

  using TestRule = Rule< TestGraph >;

  TEST_SUITE("EGraph Pattern Matching")
  {
    TEST_CASE("Basic")
    {
      TestGraph egraph;

      auto x = egraph.make_leaf("x");
      auto y = egraph.make_leaf("1");
      egraph.make_node("mul", {x, y});

      SUBCASE("multiplication identity") {
        auto rule = TestRule("multiplication identity", "(op_mul ?x 1)", "(?x)");
        CHECK(count_matches(rule.match(egraph)) == 1);
      }

      SUBCASE("zero multiplication") {
        auto rule = TestRule("zero multiplication", "(op_mul ?x 0)", "(0)");
        CHECK(count_matches(rule.match(egraph)) == 0);
      }

      SUBCASE("commutativity") {
        auto rule = TestRule("commutativity multiplication", "(op_mul ?x ?y)", "(op_mul ?y ?x)");
        CHECK(count_matches(rule.match(egraph)) == 1);
      }

      SUBCASE("identity") {
        auto rule = TestRule("id", "(?x)", "(?x)");
        CHECK(count_matches(rule.match(egraph)) == 3);
      }

      SUBCASE("named commutativity") {
        auto rule = TestRule("commutativity", "((let X (?x)) (let Y (?y)) (op_mul $X $Y))", "(op_mul ?y ?x)");
        auto m = rule.match(egraph);
        CHECK(count_matches(m) == 1);
      }
    }

    TEST_CASE("Addition")
    {
      TestGraph egraph;

      auto x  = egraph.make_leaf("x");
      auto y  = egraph.make_leaf("y");
      auto a1 = egraph.make_node("add", {x, y});

      auto u  = egraph.make_leaf("u");
      auto v  = egraph.make_leaf("v");
      auto a2 = egraph.make_node("add", {u, v});

      auto rule = TestRule("commutativity", "(op_add ?x ?y)", "(op_add ?y ?x)");
      CHECK(count_matches(rule.match(egraph)) == 2);

      egraph.merge(a1, a2);
      egraph.rebuild();

      CHECK(count_matches(rule.match(egraph)) == 2);
    }

    TEST_CASE("Multilayer")
    {
      TestGraph egraph;

      auto a  = egraph.make_leaf("a");
      auto b  = egraph.make_leaf("b");
      auto z  = egraph.make_leaf("0");
      auto add1 = egraph.make_node("add", {z, a});
      egraph.make_node("add", {b, add1});

      auto rule = TestRule("addition", "(op_add ?x (op_add 0 ?y))", "(op_add ?x ?y)");
      CHECK(count_matches(rule.match(egraph)) == 1);
    }

    TEST_CASE("Same Arguments")
    {
      TestGraph egraph;

      auto a  = egraph.make_leaf("a");
      auto b  = egraph.make_leaf("b");
      egraph.make_node("add", {a, b});

      auto rule = TestRule("twice", "(op_add ?x ?x)", "(op_mul 2 ?x)");
      CHECK(count_matches(rule.match(egraph)) == 0);

      egraph.merge(a, b);
      egraph.rebuild();

      CHECK(count_matches(rule.match(egraph)) == 1);
    }
  }

  TEST_SUITE("EGraph Pattern Rewrite")
  {
    TEST_CASE("Commutativity")
    {
      TestGraph egraph;
      TestGraphBuilder builder(&egraph);

      auto x = egraph.make_leaf("x");
      auto y = egraph.make_leaf("y");
      auto a = egraph.make_node("add", {x, y});

      auto rule = TestRule("commutativity", "(op_add ?x ?y)", "(op_add ?y ?x)");

      rule.apply(egraph, builder);
      egraph.rebuild();

      auto additions = egraph.eclass(a);
      CHECK(additions.size() == 2);

      CHECK(additions.nodes[0]->children[0] == x);
      CHECK(additions.nodes[0]->children[1] == y);

      CHECK(additions.nodes[1]->children[0] == y);
      CHECK(additions.nodes[1]->children[1] == x);
    }

    TEST_CASE("Operation")
    {
      TestGraph egraph;
      TestGraphBuilder builder(&egraph);

      auto x = egraph.make_leaf("x");
      auto a = egraph.make_node("add", {x, x});

      auto rule = TestRule("addition to multiplication", "(op_add ?x ?x)", "(op_mul ?x 2)");
      CHECK(count_matches(rule.match(egraph)) == 1);

      rule.apply(egraph, builder);
      egraph.rebuild();

      auto ops = egraph.eclass(a);

      CHECK(ops.size() == 2);
      CHECK(ops.nodes[0]->term == "add");
      CHECK(ops.nodes[0]->children[0] == x);
      CHECK(ops.nodes[0]->children[1] == x);

      CHECK(ops.nodes[1]->term == "mul");
      CHECK(ops.nodes[1]->children[0] == x);
      CHECK(ops.nodes[1]->children[1] != x);
    }

    TEST_CASE("Simplification")
    {
      TestGraph egraph;
      TestGraphBuilder builder(&egraph);

      auto x = egraph.make_leaf("x");
      auto y = egraph.make_leaf("1");
      auto root = egraph.make_node("mul", {x, y});

      auto rule = TestRule("multiplicative identity", "(op_mul ?x 1)", "(?x)");
      CHECK(rule.match(egraph).size() == 1);

      rule.apply(egraph, builder);
      egraph.rebuild();

      auto ops = egraph.eclass(root);

      CHECK(ops.size() == 2);
      CHECK(ops.nodes[0]->term == "x");

      CHECK(ops.nodes[1]->term == "mul");
      CHECK(ops.nodes[1]->children[0] == x);
      CHECK(ops.nodes[1]->children[1] == y);
    }

    TEST_CASE("Chained Rules")
    {
      TestGraph egraph;
      TestGraphBuilder builder(&egraph);

      auto x = egraph.make_leaf("x");
      auto y = egraph.make_leaf("1");
      auto mul = egraph.make_node("mul", {x, y});
      auto root = egraph.make_node("add", {x, mul});

      auto imul = TestRule("multiplicative identity", "(op_mul ?x 1)", "(?x)");
      CHECK(imul.match(egraph).size() == 1);
      imul.apply(egraph, builder);

      auto atom = TestRule("twice", "(op_add ?x ?x)", "(op_mul ?x 2)");
      CHECK(atom.match(egraph).size() == 1);
      atom.apply(egraph, builder);

      egraph.rebuild();

      auto ops = egraph.eclass(root);

      CHECK(ops.size() == 2);
      CHECK(ops.nodes[0]->term == "add");
      CHECK(ops.nodes[0]->children[0] == x);
      CHECK(ops.nodes[0]->children[0] == x);

      CHECK(ops.nodes[1]->term == "mul");
      CHECK(ops.nodes[1]->children[0] == x);
      CHECK(ops.nodes[1]->children[1] != x);
    }

    TEST_CASE("Named Subexpressions")
    {
      TestGraph egraph;
      TestGraphBuilder builder(&egraph);

      auto x = egraph.make_leaf("x");
      auto y = egraph.make_leaf("y");
      auto a = egraph.make_node("add", {x, y});

      auto rule = TestRule("commutativity", "((let X (?x)) (let Y (?y)) (op_add $X $Y))", "(op_add ?y ?x)");

      rule.apply(egraph, builder);
      egraph.rebuild();

      auto additions = egraph.eclass(a);
      CHECK(additions.size() == 2);

      CHECK(additions.nodes[0]->children[0] == x);
      CHECK(additions.nodes[0]->children[1] == y);

      CHECK(additions.nodes[1]->children[0] == y);
      CHECK(additions.nodes[1]->children[1] == x);
    }

    TEST_CASE("Named Subexpressions in Rewrite")
    {
      TestGraph egraph;
      TestGraphBuilder builder(&egraph);

      auto x = egraph.make_leaf("x");
      auto y = egraph.make_leaf("y");
      auto a = egraph.make_node("add", {x, y});

      auto rule = TestRule("commutativity", "(op_add ?x ?y)", "((let X (?x)) (op_add ?y $X))");

      rule.apply(egraph, builder);
      egraph.rebuild();

      auto additions = egraph.eclass(a);
      CHECK(additions.size() == 2);

      CHECK(additions.nodes[0]->children[0] == x);
      CHECK(additions.nodes[0]->children[1] == y);

      CHECK(additions.nodes[1]->children[0] == y);
      CHECK(additions.nodes[1]->children[1] == x);
    }
  }

} // namespace circ::eqsat