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

namespace circuitous::eqsat {

  unsigned count_matches(auto matches) {
    auto eclass_match_count = [] (unsigned sum, auto &eclass_match) {
      const auto &[id, matches] = eclass_match;
      return sum + matches.size();
    };
    return std::accumulate(matches.begin(), matches.end(), unsigned(0), eclass_match_count);
  }

  using TestRule = Rule< TestGraph >;

  TEST_CASE("EGraph Pattern Matching")
  {
    TestGraph egraph;

    auto idx = egraph.make_leaf("x");
    auto idy = egraph.make_leaf("1");
    egraph.make_node("mul", {idx, idy});

    auto imul = TestRule("identity multiplication", "(op_mul ?x 1)", "?x");
    CHECK(imul.match(egraph).size() == 1);

    auto zmul = TestRule("zero multiplication", "(op_mul ?x 0)", "0");
    CHECK(zmul.match(egraph).size() == 0);

    auto cmul = TestRule("commutativity multiplication", "(op_mul ?x ?y)", "(op_mul ?y ?x)");
    CHECK(cmul.match(egraph).size() == 1);

    auto id = TestRule("id", "?x", "?x");
    CHECK(id.match(egraph).size() == 3);
  }

  TEST_CASE("EGraph Pattern Matching Addition")
  {
    TestGraph egraph;

    auto idx  = egraph.make_leaf("x");
    auto idy  = egraph.make_leaf("y");
    auto ida1 = egraph.make_node("add", {idx, idy});

    auto idu  = egraph.make_leaf("u");
    auto idv  = egraph.make_leaf("v");
    auto ida2 = egraph.make_node("add", {idu, idv});

    auto cadd = TestRule("commutativity addition", "(op_add ?x ?y)", "(op_add ?y ?x)");
    CHECK(count_matches(cadd.match(egraph)) == 2);

    egraph.merge(ida1, ida2);
    egraph.rebuild();

    CHECK(count_matches(cadd.match(egraph)) == 2);
  }

  TEST_CASE("EGraph Pattern Matching Multilayer")
  {
    TestGraph egraph;

    auto a  = egraph.make_leaf("a");
    auto b  = egraph.make_leaf("b");
    auto z  = egraph.make_leaf("0");
    auto add1 = egraph.make_node("add", {z, a});
    egraph.make_node("add", {b, add1});

    auto cadd = TestRule("addition", "(op_add ?x (op_add 0 ?y))", "(op_add ?x ?y)");
    CHECK(count_matches(cadd.match(egraph)) == 1);
  }

  TEST_CASE("EGraph Match Same Classes")
  {
    TestGraph egraph;

    auto a  = egraph.make_leaf("a");
    auto b  = egraph.make_leaf("b");
    egraph.make_node("add", {a, b});

    auto madd = TestRule("addition to mult", "(op_add ?x ?x)", "(op_mul 2 ?x)");
    CHECK(count_matches(madd.match(egraph)) == 0);

    egraph.merge(a, b);
    egraph.rebuild();

    CHECK(count_matches(madd.match(egraph)) == 1);
  }

  TEST_CASE("EGraph Pattern Rewrite Commutativity")
  {
    TestGraph egraph;
    TestGraphBuilder builder(&egraph);

    auto idx = egraph.make_leaf("x");
    auto idy = egraph.make_leaf("y");
    auto ida = egraph.make_node("add", {idx, idy});

    auto cadd = TestRule("commutativity addition", "(op_add ?x ?y)", "(op_add ?y ?x)");

    cadd.apply(egraph, builder);
    egraph.rebuild();

    auto additions = egraph.eclass(ida);
    CHECK(additions.size() == 2);

    CHECK(additions.nodes[0]->children[0] == idx);
    CHECK(additions.nodes[0]->children[1] == idy);

    CHECK(additions.nodes[1]->children[0] == idy);
    CHECK(additions.nodes[1]->children[1] == idx);
  }

  TEST_CASE("EGraph Pattern Rewrite Operation")
  {
    TestGraph egraph;
    TestGraphBuilder builder(&egraph);

    auto idx = egraph.make_leaf("x");
    auto ida = egraph.make_node("add", {idx, idx});

    auto rule = TestRule("addition to multiplication", "(op_add ?x ?x)", "(op_mul ?x 2)");
    CHECK(count_matches(rule.match(egraph)) == 1);

    rule.apply(egraph, builder);
    egraph.rebuild();

    auto ops = egraph.eclass(ida);

    CHECK(ops.size() == 2);
    CHECK(ops.nodes[0]->term == "add");
    CHECK(ops.nodes[0]->children[0] == idx);
    CHECK(ops.nodes[0]->children[1] == idx);

    CHECK(ops.nodes[1]->term == "mul");
    CHECK(ops.nodes[1]->children[0] == idx);
    CHECK(ops.nodes[1]->children[1] != idx);
  }

} // namespace circuitous::eqsat