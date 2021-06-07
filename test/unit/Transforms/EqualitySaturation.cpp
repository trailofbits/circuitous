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

namespace circuitous {

  unsigned count_matches(auto matches) {
    auto eclass_match_count = [] (unsigned sum, auto &eclass_match) {
      const auto &[id, matches] = eclass_match;
      return sum + matches.size();
    };
    return std::accumulate(matches.begin(), matches.end(), unsigned(0), eclass_match_count);
  }
  TEST_CASE("EGraph Pattern Matching")
  {
    using Rule = circuitous::Rule< TestGraph >;

    TestGraph egraph;

    auto idx = egraph.make_leaf("x");
    auto idy = egraph.make_leaf("1");
    egraph.make_node("mul", {idx, idy});

    auto imul = Rule("identity multiplication", "(op_mul ?x 1)", "?x");
    CHECK(imul.match(egraph).size() == 1);

    auto zmul = Rule("zero multiplication", "(op_mul ?x 0)", "0");
    CHECK(zmul.match(egraph).size() == 0);

    auto cmul = Rule("commutativity multiplication", "(op_mul ?x ?y)", "(op_mul ?y ?x)");
    CHECK(cmul.match(egraph).size() == 1);

    auto id = Rule("id", "?x", "?x");
    CHECK(id.match(egraph).size() == 3);
  }

  TEST_CASE("EGraph Pattern Matching Addition")
  {
    using Rule = circuitous::Rule< TestGraph >;

    TestGraph egraph;

    auto idx  = egraph.make_leaf("x");
    auto idy  = egraph.make_leaf("y");
    auto ida1 = egraph.make_node("add", {idx, idy});

    auto idu  = egraph.make_leaf("u");
    auto idv  = egraph.make_leaf("v");
    auto ida2 = egraph.make_node("add", {idu, idv});

    auto cadd = Rule("commutativity addition", "(op_add ?x ?y)", "(op_add ?y ?x)");
    CHECK(count_matches(cadd.match(egraph)) == 2);

    egraph.merge(ida1, ida2);
    egraph.rebuild();

    CHECK(count_matches(cadd.match(egraph)) == 2);
  }

  TEST_CASE("EGraph Pattern Matching Multilayer")
  {
    using Rule = circuitous::Rule< TestGraph >;

    TestGraph egraph;

    auto a  = egraph.make_leaf("a");
    auto b  = egraph.make_leaf("b");
    auto z  = egraph.make_leaf("0");
    auto add1 = egraph.make_node("add", {z, a});
    egraph.make_node("add", {b, add1});

    auto cadd = Rule("addition", "(op_add ?x (op_add 0 ?y))", "(op_add ?x ?y)");
    CHECK(count_matches(cadd.match(egraph)) == 1);
  }

  TEST_CASE("EGraph Match Same Classes")
  {
    using Rule = circuitous::Rule< TestGraph >;

    TestGraph egraph;

    auto a  = egraph.make_leaf("a");
    auto b  = egraph.make_leaf("b");
    egraph.make_node("add", {a, b});

    auto madd = Rule("addition to mult", "(op_add ?x ?x)", "(op_mul 2 ?x)");
    // CHECK(count_matches(madd.match(egraph)) == 0);

    egraph.merge(a, b);
    egraph.rebuild();

    egraph.dump("egraph.dot");

    auto m = madd.match(egraph);
    CHECK(count_matches(m)== 1);
  }

} // namespace circuitous