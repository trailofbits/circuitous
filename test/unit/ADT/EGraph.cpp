/*
 * Copyright (c) 2021, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <doctest/doctest.h>
#include <support/EGraph.hpp>
#include <circuitous/ADT/EGraph.hpp>

#include <fstream>

namespace circ::eqsat {

  TEST_CASE("EGraph Merge Leaf Nodes")
  {
    TestGraph egraph;

    auto idx = egraph.make_leaf("x");
    auto idy = egraph.make_leaf("y");
    auto ida = egraph.make_node("+", {idx, idy});

    auto midx = egraph.merge(idx, idy);
    egraph.rebuild();

    CHECK(egraph.find(idx) == egraph.find(idy));
    CHECK(egraph.find(idx) != egraph.find(ida));

    CHECK(egraph.classes().size() == 2);
    CHECK(egraph.eclass(idx) == egraph.eclass(idy));

    CHECK(egraph.eclass(idx).parents.size() == 1);
    CHECK(egraph.eclass(midx).size() == 2);
  }


  TEST_CASE("EGraph Merge Internal Nodes")
  {
    TestGraph egraph;

    auto idx = egraph.make_leaf("x");
    auto idy = egraph.make_leaf("y");
    auto idz = egraph.make_leaf("z");
    auto idp = egraph.make_node("+", {idx, idy});
    auto idm = egraph.make_node("*", {idy, idz});
    auto ids = egraph.make_node("-", {idp, idm});

    egraph.merge(idp, idm);
    egraph.rebuild();

    CHECK(egraph.find(idp) == egraph.find(idm));
    CHECK(egraph.find(idp) != egraph.find(ids));
    CHECK(egraph.find(idp) != egraph.find(idx));
    CHECK(egraph.find(idp) != egraph.find(idy));
    CHECK(egraph.find(idp) != egraph.find(idz));

    CHECK(egraph.classes().size() == 5);
    CHECK(egraph.eclass(idp) == egraph.eclass(idm));

    CHECK(egraph.eclass(idp).parents.size() == 1);
    CHECK(egraph.eclass(idp).size() == 2);
  }

  TEST_CASE("EGraph Deffer Multiple Merges")
  {
    TestGraph egraph;

    auto idx = egraph.make_leaf("x");
    auto idy = egraph.make_leaf("y");
    auto idz = egraph.make_leaf("z");
    auto idp = egraph.make_node("+", {idx, idy});
    auto idm = egraph.make_node("*", {idy, idz});
    auto ids = egraph.make_node("-", {idp, idm});

    egraph.merge(idp, idm);
    egraph.merge(idx, idy);
    egraph.merge(idz, idy);

    egraph.rebuild();

    CHECK(egraph.find(idp) == egraph.find(idm));
    CHECK(egraph.find(idx) == egraph.find(idy));
    CHECK(egraph.find(idx) == egraph.find(idz));

    CHECK(egraph.find(idp) != egraph.find(ids));
    CHECK(egraph.find(idp) != egraph.find(idx));
    CHECK(egraph.find(idx) != egraph.find(idp));

    CHECK(egraph.classes().size() == 3);
    CHECK(egraph.eclass(idp) == egraph.eclass(idm));
    CHECK(egraph.eclass(idx) == egraph.eclass(idy));
    CHECK(egraph.eclass(idx) == egraph.eclass(idz));

    CHECK(egraph.eclass(idp).parents.size() == 1);
    CHECK(egraph.eclass(idx).parents.size() == 1);

    CHECK(egraph.eclass(ids).size() == 1);
    CHECK(egraph.eclass(idp).size() == 2);
    CHECK(egraph.eclass(idx).size() == 3);
  }

  TEST_CASE("EGraph Merge at Different layers")
  {
    TestGraph egraph;

    auto idx = egraph.make_leaf("x");
    auto idy = egraph.make_leaf("y");
    auto idz = egraph.make_leaf("z");
    auto idp = egraph.make_node("+", {idx, idy});
    auto idm = egraph.make_node("*", {idy, idz});
    auto ids = egraph.make_node("-", {idp, idm});

    egraph.merge(ids, idx);
    egraph.merge(idx, idy);
    egraph.merge(idz, idy);

    egraph.rebuild();

    CHECK(egraph.find(idx) == egraph.find(idy));
    CHECK(egraph.find(idx) == egraph.find(idz));
    CHECK(egraph.find(idx) == egraph.find(ids));

    CHECK(egraph.find(idp) != egraph.find(idm));
    CHECK(egraph.find(idp) != egraph.find(ids));

    CHECK(egraph.classes().size() == 3);
    CHECK(egraph.eclass(idx) == egraph.eclass(ids));
    CHECK(egraph.eclass(idx) == egraph.eclass(idy));
    CHECK(egraph.eclass(idx) == egraph.eclass(idz));

    CHECK(egraph.eclass(idp).parents.size() == 1);
    CHECK(egraph.eclass(idx).parents.size() == 2);

    CHECK(egraph.eclass(ids).size() == 4);
    CHECK(egraph.eclass(idp).size() == 1);
    CHECK(egraph.eclass(idm).size() == 1);
  }
} // namespace circ::eqsat
