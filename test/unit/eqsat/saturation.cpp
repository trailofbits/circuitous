/*
 * Copyright (c) 2022, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <doctest/doctest.h>

#include <eqsat/pattern/rewrite_rule.hpp>
#include <eqsat/algo/saturation_graph.hpp>
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

        auto saturable = saturable_egraph(std::move(egraph));
        auto result = match_and_apply(std::move(saturable), rule);
        result.rebuild();

        auto additions = result.eclass(add);
        CHECK_EQ(additions.nodes.size(), 2);

        CHECK_EQ(additions.nodes[0]->child(0), ida);
        CHECK_EQ(additions.nodes[0]->child(1), idb);

        CHECK_EQ(additions.nodes[1]->child(0), idb);
        CHECK_EQ(additions.nodes[1]->child(1), ida);

        CHECK_EQ(result.eclass(ida).parents.size(), 2);
        CHECK_EQ(result.eclass(idb).parents.size(), 2);
    }
    } // test suite: eqsat::pattern-rewrite
} // namespace eqsat::test
