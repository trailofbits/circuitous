/*
 * Copyright (c) 2022, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <doctest/doctest.h>
#include <eqsat/algo/saturation_graph.hpp>
#include <eqsat/core/egraph.hpp>

#include <support/egraph.hpp>

namespace eqsat::test
{

    TEST_SUITE("eqsat::building-and-merging") {
    TEST_CASE( "EGraph Merge Leaf Nodes" )
    {
        test_graph egraph;

        auto idx = make_node(egraph, "x");
        auto idy = make_node(egraph, "y");
        auto ida = make_node(egraph, "+", {idx, idy});

        auto saturable = saturable_egraph(std::move(egraph));

        saturable.merge( idx, idy );
        saturable.rebuild();

        CHECK( saturable.find( idx ) == saturable.find( idy ) );
        CHECK( saturable.find( idx ) != saturable.find( ida ) );

        CHECK( saturable.num_of_eclasses() == 2 );
        CHECK( saturable.eclass( idx ) == saturable.eclass( idy ) );

        // CHECK(egraph.eclass(idx).parents.size() == 1);
        // CHECK(egraph.eclass(midx).size() == 2);
    }

    TEST_CASE( "EGraph Merge Internal Nodes" )
    {
        test_graph egraph;

        auto idx = make_node(egraph, "x");
        auto idy = make_node(egraph, "y");
        auto idz = make_node(egraph, "z");
        auto idp = make_node(egraph, "+", {idx, idy});
        auto idm = make_node(egraph, "*", {idy, idz});
        auto ids = make_node(egraph, "-", {idp, idm});

        auto saturable = saturable_egraph(std::move(egraph));

        saturable.merge( idp, idm );
        saturable.rebuild();

        CHECK( saturable.find( idp ) == saturable.find( idm ) );
        CHECK( saturable.find( idp ) != saturable.find( ids ) );
        CHECK( saturable.find( idp ) != saturable.find( idx ) );
        CHECK( saturable.find( idp ) != saturable.find( idy ) );
        CHECK( saturable.find( idp ) != saturable.find( idz ) );

        CHECK( saturable.num_of_eclasses() == 5 );
        CHECK( saturable.eclass( idp ) == saturable.eclass( idm ) );

        // CHECK(saturable.eclass(idp).parents.size() == 1);
        // CHECK(saturable.eclass(idp).size() == 2);
    }

    TEST_CASE( "EGraph Deffer Multiple Merges" )
    {
        test_graph egraph;

        auto idx = make_node(egraph, "x");
        auto idy = make_node(egraph, "y");
        auto idz = make_node(egraph, "z");
        auto idp = make_node(egraph, "+", {idx, idy});
        auto idm = make_node(egraph, "*", {idy, idz});
        auto ids = make_node(egraph, "-", {idp, idm});

        auto saturable = saturable_egraph(std::move( egraph ));

        saturable.merge( idp, idm );
        saturable.merge( idx, idy );
        saturable.merge( idz, idy );

        saturable.rebuild();

        CHECK( saturable.find( idp ) == saturable.find( idm ) );
        CHECK( saturable.find( idx ) == saturable.find( idy ) );
        CHECK( saturable.find( idx ) == saturable.find( idz ) );

        CHECK( saturable.find( idp ) != saturable.find( ids ) );
        CHECK( saturable.find( idp ) != saturable.find( idx ) );
        CHECK( saturable.find( idx ) != saturable.find( idp ) );

        CHECK( saturable.num_of_eclasses() == 3 );
        CHECK( saturable.eclass( idp ) == saturable.eclass( idm ) );
        CHECK( saturable.eclass( idx ) == saturable.eclass( idy ) );
        CHECK( saturable.eclass( idx ) == saturable.eclass( idz ) );

        // CHECK( saturable.eclass( idp ).parents.size() == 1 );
        // CHECK( saturable.eclass( idx ).parents.size() == 1 );

        // CHECK( saturable.eclass( ids ).size() == 1 );
        // CHECK( saturable.eclass( idp ).size() == 2 );
        // CHECK( saturable.eclass( idx ).size() == 3 );
    }

      TEST_CASE("EGraph Merge at Different layers")
      {
        test_graph egraph;

        auto idx = make_node(egraph, "x");
        auto idy = make_node(egraph, "y");
        auto idz = make_node(egraph, "z");
        auto idp = make_node(egraph, "+", {idx, idy});
        auto idm = make_node(egraph, "*", {idy, idz});
        auto ids = make_node(egraph, "-", {idp, idm});

        auto saturable = saturable_egraph(std::move(egraph));

        saturable.merge(ids, idx);
        saturable.merge(idx, idy);
        saturable.merge(idz, idy);

        saturable.rebuild();

        CHECK(saturable.find(idx) == saturable.find(idy));
        CHECK(saturable.find(idx) == saturable.find(idz));
        CHECK(saturable.find(idx) == saturable.find(ids));

        CHECK(saturable.find(idp) != saturable.find(idm));
        CHECK(saturable.find(idp) != saturable.find(ids));

        CHECK(saturable.num_of_eclasses() == 3);
        CHECK(saturable.eclass(idx) == saturable.eclass(ids));
        CHECK(saturable.eclass(idx) == saturable.eclass(idy));
        CHECK(saturable.eclass(idx) == saturable.eclass(idz));

        // CHECK(saturable.eclass(idp).parents.size() == 1);
        // CHECK(saturable.eclass(idx).parents.size() == 2);

        // CHECK(saturable.eclass(ids).size() == 4);
        // CHECK(saturable.eclass(idp).size() == 1);
        // CHECK(saturable.eclass(idm).size() == 1);
      }

    //   TEST_CASE("EGraph with bitwidths")
    //   {
    //     TestGraph egraph;

    //     auto idx = make_node(egraph, "x:32");
    //     auto idy = make_node(egraph, "y:32");
    //     auto add = make_node(egraph, "+", {idx, idy});

    //     auto sx = egraph.singleton(idx);
    //     auto sy = egraph.singleton(idy);
    //     auto sa = egraph.singleton(add);
    //     CHECK(bitwidth(&sx) == 32);
    //     CHECK(bitwidth(&sy) == 32);
    //   }

    } // test suite: eqsat::building-and-merging

} // namespace eqsat::test
