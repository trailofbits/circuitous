/*
 * Copyright (c) 2021, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <doctest/doctest.h>
#include <circuitous/ADT/UnionFind.hpp>

using UnionFind = circ::UnionFind;
using Id = typename UnionFind::Id;

TEST_CASE("UnionFind simple") {
  auto union_find = UnionFind();

  constexpr int n = 10;
  for (int i = 0; i < n; i++) {
    union_find.make_set();
  }

  for (std::size_t i = 0; i < n; i++) {
    Id id(i);
    CHECK(union_find.parent(id) == id);
  }

  auto id = [&] (std::size_t v) { return union_find.find(Id(v)); };

  union_find.merge(Id(0), Id(1));
  CHECK(union_find.find(Id(0)) == union_find.find(Id(1)));

  union_find.merge(Id(0), Id(2));
  CHECK(union_find.find(Id(0)) == union_find.find(Id(2)));

  union_find.merge(Id(0), Id(3));
  CHECK(union_find.find(Id(0)) == union_find.find(Id(3)));
  CHECK(union_find.find(Id(2)) == union_find.find(Id(3)));

  union_find.merge(Id(5), Id(8));
  CHECK(union_find.find(Id(5)) == union_find.find(Id(8)));

  union_find.merge(id(8), id(7));
  union_find.merge(id(6), id(7));
  CHECK(union_find.find_compress(Id(5)) == union_find.find_compress(Id(7)));
  CHECK(union_find.find(Id(8)) == union_find.find(Id(6)));
}