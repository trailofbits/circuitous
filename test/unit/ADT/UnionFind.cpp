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

TEST_CASE("UnionFind simple") {
  auto union_find = UnionFind();

  constexpr int n = 10;
  for (int i = 0; i < n; i++) {
    union_find.make_set();
  }

  for (unsigned int i = 0; i < n; i++) {
    CHECK(union_find.parent(i) == i);
  }

  auto id = [&] (unsigned v) { return union_find.find(v); };

  union_find.merge(0, 1);
  CHECK(union_find.find(0) == union_find.find(1));

  union_find.merge(0, 2);
  CHECK(union_find.find(0) == union_find.find(2));

  union_find.merge(0, 3);
  CHECK(union_find.find(0) == union_find.find(3));
  CHECK(union_find.find(2) == union_find.find(3));

  union_find.merge(5, 8);
  CHECK(union_find.find(5) == union_find.find(8));

  union_find.merge(id(8), id(7));
  union_find.merge(id(6), id(7));
  CHECK(union_find.find_compress(5) == union_find.find_compress(7));
  CHECK(union_find.find(8) == union_find.find(6));
}