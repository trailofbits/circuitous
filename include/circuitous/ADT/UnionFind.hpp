/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <cstdint>
#include <vector>

#include <circuitous/Util/StrongType.hpp>

namespace circ {

  struct UnionFind {

    struct id_tag;
    using Id = strong_type< std::size_t, id_tag >;


    inline Id make_set() noexcept
    {
      _parents.push_back( Id(_parents.size()) );
      return _parents.back();
    }

    [[nodiscard]] inline Id& parent(Id id) noexcept
    {
      CHECK( _parents.size() > id.ref() );
      return _parents[id.ref()];
    }

    [[nodiscard]] inline Id parent(Id id) const noexcept
    {
      CHECK( _parents.size() > id.ref() );
      return _parents[id.ref()];
    }

    // Obtains a root 'id' for given node, but does not
    // update union-find hierarchy.
    // Preferably use find_compress that performs also
    // a path compression.
    [[nodiscard]] inline Id find(Id node) const noexcept
    {
      while (node != parent(node)) {
        node = parent(node);
      }
      return node;
    }

    // Performs 'find' with a path compression
    inline Id find_compress(Id node) noexcept
    {
      while (node != parent(node)) {
        auto gradparent = parent(parent(node));
        parent(node) = gradparent;
        node = gradparent;
      }
      return node;
    }

    inline Id merge(Id a, Id b) noexcept
    {
      CHECK(a == parent(a));
      CHECK(b == parent(b));
      CHECK(a != b);

      parent(b) = a;
      return a;
    }

  private:
    std::vector< Id > _parents;
  };

} // namespace circ
