/*
 * Copyright (c) 2021, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <doctest/doctest.h>
#include <circuitous/ADT/EGraph.hpp>

#include <fstream>

namespace circuitous {

  struct TestGraph : EGraph< ENode< std::string> >
  {
    auto make_leaf(const std::string &expr)
    {
      return add(ENode(expr));
    }

    auto make_node(const std::string &expr, std::vector<Id> children)
    {
      ENode node(expr);
      node.children = std::move(children);
      return add(std::move(node));
    }

    void dump(const std::string &file) {
      std::ofstream out(file);
      to_dot(*this, out, [] (auto *node) { return node->term; });
    }
  };

  using TestENode = TestGraph::ENode;
  using Id = TestGraph::Id;

} // namesapce circuitous