/*
 * Copyright (c) 2021, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <doctest/doctest.h>
#include <circuitous/ADT/EGraph.hpp>
#include <circuitous/Transforms/Pattern.hpp>

#include <glog/logging.h>

#include <fstream>
#include <string>
#include <map>

namespace circ {

  template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
  template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

  using StringNode = ENode< std::string >;
  struct TestGraph : EGraph< StringNode >
  {
    auto make_leaf(std::string_view atom)
    {
      auto candidate = ENode(std::string(atom));
      auto constant = candidate.constant();

      if (constant) {
        if (auto it = constants.find(constant.value()); it != constants.end()) {
          return find(it->second);
        }
      }

      auto [id, node] = add(std::move(candidate));

      if (constant) {
        constants.emplace(constant.value(), node);
      }

      return id;
    }

    auto make_node(std::string_view atom, std::vector<Id> children)
    {
      ENode node((std::string(atom)));
      node.children = std::move(children);
      auto [id, _] = add(std::move(node));
      return id;
    }

    void dump(const std::string &file) {
      std::ofstream out(file);
      to_dot(*this, out, [] (auto *node) { return node->term; });
    }

    std::map<std::int64_t, StringNode*> constants;
  };

  using TestENode = TestGraph::ENode;
  using Id = TestGraph::Id;

  struct TestGraphBuilder
  {
    using constant  = eqsat::constant;
    using place     = eqsat::place;
    using operation = eqsat::operation;

    TestGraphBuilder(TestGraph *graph) : _graph(graph) {}

    Id synthesize(const eqsat::pattern &pat, const auto &substitutions, const auto &places) const
    {
      std::vector< Id > args;
      for (const auto &child : children(pat)) {
        args.push_back(synthesize(child, substitutions, places));
      }

      auto node = std::visit( overloaded {
        [&] (const constant &con) -> Id { return _graph->make_leaf( std::to_string(con.ref()) ); },
        [&] (const place &plc)    -> Id { return substitutions.id(places.at(plc)); },
        [&] (const operation &op) -> Id { return _graph->make_node(op.ref(), args); },
        [&] (const auto&)         -> Id { LOG(FATAL) << "unsupported node"; },
      }, root(pat));

      return node;
    }

    TestGraph *_graph;
  };

} // namesapce circuitous