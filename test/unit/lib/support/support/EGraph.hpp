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
    auto make_leaf(const std::string &expr)
    {
      auto candidate = ENode(expr);
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

    auto make_node(const std::string &expr, std::vector<Id> children)
    {
      ENode node(expr);
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
    using Constant = eqsat::ASTNode::Constant;
    using Place    = eqsat::ASTNode::Place;
    using Op       = eqsat::ASTNode::Op;

    using PatternNode = eqsat::ASTNodePtr;

    TestGraphBuilder(TestGraph *graph) : _graph(graph) {}

    template< typename Substitutions >
    Id synthesize(const PatternNode &ast, const Substitutions &subs) const
    {

      std::vector< Id > args;
      for (const auto &child : ast->children)
        args.push_back(synthesize(child, subs));

      auto node = std::visit( overloaded {
        [&] (const Constant &con) -> Id { return _graph->make_leaf( std::to_string(con.ref()) ); },
        [&] (const Place &plc)    -> Id { return subs.id(plc.ref()); },
        [&] (const Op &op)        -> Id { return _graph->make_node(op.ref(), args); },
        [&] (const auto&)         -> Id { LOG(FATAL) << "unsupported node"; },
      }, ast->value);

      return node;
    }

    template< typename Pattern, typename Substitution >
    Id synthesize(const Pattern &pattern, const Substitution &sub) const
    {
      CHECK(sub.size() == pattern.places.size());
      return synthesize(pattern.ast, sub);
    }

    TestGraph *_graph;
  };

} // namesapce circuitous