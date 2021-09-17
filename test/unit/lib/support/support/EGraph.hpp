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

namespace circ::eqsat {

  template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
  template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

  using StringNode = ENode< std::string >;

  static inline bool is_context_node(const StringNode *node)
  {
    return std::string_view(node->term).starts_with("CTX");
  }

  static inline std::string name(const StringNode *node)
  {
    return node->term;
  }

  static inline std::optional<std::int64_t> extract_constant(const StringNode *node)
  {
    auto is_number = [] (std::string_view s) {
      auto isdigit = [](auto c) { return std::isdigit(c); };
      return !s.empty() && std::all_of(s.begin(), s.end(), isdigit);
    };

    if (is_number(node->term))
      return std::stoll(node->term);

    return std::nullopt;
  }

  struct TestGraph : EGraph< StringNode >
  {
    auto make_leaf(std::string_view atom)
    {
      auto candidate = ENode(std::string(atom));
      auto con = extract_constant(&candidate);

      if (con)
        if (auto it = constants.find(*con); it != constants.end())
          return find(it->second);

      auto [id, node] = add(std::move(candidate));

      if (con)
        constants.emplace(*con, node);

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
    using label     = eqsat::label;
    using operation = eqsat::operation;

    TestGraphBuilder(TestGraph *graph) : _graph(graph) {}

    Id synthesize(const eqsat::expr &e, const auto &subs,
                  const auto &places, const auto &subexprs) const
    {
      CHECK(subs.size() == places.size());
      auto synth = [&] (const auto &sub) {
        return synthesize(sub, subs, places, subexprs);
      };

      std::vector< Id > args;
      for (const auto &child : children(e)) {
        args.push_back(synth(child));
      }

      auto node = std::visit( overloaded {
        [&] (const constant &con) -> Id { return _graph->make_leaf( std::to_string(con.ref()) ); },
        [&] (const place &plc)    -> Id { return subs.id(places.at(plc)); },
        [&] (const operation &op) -> Id { return _graph->make_node(op.ref(), args); },
        [&] (const label &lab)    -> Id { return synth(subexprs.at(lab)); },
        [&] (const auto&)         -> Id { LOG(FATAL) << "unsupported node"; },
      }, root(e));

      return node;
    }

    TestGraph *_graph;
  };

} // namespace circ