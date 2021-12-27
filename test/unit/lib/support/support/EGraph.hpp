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
#include <circuitous/Util/Overloads.hpp>
#include <circuitous/Util/Logging.hpp>

#include <fstream>
#include <string>
#include <map>

namespace circ::eqsat {

  using StringNode = ENode< std::string >;

  static inline std::string full_name(const StringNode *node)
  {
    return std::visit( overloaded {
      [] (const BondNode &n) { return n.name(); },
      [] (const StorageNode< std::string > &n) { return n.get(); }
    }, node->get());
  }

  static inline std::string name(const StringNode *node)
  {
    auto strip_bitwidth = [] (const auto &name) {
      return name.substr(0, name.find(":"));
    };

    return strip_bitwidth(full_name(node));
  }

  static inline std::optional< uint32_t > bitwidth(const StringNode *node)
  {
    const auto &name = full_name(node);

    if (auto from = name.find(":"); from != std::string::npos) {
      return std::stoi(name.substr(from + 1));
    }

    return std::nullopt;
  }

  static inline bool is_context_node(const StringNode *node)
  {
    auto n = name(node);
    return std::string_view(n).starts_with("CTX");
  }

  static inline std::optional<std::int64_t> extract_constant(const StringNode *node)
  {
    auto is_number = [] (std::string_view s) {
      auto isdigit = [](auto c) { return std::isdigit(c); };
      return !s.empty() && std::all_of(s.begin(), s.end(), isdigit);
    };

    if (is_number(node->data()))
      return std::stoll(node->data());

    return std::nullopt;
  }

  struct TestGraph : EGraph< StringNode >
  {
    using Base  = EGraph< StringNode >;
    using ENode = Base::Node;

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
      node.children() = std::move(children);
      auto [id, _] = add(std::move(node));
      return id;
    }

    StringNode singleton(Id id)
    {
      const auto &eclass = this->eclass(id);
      CHECK(eclass.size() == 1);
      return eclass.nodes.at(0)->data();
    }

    void dump(const std::string &file) {
      std::ofstream out(file);
      to_dot(*this, out);
    }

    std::map<std::int64_t, StringNode*> constants;
  };

  using TestENode = TestGraph::Node;

  struct TestGraphBuilder
  {
    using constant  = eqsat::constant;
    using place     = eqsat::place;
    using label     = eqsat::label;
    using operation = eqsat::operation;

    TestGraphBuilder(TestGraph *graph) : _graph(graph) {}

    Id constrain(const Id node, const Id advice) const
    {
      return _graph->make_node("constraint", {node, advice});
    }

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
        [&] (const operation &op) -> Id { return _graph->make_node(op.name, args); },
        [&] (const label &lab)    -> Id { return synth(subexprs.at(label_name(lab))); },
        [&] (const auto&)         -> Id { LOG(FATAL) << "unsupported node"; },
      }, root(e));

      return node;
    }

    TestGraph *_graph;
  };

} // namespace circ
