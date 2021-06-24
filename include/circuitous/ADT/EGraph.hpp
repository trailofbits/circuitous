/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <memory>
#include <optional>
#include <vector>
#include <ostream>
#include <unordered_map>
#include <unordered_set>

#include <circuitous/ADT/UnionFind.hpp>

namespace circ {

  template< typename Term_ >
  struct ENode
  {
    using Id = UnionFind::Id;
    using Term = Term_;
    using Children = std::vector< Id >;

    explicit ENode(const Term &t) : term(t) {}

    template< typename Fn >
    void update_children(Fn &&fn)
    {
      for (auto &child : children)
        child = fn(child);
    }

    friend bool operator==(const ENode &a, const ENode &b)
    {
      return std::tie(a.term, a.children) == std::tie(b.term, b.children);
    }
    friend bool operator<(const ENode &a, const ENode &b) { return a.term < b.term; }

    std::string name() const
    {
      if constexpr (std::is_same_v< Term, std::string >)
        return term;
      else
        return term->Name();
    }

    std::optional< std::int64_t > constant() const
    {
      auto is_number = [] (const std::string &s) {
        auto isdigit = [](auto c) { return std::isdigit(c); };
        return !s.empty() && std::all_of(s.begin(), s.end(), isdigit);
      };

      if constexpr (std::is_same_v< Term, std::string >) {
        if (is_number(term)) {
          return std::stoll(term);
        }
      }

      // TODO(Heno): Parse constant from circ

      return std::nullopt;
    }

    Term term;
    Children children;
  };

  // Equivalence class of term nodes
  template< typename ENode >
  struct EClass {

    using Id = UnionFind::Id;

    bool empty() const { return nodes.empty(); }
    auto size() const { return nodes.size(); }

    void add(ENode *enode) { nodes.push_back(enode); }

    void merge(EClass &&other)
    {
      std::move(other.nodes.begin(), other.nodes.end(), std::back_inserter(nodes));
      std::move(other.parents.begin(), other.parents.end(), std::back_inserter(parents));
    }

    friend bool operator==(const EClass &a, const EClass &b)
    {
      return std::tie(a.nodes, a.parents) == std::tie(b.nodes, b.parents);
    }

    std::vector< ENode* > nodes;
    std::vector< ENode* > parents;
  };


  template< typename ENode_ >
  struct EGraph
  {
    using Id = UnionFind::Id;
    using ENode = ENode_;
    using Term = typename ENode::Term;
    using Children = typename ENode::Children;
    using TermKey = std::pair< Term, Children >;
    using EClass = EClass< ENode >;

    Id create_singleton_eclass(ENode *enode)
    {
      auto id = _unions.make_set();
      _nodes.emplace(enode, id);

      EClass eclass;
      eclass.add(enode);
      _classes.emplace(id, eclass);

      return id;
    }

    void canonicalize(ENode *node)
    {
      node->update_children([&](auto child) { return _unions.find_compress(child); });
    }

    std::pair< Id, ENode* > add(ENode &&node)
    {
      canonicalize(&node);
      // allocate new egraph node
      _terms.push_back(std::make_unique< ENode >(std::move(node)));
      auto &enode = _terms.back();

      auto id = create_singleton_eclass(enode.get());

      // add children - parent links
      for (auto child : enode->children) {
        _classes[child].parents.push_back(enode.get());
      }

      return {id, enode.get()};
    }

    Id find(Id id) const { return _unions.find(id); }
    Id find(const ENode *enode) const { return _unions.find( _nodes.at(enode) ); }

    Id merge(Id a, Id b)
    {
      a = _unions.find_compress(a);
      b = _unions.find_compress(b);

      if (a == b)
        return a;

      // make sure that second eclass has fewer parents
      if ( _classes[a].parents.size() < _classes[b].parents.size() ) {
        std::swap(a, b);
      }

      auto new_id = _unions.merge(a, b);
      CHECK_EQ(new_id, a);

      _pending.push_back(new_id);

      // TODO(Heno) maybe can be moved to rebuild?
      auto class_b = _classes[b];
      _classes.erase(b);
      _classes[a].merge(std::move(class_b));

      return new_id;
    }

    EClass& eclass(ENode *enode) { return _classes[ _nodes[enode] ]; }
    const EClass& eclass(ENode *enode) const { return _classes[ _nodes[enode] ]; }

    EClass& eclass(Id id) { return _classes[ _unions.find(id) ]; }
    const EClass& eclass(Id id) const { return _classes.at( _unions.find(id) ); }

    // Restores the egraph invariants, i.e, congruence equality and enode uniqueness
    void rebuild()
    {
      // canonicalize and deduplicate the eclass references
      // to save calls to repair
      for (auto id : _pending)
        for (auto *node : _classes[id].nodes)
          canonicalize(node);

      std::unordered_set< Id > changed_classes;
      for (auto id : _pending)
        changed_classes.insert( _unions.find_compress(id) );

      for (auto id : changed_classes)
        repair(_classes[id]);

      _pending.clear();
    }

    void repair(EClass &eclass)
    {
      // update the '_nodes' so it always points
      // canonical enodes to canonical eclasses
      for (auto *node : eclass.nodes) {
        canonicalize(node);
        _nodes[node] = _unions.find_compress(_nodes[node]);
      }

      // deduplicate the parents, noting that equal
      // parents get merged and put on the worklist
      std::unordered_map< Id, ENode* > new_parents;
      for (auto *node : eclass.parents) {
        auto id = _unions.find_compress( _nodes[node] );
        new_parents.try_emplace(id, node);
      }

      eclass.parents.clear();
      for (auto[id, node] : new_parents)
        eclass.parents.push_back(node);

      // obliterate empty classes
      for (auto it = _classes.begin(); it != _classes.end(); ) {
        auto &[_, eclass] = *it;
        it = eclass.empty() ? _classes.erase(it) : std::next(it);
      }
    }

    const auto& classes() const { return _classes; }
    const auto& nodes() const { return _nodes; }

  private:
    // stores heap allocated nodes of egraph
    std::vector< std::unique_ptr< ENode > > _terms;

    // stores equivalence relation between equaltity classes
    UnionFind _unions;

    // all equavalent ids  map to the same class
    std::unordered_map< Id, EClass > _classes;

    // stores equality ids of enodes
    std::unordered_map< const ENode*, Id > _nodes;

    // modified eclasses that needs to be rebuild
    std::vector< Id > _pending;
  };

  template< typename Graph, typename Format >
  void to_dot(const Graph &egraph, std::ostream &out, Format fmt)
  {
    auto enumerate = [] (const auto &container, auto &&fn) {
      std::size_t i = 0;
      for (const auto &val : container) {
          fn(i++, val);
      }
    };

    out << "digraph egraph {\n";
    out << "  compound=true\n";
    out << "  clusterrank=local\n";

    for (const auto &[id_, eclass] : egraph.classes()) {
      out << "  subgraph cluster_" << id_ << " {\n";
      out << "    style=dotted\n";

      auto id = id_; // to allow lambda capture
      enumerate(eclass.nodes, [&] (auto node_idx, const auto &enode) {
        out << "    " << id << '.' << node_idx << " [label = \"" << fmt(enode) << "\" ]\n";
      });

      out << "  }\n";
    }

    for (const auto &[id_, eclass] : egraph.classes()) {
      auto id = id_; // to allow lambda capture
      enumerate(eclass.nodes, [&] (auto node_idx, const auto &enode) {
        enumerate(enode->children, [&] (auto child_idx, const auto &child) {
          auto child_class = egraph.find(child);
          out << id << '.' << node_idx << " -> ";
          if (id == child_class) {
            out << id << '.' << node_idx << ":n ";
          } else {
            out << child_class << ".0 ";
          }
          out << "[lhead = cluster_"<< child_class << "]\n";
        });
      });
    }

    out << "}\n";
  }

} // namespace circ