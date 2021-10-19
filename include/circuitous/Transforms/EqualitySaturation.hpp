/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iterator>
#include <map>
#include <set>
#include <memory>
#include <optional>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>
#include <string>
#include <string_view>

#include <circuitous/IR/IR.h>
#include <circuitous/ADT/EGraph.hpp>
#include <circuitous/ADT/UnionFind.hpp>
#include <circuitous/Transforms/Pattern.hpp>
#include <circuitous/Transforms/PassBase.hpp>

#include <circuitous/Transforms/EqSat/Graph.hpp>

namespace circ::eqsat {

  template< typename Graph, typename ENode = typename Graph::Node >
  static inline std::unordered_set< ENode* > contexts(const Graph &egraph, Id id)
  {
    std::unordered_set< ENode* > res;
    for (auto p : egraph.eclass(id).parents) {
      if (is_context_node(p)) {
        res.insert(p);
      } else {
        res.merge(contexts(egraph, egraph.find(p)));
      }
    }
    return res;
  }

  // substitution mapping from places (variables) to equality classes
  struct Substitution
  {
    using value_type = std::pair< Id, bool >;
    using index = std::size_t;

    Substitution(std::size_t size) : _mapping(size, Id(0)) , _matched(size, false) {}
    Substitution(std::size_t size, index idx, Id id)
      : Substitution(size)
    {
      set(idx, id);
    }

    bool test(index idx) const { return _matched[idx]; }

    void set(index idx, Id id)
    {
      _matched[idx] = true;
      _mapping[idx] = id;
    }

    Id id(index idx) const
    {
      CHECK(test(idx));
      return _mapping[idx];
    }

    size_t size() const { return _mapping.size(); }

    value_type get(index idx) const { return { _mapping[idx], _matched[idx] }; }

    bool fully_matched() const
    {
      return std::all_of(_matched.begin(), _matched.end(), [](bool v) { return v; });
    }

    friend bool operator<(const Substitution &a, const Substitution &b)
    {
      return std::tie(a._mapping, a._matched) < std::tie(b._mapping, b._matched);
    }

    const std::vector<Id>& mapping() const { return _mapping; }
    const std::vector<bool>& matched() const { return _matched; }

  private:
    std::vector< Id > _mapping;
    std::vector< bool > _matched;
  };

  template< typename stream >
  auto operator<<(stream &os, const Substitution &sub) -> decltype(os << "")
  {
    size_t idx = 0;
    for (const auto &[id, matched] : llvm::zip(sub.mapping(), sub.matched())) {
      auto matchedid = matched ? std::to_string(id.ref()) : "unmatched";
      os << "\t" << idx++ << " -> " << matchedid << "\n";
    }
    return os;
  }

  using Substitutions = std::set< Substitution >;

  using MatchLabel = std::variant< std::monostate, label >;
  using LabelsMap = std::unordered_map< MatchLabel, Id >;

  template< typename stream >
  auto operator<<(stream &os, const MatchLabel &lab) -> decltype(os << "")
  {
    return std::visit( overloaded {
      [&] (const std::monostate &) -> stream& { return os << "none"; },
      [&] (const label &l)         -> stream& { return os << l; }
    }, lab);
  }

  static inline constexpr MatchLabel anonymous_label = std::monostate{};

  // Single match of labeled expressions, maps to each matching label
  // a single equality class.
  // Maintains possible substitutions of pattern plces for current match.
  // A special case is anonymous match for unlabeled pattern.
  struct LabeledMatch
  {
    // matching of labels to equality nodes
    LabelsMap labels;

    // corresponding substitutions for the matching
    Substitutions substitutions;

    bool is_anonymous() const { return labels.count(anonymous_label); }
  };

  template< typename stream >
  auto operator<<(stream &os, const LabeledMatch &match) -> decltype(os << "")
  {
    os << "labels {\n";
    for (const auto &[label, id] : match.labels) {
      os << "\t" << label << " -> " << id << "\n";
    }
    os << "}\n";
    os << "substitutions {\n";
    for (const auto &sub : match.substitutions) {
      os << sub;
    }
    os << "}\n";
    return os;
  }

  // All possible matches for a given pattern
  using Matches = std::vector< LabeledMatch >;

  static inline auto tail(const auto &vec)
  {
    auto tail = std::next(vec.begin());
    return std::span( &(*tail), vec.size() - 1 );
  }

  template< typename Graph, typename Pattern, typename Places >
  struct Match
  {
    Match(const Graph &graph, const Pattern &pat, const Places &places)
      : egraph(graph), pattern(pat), places(places)
    {}

    Matches result() const
    {
      auto matches = std::visit( overloaded {
        [&] (const match_expr &e) -> Matches { return match(e); },
        [&] (const expr_list  &e) -> Matches { return match(e); },
        [&] (const atom &a)       -> Matches { return match(a); },
        [&] (const bond_expr & ) -> Matches {
          throw std::runtime_error("bond clause is forbidden in the matching pattern");
        },
        [&] (const union_expr & ) -> Matches {
          throw std::runtime_error("union clause is forbidden in the matching pattern");
        }
      }, pattern.get());

      return filter_constrained(std::move(matches));
    }

  private:

    const Graph &egraph;
    const Pattern &pattern;
    const Places &places;

    using ENode  = typename Graph::Node;
    using EClass = typename Graph::EClass;

    // Filters out matches that does not satisfy pattern constraints (context
    // constraints and place constraints).
    Matches filter_constrained(Matches &&matches) const
    {
      for (const auto &con : pattern.context_constraints)
        matches = filter_constrained_disjoint_contexts(std::move(matches), con);

      for (const auto &con : pattern.place_constraints)
        matches = filter_constrained_equivalent_places(std::move(matches), con);

      return std::move(matches);
    }

    // Filters out matches that does not satisfy disjoint contexts
    Matches filter_constrained_disjoint_contexts(Matches &&matches, const disjoint_expr &constraint) const
    {
      if (constraint.contexts.size() < 2)
        return std::move(matches);

      auto filter = [&] (const auto &match) { return has_disjoint_contexts(match, constraint.contexts); };
      matches.erase( std::remove_if(matches.begin(), matches.end(), filter), matches.end() );

      return std::move(matches);
    }

    // Filters out matches that does not satisfy semantic equivalence of
    // constrained places, e.g., for constraint (equiv ?x ?y), the match needs
    // to have semanticaly equivalent matched places ?x and ?y.  Places are
    // semantically equivalent if corresponding classes contain at least one
    // semantically equivalent node, that is:
    // 1) its the same operation with sem. equivalent children
    // 2) the sem. equivalent leaf node.
    Matches filter_constrained_equivalent_places(Matches &&matches, const equiv_expr &constraint) const
    {
      if (places.empty())
        return std::move(matches);

      if (constraint.places.size() < 2)
        return std::move(matches);

      auto filter_places = [&] (const auto &sub) { return !has_equivalent_places(sub, constraint.places); };

      for (auto &match : matches) {
        std::erase_if(match.substitutions, filter_places);
      }

      auto filter_empty_matches = [&] (const auto &match) { return match.substitutions.empty(); };
      matches.erase( std::remove_if(matches.begin(), matches.end(), filter_empty_matches), matches.end() );

      return std::move(matches);
    }

    using context_node = typename Graph::Node*;
    using context_t = std::unordered_set< context_node >;
    using named_contexts = std::unordered_map<context_name, context_t>;

    named_contexts get_named_contexts(const auto &labels) const
    {
      named_contexts res;
      for (const auto &[lab, id] : labels) {
        if (std::holds_alternative<label>(lab))
          if (auto sub = pattern.subexprs.at(std::get<label>(lab)); sub.context)
            res[*sub.context] = contexts(egraph, id);
      }
      return res;
    }

    // Returns true if 'match' has non overlapping context sets
    bool has_disjoint_contexts(const LabeledMatch &match, const auto &contexts) const
    {
      auto named = get_named_contexts(match.labels);

      std::unordered_map< context_node, int > counts;
      for (const auto &name : contexts)
        for (const auto &node : named.at(name.ref()))
          counts[node]++;

      return std::any_of(counts.begin(), counts.end(), [] (const auto &elem) {
        const auto &[_, count] = elem;
        return count > 1;
      });
    }

    // Returns true if nodes are semantically equivalent: node terms satisfies
    // its == relation and all children classes are sem. equivalent
    bool semantically_equivalent_nodes(const ENode * lhs, const ENode * rhs) const
    {
        if (lhs->get() != rhs->get())
          return false;
        for (const auto &[lch, rch] : llvm::zip(lhs->children(), rhs->children())) {
          auto lclass = egraph.eclass(lch);
          auto rclass = egraph.eclass(rch);
          if (!semantically_equivalent_classes(lclass, rclass))
            return false;
        }
        return true;
    }

    // Equility class is semantically equivalent to other class if it contains
    // semantically equivalent node to some of nodes of the other class.
    bool semantically_equivalent_classes(const EClass &lhs, const EClass &rhs) const
    {
      for (const auto &lnode : lhs.nodes) {
        for (const auto &rnode : rhs.nodes) {
          if (semantically_equivalent_nodes(lnode, rnode)) {
            return true;
          }
        }
      }
      return false;
    }

    // Returns true if substitution satisfy semantic equivalence constraint 'con_places'.
    bool has_equivalent_places(const Substitution &sub, const auto &con_places) const
    {
      auto equivalent = [&] (const place &lhs, const place &rhs) {
        auto eclass = [&] (const place &p) {
          return egraph.eclass( sub.id( places.at(p) ) );
        };
        return semantically_equivalent_classes(eclass(lhs), eclass(rhs));
      };

      // if all places are equivalent with the first place than they are all equivalent
      return std::all_of(con_places.begin(),con_places.end(), [&] (const auto &place) {
        return equivalent(place, con_places.front());
      });
    }

    static inline Matches filter_nonunique_matches(Matches &&matches)
    {
      using MatchedIds = std::set< Id >;

      auto nonunique = [] (const auto &match) {
        MatchedIds matched;
        for (const auto &[label, id] : match.labels)
          matched.insert(id);
        return matched.size() != match.labels.size();
      };

      matches.erase( std::remove_if(matches.begin(), matches.end(), nonunique), matches.end() );
      return std::move(matches);
    }

    static inline Matches filter_commutative_matches(Matches &&matches)
    {
      using MatchedIds = std::set< Id >;
      std::vector< MatchedIds > seen;

      auto matched_ids = [] (const LabelsMap &labels) -> MatchedIds {
        MatchedIds matched;
        for (const auto &[label, id] : labels)
          matched.insert(id);
        return matched;
      };

      auto commutes = [&] (const auto &match) {
        auto ids = matched_ids(match.labels);
        if (std::find(seen.begin(), seen.end(), ids) != seen.end())
          return true;
        seen.push_back(ids);
        return false;
      };

      matches.erase( std::remove_if(matches.begin(), matches.end(), commutes), matches.end() );
      return std::move(matches);
    }

    static inline std::optional< LabeledMatch > merge(const LabeledMatch &lhs, const LabeledMatch &rhs)
    {
      auto product = product_of_substitutions(lhs.substitutions, rhs.substitutions);
      if (product.empty())
        return std::nullopt;

      LabeledMatch match{lhs.labels, std::move(product)};

      for (const auto &[label, id] : rhs.labels) {
        CHECK(!match.labels.count(label));
        match.labels[label] = id;
      }

      return match;
    }

    // TODO(Heno): optimize copies of vectors and maps
    static inline Matches product_of_matches(const auto &matches)
    {
      CHECK(matches.size() > 0);
      if (matches.size() == 1)
        return matches.front();

      Matches result;
      auto partial = product_of_matches(tail(matches));
      for (const auto &lhs : matches.front()) {
        for (const auto &rhs : partial) {
          if (auto merged = merge(lhs, rhs)) {
            result.push_back(std::move(*merged));
          }
        }
      }

      return result;
    }

    // combine single label matches into multilable matches
    Matches combine_matches(const std::vector< Matches > &matches) const
    {
      return product_of_matches(std::span(matches));
    }

    // match multiple labeled expressions and unify the result
    Matches match(const match_expr &e) const
    {
      std::vector< Matches > matches;
      for (const auto &label : labels(e)) {
        matches.push_back(match(pattern.subexprs.at(label), label));
      }

      auto result = combine_matches(matches);

      result = filter_nonunique_matches(std::move(result));

      if (std::holds_alternative< commutative_match_expr >(e))
        return filter_commutative_matches(std::move(result));
      return result;
    }

    // match single unlabled expression
    Matches match(const auto &e, MatchLabel lab = anonymous_label) const
    {
      Matches matched;
      // TODO(Heno): add operation caching to egraph
      for (const auto &[id, eclass] : egraph.classes())
        // pattern matches with a root in the eclass
        if (auto subs = match_eclass(eclass, e); !subs.empty())
          matched.push_back( LabeledMatch{{{lab, id}}, std::move(subs)} );
      return matched;
    }

    // match pattern with some of the eclass nodes (produces all possible matches)
    Substitutions match_eclass(const auto &eclass, const auto &e) const
    {
      Substitutions res;
      for (const auto &node : eclass.nodes)
        res.merge(match_enode(node, e));
      return res;
    }

    // match pattern with root in the enode
    Substitutions match_enode(const auto &enode, const expr &e) const
    {
      return std::visit( overloaded {
        [&] (const atom &a)      -> Substitutions { return match_enode(enode, a); },
        [&] (const expr_list &l) -> Substitutions { return match_enode(enode, l); },
        [&] (const match_expr &) -> Substitutions {
          throw std::runtime_error("match clause is forbidden in the nested expression");
        },
        [&] (const union_expr &) -> Substitutions {
          throw std::runtime_error("union clause is forbidden in the nested expression");
        },
        [&] (const bond_expr &) -> Substitutions {
          throw std::runtime_error("bond clause is forbidden in the nested expression");
        }
      }, e.get());
    }

    Substitutions match_enode(const auto &enode, const atom &a) const
    {
      return std::visit([&] (const auto &v) -> Substitutions { return match_atom(enode, v); }, a);
    }

    Substitutions match_enode(const auto &enode, const expr_list &e) const
    {
      if (auto head = match_enode(enode, e.front()); !head.empty()) {
        if (tail(e).empty())
          return head;

        if (tail(e).size() != enode->children().size())
          return {};

        // gather substitutions for all children
        std::vector< Substitutions > subs;
        auto child = enode->children().begin();
        for (const auto &node : tail(e)) {
          auto child_class = egraph.eclass(*child);
          auto sub = match_eclass(child_class, node);
          if (sub.empty())
            return {}; // unmatched child
          subs.push_back(std::move(sub));
          ++child;
        }

        return product_of_substitutions(head, subs);
      }

      return {};
    }

    static inline std::optional< Substitution > merge_subsitutions(auto lhs, const auto &rhs)
    {
      for (size_t idx = 0; idx < rhs.size(); ++idx) {
        if (auto [id, set] = rhs.get(idx); set) {
          if (!lhs.test(idx)) {
            lhs.set(idx, id);
          } else {
            if (lhs.id(idx) != id)
              return std::nullopt;
          }
        }
      }
      return lhs;
    }

    static inline Substitutions product_of_substitutions(const Substitutions &lhs, const Substitutions &rhs)
    {
      Substitutions product;
      for (const auto &lsub : lhs)
        for (const auto &rsub : rhs)
          if (auto merged = merge_subsitutions(lsub, rsub))
            product.insert(std::move(*merged));
      return product;
    }

    static inline Substitutions product_of_substitutions(const Substitutions &head, const std::vector< Substitutions > &tail)
    {
      Substitutions product = head;

      for (const auto &rhs : tail) {
        Substitutions next = product_of_substitutions(product, rhs);
        std::swap(product, next);
      }

      return product;
    }

    // creates trivial substitution (with no fixed place) if matched
    static inline Substitutions trivial(bool matched, std::size_t size)
    {
      if (matched)
        return {Substitution(size)};
      return {};
    }

    // match constant node
    Substitutions match_atom(const auto &enode, const constant &c) const
    {
      if (auto nconst = extract_constant(enode))
        return trivial(nconst == c.ref(), places.size());
      return {};
    }

    // match operation node
    Substitutions match_atom(const auto &enode, const operation &o) const
    {
      return trivial(name(enode) == o, places.size());
    }

    // match place (variable) node, returns single substitution with
    // a matched place to the enode
    Substitutions match_atom(const auto &enode, const place &p) const
    {
      auto id = egraph.find(enode);
      return {Substitution{places.size(), places.at(p), id}};
    }

    // match labeled subexpression
    Substitutions match_atom(const auto &enode, const single_label &lab) const
    {
      return match_enode(enode, pattern.subexprs.at(lab));
    }

    // match variadic labeled subexpression
    Substitutions match_atom(const auto &enode, const variadic_label &lab) const
    {
      llvm_unreachable("match variadic");
    }

    // match labeled subexpression
    Substitutions match_atom(const auto &enode, const label &lab) const
    {
      return std::visit([&] (const auto &l) { return match_atom(enode, l); }, lab);
    }

  }; // Matcher

  template< typename Graph, typename Builder, typename Pattern, typename Places >
  struct Apply
  {
    Apply(Graph &graph, const Builder &builder, const Pattern &pattern, const Places &places)
      : egraph(graph), builder(builder), pattern(pattern), places(places)
    {}

    Graph &egraph;
    const Builder &builder;
    const Pattern &pattern;
    const Places  &places;

    void apply(const Matches &matches) const { apply(pattern, matches); }

    void apply(const expr &e, const Matches &matches) const
    {
      std::visit( overloaded {
        [&] (const atom &a)       {
          throw std::runtime_error("rewrite rule is applied on the level of expression lists");
        },
        [&] (const expr_list  &e) { apply(e, matches); },
        [&] (const union_expr &e) { apply(e, matches); },
        [&] (const bond_expr &e) { apply(e, matches); },
        [&] (const match_expr & ) {
          throw std::runtime_error("match clause is forbidden in the rewrite pattern");
        }
      }, e.get());
    }

    void apply(const bond_expr &e, const Matches &matches) const
    {
      for (const auto &match : matches) {
        std::vector<Id> nodes;
        for (const auto &lab : e.labels)
          nodes.push_back(match.labels.at(lab));
        egraph.bond(std::move(nodes));
      }
    }

    void apply(const union_expr &e, const Matches &matches) const
    {
      for (const auto &match : matches) {
        auto head = match.labels.at(e.labels.front());
        for (const auto &next : tail(e.labels)) {
          egraph.merge(head, match.labels.at(next));
        }
      }
    }

    void apply(const expr_list &list, const Matches &matches) const
    {
      if (!is_nested(list))
        return apply_patch(list, matches);

      for (const auto &e :list)
        apply(e, matches);
    }

    void apply_patch(const expr &e, const Matches &matches) const
    {
      for (const auto &match : matches) {
        auto id = match.labels.at(anonymous_label);
        // TODO(Heno): perform once for all substitutions
        for (const auto &sub : match.substitutions) {
          auto patch = builder.synthesize(e, sub, places, pattern.subexprs);
          egraph.merge(id, patch);
        }
      }
    }

    bool is_nested(const expr_list &list) const
    {
      return std::all_of(list.begin(), list.end(), [] (const auto &e) {
        return std::holds_alternative<expr_list>(e);
      });
    }
  };

  template< typename Graph >
  struct Rule
  {
    Rule(std::string_view name, std::string_view lhs_, std::string_view rhs_)
      : name(name), lhs(make_pattern(lhs_)), rhs(make_pattern(rhs_))
      , places(get_indexed_places(lhs))
    {}

    auto match(const Graph &egraph) const
    {
      Match m(egraph, lhs, places);
      return m.result();
    }

    void apply(Graph &egraph, const auto &builder, const auto &matches) const
    {
      Apply a(egraph, builder, rhs, places);
      a.apply(matches);
    }

    void apply(Graph &egraph, const auto &builder) const
    {
      apply(egraph, builder, match(egraph));
    }

    const std::string name;
    // Rewrite rule 'lhs -> rhs' that allows to match
    // left-hand-side and replace it with right-hand-side
    Pattern lhs;
    Pattern rhs;

    // Places that occur in the rewrite pattern
    // Note: it is required that place occurs on the left hand side
    // of the rule when it occurs on the right hand side
    indexed_places places;
  };


  template< typename Graph >
  using Rules = std::vector< Rule< Graph > >;

  template< typename Graph, typename Builder >
  struct BasicRulesScheduler
  {
    using Rule = Rule< Graph >;

    BasicRulesScheduler(Builder &builder) : _builder(builder) {}

    Matches match_rule(const Graph &egraph, const Rule &rule) const
    {
      return rule.match(egraph);
    }

    void apply_rule(Graph &egraph, const Rule &rule, const Matches &matches) const
    {
      rule.apply(egraph, _builder, matches);
    }

    void apply_rule(Graph &egraph, const Rule &rule) const
    {
      rule.apply(egraph);
    }

  private:
    Builder _builder;
  };

  CircuitPtr EqualitySaturation(const CircuitPtr &);

} // namespace circ::eqsat
