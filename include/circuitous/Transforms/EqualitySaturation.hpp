/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.hpp>

#include <algorithm>
#include <circuitous/ADT/EGraph.hpp>
#include <circuitous/ADT/UnionFind.hpp>
#include <circuitous/Support/Check.hpp>
#include <circuitous/Transforms/EqSat/Graph.hpp>
#include <circuitous/Transforms/PassBase.hpp>
#include <circuitous/Transforms/Pattern.hpp>
#include <circuitous/Util/Parser.hpp>
#include <cstddef>
#include <cstdint>
#include <experimental/iterator>
#include <functional>
#include <iterator>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

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
      check(test(idx));
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

  struct anonymous_match { Id id; };

  struct unary_match
  {
    unary_label label;
    Id id;
  };

  struct variadic_match
  {
    variadic_label label;
    std::set< Id > ids;
  };

  using label_match = std::variant< anonymous_match, unary_match, variadic_match >;

  inline std::string name(const anonymous_match &) {
    return "anonymous";
  }
  inline std::string name(const unary_match &m) {
    return label_name(m.label);
  }
  inline std::string name(const variadic_match &m) {
    return label_name(m.label);
  }
  inline std::string name(const label_match &match) {
    return std::visit([&] (const auto &m) { return name(m); }, match);
  }

  template< typename stream >
  auto operator<<(stream &os, const anonymous_match &m) -> decltype(os << "")
  {
    return os << name(m) << " → " << m.id << "\n";
  }

  template< typename stream >
  auto operator<<(stream &os, const unary_match &m) -> decltype(os << "")
  {
    return os << name(m) << " → " << m.id << "\n";
  }

  template< typename stream >
  auto operator<<(stream &os, const variadic_match &m) -> decltype(os << "")
  {
    os << name(m) << " → {";
    std::copy(std::begin(m.ids), std::end(m.ids), std::experimental::make_ostream_joiner(std::cout, ", "));
    os << "}";
    return os;
  }

  template< typename stream >
  auto operator<<(stream &os, const label_match &match) -> decltype(os << "")
  {
    return std::visit([&] (const auto &m) -> stream& { return os << m; }, match);
  }

  struct matched_labels
  {
    std::vector< label_match > labels;
    Substitutions substitutions;
  };

  inline std::optional< label_match > get_label_match(const matched_labels &match, std::string_view label_name)
  {
    auto same_label = [&] (const auto& other) { return name(other) == label_name; };
    const auto &labs = match.labels;
    if (auto it = std::find_if(labs.begin(), labs.end(), same_label); it != labs.end())
      return *it;
    return std::nullopt;
  }

  inline std::optional< label_match > get_label_match(const matched_labels &match, const label_match &lab)
  {
    return get_label_match(match, name(lab));
  }

  inline std::set< Id > matched_ids(const label_match &match)
  {
    return std::visit( overloaded {
      [&] (const anonymous_match &m) -> std::set< Id > { return {m.id}; },
      [&] (const unary_match &m)     -> std::set< Id > { return {m.id}; },
      [&] (const variadic_match &m)  -> std::set< Id > { return m.ids; }
    }, match);
  }

  inline std::set< Id > matched_ids(const matched_labels &match)
  {
    std::set< Id > ids;

    for (const auto &lab : match.labels)
      ids.merge(matched_ids(lab));

    return ids;
  }

  template< typename stream >
  auto operator<<(stream &os, const matched_labels &match) -> decltype(os << "")
  {
    os << "labels {\n";
    for (const auto &m : match.labels) {
      os << "\t" << m << "\n";
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
  using Matches = std::vector< matched_labels >;

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
        [&] (const bond_expr & )  -> Matches {
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
    Matches filter_constrained_disjoint_contexts(
        Matches &&matches, const disjoint_expr &constraint) const {
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
    using named_contexts =
        std::unordered_map<std::string, std::vector<context_t>>;

    named_contexts get_named_contexts(const anonymous_match &match) const { return {}; /* none */ }

    named_contexts get_named_contexts(const unary_match &match) const
    {
      named_contexts ctxs;
      if (auto sub = pattern.subexpr(match.label); sub.context) {
        auto name = std::string(sub.context.value());
        ctxs[name].push_back( contexts(egraph, match.id) );
      }
      return ctxs;
    }

    named_contexts get_named_contexts(const variadic_match &match) const
    {
      named_contexts ctxs;
      if (auto sub = pattern.subexpr(match.label); sub.context) {
        auto ctx = sub.context.value();
        int i = 1;
        for (auto id : match.ids) {
          auto name = std::string("_") + std::string(ctx) + std::to_string(i++);
          ctxs[name].push_back( contexts(egraph, id) );
        }
      }
      return ctxs;
    }

    named_contexts get_named_contexts(const label_match &match) const
    {
      return std::visit([&] (const auto &m) { return get_named_contexts(m); }, match);
    }

    named_contexts get_named_contexts(const std::vector< label_match > &labels) const
    {
      named_contexts res;
      for (const auto &lab : labels)
        res.merge(get_named_contexts(lab));
      return res;
    }

    // TODO(Heno): deal with varidadic contexts
    // Returns true if 'match' has non overlapping context sets
    bool has_disjoint_contexts(const matched_labels &match, const auto &contexts) const
    {
      // TODO(Heno): simplify with set
      auto named = get_named_contexts(match.labels);

      std::unordered_map< context_node, int > counts;
      for (const auto &context : contexts) {
        std::visit(overloaded{
          [&](const single_context &sc) {
            for (const auto &nodes : named.at(std::string(sc.ref()))) {
              for (const auto &node : nodes) {
                counts[node]++;
              }
            }
          },
          [&](const variadic_context &vc) {
            for (const auto &[name, set] : named) {
              if (name.starts_with('_') && name.substr(1).starts_with(vc.ref()))
                for (const auto &nodes : set) {
                  for (const auto &node : nodes) {
                    counts[node]++;
                  }
                }
            }
          }}, context);
      }

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
      auto nonunique = [] (const auto &match) {
        std::set< Id > seen;

        auto not_seen = [&] (Id id) -> bool { return seen.insert(id).second; };

        auto unique = [&] (const label_match &lab) {
          return std::visit( overloaded {
            [&] (const anonymous_match &m) { return not_seen(m.id); },
            [&] (const unary_match &m)     { return not_seen(m.id); },
            [&] (const variadic_match &m)  { return std::all_of(m.ids.begin(), m.ids.end(), not_seen); }
          }, lab);
        };

        return !std::all_of( match.labels.begin(), match.labels.end(), unique );
      };

      matches.erase( std::remove_if(matches.begin(), matches.end(), nonunique), matches.end() );
      return std::move(matches);
    }

    static inline Matches filter_commutative_matches(Matches &&matches)
    {
      using MatchedIds = std::set< Id >;
      std::vector< MatchedIds > seen;

      auto commutes = [&] (const auto &match) {
        auto ids = matched_ids(match);
        if (std::find(seen.begin(), seen.end(), ids) != seen.end())
          return true;
        seen.push_back(ids);
        return false;
      };

      matches.erase( std::remove_if(matches.begin(), matches.end(), commutes), matches.end() );
      return std::move(matches);
    }

    static inline std::optional< matched_labels > merge(const matched_labels &lhs, const matched_labels &rhs)
    {
      auto product = product_of_substitutions(lhs.substitutions, rhs.substitutions);
      if (product.empty())
        return std::nullopt;

      matched_labels match{lhs.labels, std::move(product)};

      for (const auto &lab : rhs.labels) {
        check(!get_label_match(match, lab).has_value());
        match.labels.push_back(lab);
      }

      return match;
    }

    // TODO(Heno): optimize copies of vectors and maps
    static inline Matches product_of_matches(const auto &matches)
    {
      check(matches.size() > 0);
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
      for (const auto &l : labels(e)) {
        matches.push_back(match(pattern.subexpr(l), l));
      }

      auto result = combine_matches(matches);

      result = filter_nonunique_matches(std::move(result));

      if (std::holds_alternative< commutative_match_expr >(e))
        return filter_commutative_matches(std::move(result));
      return result;
    }

    Matches match(const auto &e, label lab = anonymous_label()) const
    {
      return std::visit( [&] (const auto &l) { return match(e, l); }, lab );
    }

    using single_match = std::pair< Id, Substitutions >;

    // pick m elements from n
    using combination  = std::vector< std::size_t >;
    using combinations = std::vector< combination >;

    static bool for_combination(std::size_t n, std::size_t m, std::size_t off,
                                combination curr, auto yield)
    {
      if (m == 0) {
        return yield(std::move(curr));
      }

      for (std::size_t i = off; i <= n - m; ++i) {
        combination next = curr;
        next.push_back(i);
        if (for_combination(n, m - 1, i + 1, std::move(next), yield))
          return true;
      }

      return false;
    }

    static bool for_combination(std::size_t n, std::size_t m, auto yield) {
      return for_combination(n, m, 0, {}, yield);
    }

    static Matches make_viariadic_match(const std::vector< single_match > &matched,
                                        const combination& comb, variadic_label l)
    {
      std::set<Id> ids;
      Substitutions subs;

      for (auto idx : comb) {
        const auto &[matched_id, matched_subs] = matched[idx];
        ids.insert(matched_id);
        subs.insert(matched_subs.begin(), matched_subs.end());
      }

      variadic_match vmatch{l, ids};
      return { matched_labels{{vmatch}, subs} };
    }

    // not optimal but works for greedy solutions
    Matches match(const auto &e, variadic_label l) const
    {
      std::vector< single_match > matched;
      for (const auto &[id, eclass] : egraph.classes()) {
        if (auto subs = match_eclass(eclass, e); !subs.empty()) {
          matched.emplace_back(id, std::move(subs));
        }
      }

      for (std::size_t pick = matched.size(); pick > 0; --pick) {
        std::optional<Matches> result = std::nullopt;
        circ::log_info() << "[eqsat] trying combination of size " << pick;
        for_combination(matched.size(), pick, [&] (auto comb) {
          auto matches = make_viariadic_match(matched, comb, l);

          // return the first match that satisfies constraints
          for (const auto &con : pattern.context_constraints) {
            matches = filter_constrained_disjoint_contexts(std::move(matches), con);
          }

          if (matches.size() > 0) {
            result = std::move(matches);
          }

          return result.has_value();
        });

        if (result.has_value())
          return result.value();
      }

      return {};
    }

    unary_match named_match(const unary_label &l, Id id) const { return {l, id}; }
    anonymous_match named_match(const anonymous_label &, Id id) const { return {id}; }

    Matches match_single(const auto &e, const auto &l) const
    {
      Matches matches;
      // TODO(Heno): add operation caching to egraph
      for (const auto &[id, eclass] : egraph.classes()) {
        // pattern matches with a root in the eclass
        if (auto subs = match_eclass(eclass, e); !subs.empty()) {
          matched_labels single;
          single.labels.push_back(named_match(l, id));
          single.substitutions.merge(std::move(subs));
          matches.push_back(single);
        }
      }
      return matches;
    }

    Matches match(const auto &e, unary_label l)     const { return match_single(e, l); }
    Matches match(const auto &e, anonymous_label l) const { return match_single(e, l); }

    // match pattern with some of the eclass nodes (produces all possible matches)
    Substitutions match_eclass(const auto &eclass, const auto &e) const
    {
      Substitutions res;
      for (const auto &node : eclass.nodes) {
        // FIXME: ignore eclasses that contain bond nodes for now
        // This solves infinite chains of bonds
        if (node->is_bond_node()) {
          return {};
        }
        res.merge(match_enode(node, e));
      }
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
      auto repr = [&] {
        if (auto bw = bitwidth(enode)) {
          return std::string(name(enode)) + ":" + std::to_string(bw.value());
        }
        return name(enode);
      };
      return trivial(repr() == o.full_name(), places.size());
    }

    // match place (variable) node, returns single substitution with
    // a matched place to the enode
    Substitutions match_atom(const auto &enode, const place &p) const
    {
      auto id = egraph.find(enode);
      return {Substitution{places.size(), places.at(p), id}};
    }

    // match labeled subexpression
    Substitutions match_atom(const auto &enode, const unary_label &lab) const
    {
      return match_enode(enode, pattern.subexpr(lab));
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
        [&] (const bond_expr &e)  { apply(e, matches); },
        [&] (const match_expr & ) {
          throw std::runtime_error("match clause is forbidden in the rewrite pattern");
        }
      }, e.get());
    }

    Id apply(const expr &e, const matched_labels &match) const
    {
      return std::visit( overloaded {
        [&] (const atom &a) -> Id {
          throw std::runtime_error("rewrite rule is applied on the level of expression lists");
        },
        [&] (const expr_list  &e) -> Id { return apply(e, match); },
        [&] (const union_expr &e) -> Id { return apply(e, match); },
        [&] (const bond_expr &e)  -> Id { return apply(e, match); },
        [&] (const match_expr & ) -> Id {
          throw std::runtime_error("match clause is forbidden in the rewrite pattern");
        }
      }, e.get());
    }

    std::set< Id > matched_ids_for_labels(const matched_labels &match,
                                          const std::vector< label > &labels) const
    {
      // TODO(Heno): use coroutines here
      std::set< Id > ids;

      for (const auto & l : labels) {
        auto name = label_name(l);
        if (auto m = get_label_match(match, name)) {
          ids.merge(matched_ids(m.value()));
        } else {
          assert( pattern.subexpr(l) );
          auto sub = pattern.subexpr(l);
          ids.insert( apply(sub, match) );
        }
      }

      return ids;
    }

    Id apply(const bond_expr &e, const matched_labels &match) const
    {
      auto ids = matched_ids_for_labels(match, e.labels);
      return egraph.bond({ids.begin(), ids.end()});
    }

    Id apply(const union_expr &e, const matched_labels &match) const
    {
      auto ids = matched_ids_for_labels(match, e.labels);
      return egraph.merge({ids.begin(), ids.end()});
    }

    Id apply(const expr_list &list, const matched_labels &match) const
    {
      if( is_nested(list) ) {
        throw std::runtime_error("nested expression in rewrite pattern");
      }

      return apply_patch(list, match);
    }

    void apply(const bond_expr &e, const Matches &matches) const
    {
      for (const auto &match : matches) {
        apply(e, match);
      }
    }

    void apply(const union_expr &e, const Matches &matches) const
    {
      for (const auto &match : matches) {
        apply(e, match);
      }
    }

    void apply(const expr_list &list, const Matches &matches) const
    {
      if (!is_nested(list))
        return apply_patch(list, matches);

      for (const auto &e :list)
        apply(e, matches);
    }

    std::optional< Id > anonymous_match_id(const matched_labels &matches) const
    {
      if (auto match = get_label_match(matches, anonymous_match()))
        return std::get<anonymous_match>(match.value()).id;
      return std::nullopt;
    }

    Id apply_patch(const expr &e, const matched_labels &match) const
    {
      auto id = anonymous_match_id(match);
      // TODO(Heno): perform once for all substitutions
      for (const auto &sub : match.substitutions) {
        auto patch = builder.synthesize(e, sub, places, pattern.subexprs);
        id = id ? egraph.merge(id.value(), patch) : patch;
      }
      return id.value();
    }

    void apply_patch(const expr &e, const Matches &matches) const
    {
      for (const auto &match : matches) {
        apply_patch(e, match);
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
    Rule(const std::string &name, const std::string &lhs_,
         const std::string &rhs_)
        : name(name),
          lhs(make_pattern(lhs_)),
          rhs(make_pattern(rhs_)),
          places(get_indexed_places(lhs))
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

  using RewriteRule = Rule<CircuitEGraph>;
  using RewriteRules = Rules<CircuitEGraph>;

  static inline std::optional<std::string_view>
  parse_ruleset_name(std::string_view line) {
    if (!line.starts_with('[')) {
      return std::nullopt;
    }
    if (!line.ends_with(']')) {
      log_error() << "missing closing bracket: " << line;
      return std::nullopt;
    }
    return line.substr(1, line.size() - 2);
  }

  struct RuleSet {
    std::string name;
    RewriteRules rules;
  };

  using MaybeRuleSet = std::optional<RuleSet>;

  using MaybeString = std::optional<std::string>;

  static inline MaybeRuleSet maybe_new_ruleset(std::string_view line) {
    if (auto name = parse_ruleset_name(line))
      return RuleSet{std::string{name.value()}, RewriteRules{}};
    return std::nullopt;
  }

  static inline MaybeString parse_rule_name(std::string_view line) {
    if (line.ends_with(':'))
      return std::string(line.substr(0, line.size() - 1));
    return std::nullopt;
  }

  static inline std::string_view ltrim(std::string_view line) {
    line.remove_prefix(
        std::min(line.find_first_not_of(" \n\r\t"), line.size()));
    return line;
  }

  static inline bool is_commented(std::string_view line) {
    return ltrim(line).starts_with('#');
  }

  static inline std::optional<std::string> get_nonempty_line(std::istream &is) {
    std::string line;
    while (std::getline(is, line)) {
      if (line.empty()) {
        /* noop */
      } else if (is_commented(line)) {
        /* noop */
      } else {
        return line;
      }
    }

    return std::nullopt;
  }

  static inline MaybeString parse_pattern(std::string_view line) {
    line = ltrim(line);
    if (line.starts_with('-'))
      return std::string(line.substr(2));
    return std::nullopt;
  }

  static inline std::optional<RewriteRule>
  parse_rule(std::string_view name_line, std::istream &is) {
    auto pattern = [&]() -> MaybeString {
      if (auto line = get_nonempty_line(is)) {
        if (auto pat = parse_pattern(*line)) {
          return pat;
        } else {
          log_error() << "expected a pattern: " << *line;
          return std::nullopt;
        }
      }

      log_error() << "missing pattern";
      return std::nullopt;
    };

    if (auto name = parse_rule_name(name_line)) {
      log_info() << "rule: " << *name;
      auto lhs = pattern();
      auto rhs = pattern();
      if (lhs && rhs) {
        log_info() << "lhs: " << *lhs;
        log_info() << "rhs: " << *rhs;
        return RewriteRule{*name, *lhs, *rhs};
      }
    } else {
      log_error() << "expected rule name: " << name_line;
      return std::nullopt;
    }

    return std::nullopt;
  }

  static inline std::vector<RuleSet> parse_rules(std::istream &is) {
    std::vector<RuleSet> rulesets;

    auto add_to_current_ruleset = [&](auto &&rule) {
      rulesets.back().rules.push_back(std::move(rule));
    };

    while (auto line = get_nonempty_line(is)) {
      if (auto ruleset = maybe_new_ruleset(*line)) {
        log_info() << "new set of rules: " << ruleset->name << '\n';
        rulesets.push_back(std::move(*ruleset));
      } else if (auto rule = parse_rule(*line, is)) {
        add_to_current_ruleset(std::move(*rule));
      } else {
        unreachable() << "syntax error: " << *line << '\n';
      }
    }

    return rulesets;
  }

  static inline std::vector<RuleSet> parse_rules(std::string_view filename) {
    std::ifstream file(std::string(filename), std::ios::in);
    return parse_rules(file);
  }

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

  CircuitPtr EqualitySaturation(CircuitPtr &&, std::span<RuleSet> rules);

  template< typename Graph, typename Builder >
  void lower_advices(Graph &egraph, const Builder &builder)
  {
    using Node = typename Graph::Node;
    using UseEdge = typename Graph::UseEdge;

    auto enumerate = [] (const auto &container, auto &&fn) {
      std::size_t i = 0;
      for (const auto &val : container) {
          fn(i++, val);
      }
    };

    // gather all parent -> advice edges
    std::set< UseEdge > adviced;
    for (const auto &[node, id] : egraph.nodes()) {
      if (name(node) == "Advice")
        adviced.merge(egraph.parents(node));
    }

    // yield bond nodes that are in equality class with
    // a parent from adviced edge
    auto adviced_bonds = [&] (auto edge, auto yield) {
      auto parent_eclass = egraph.eclass(edge.parent);
      for (auto node : parent_eclass.nodes) {
        if (node != edge.parent) {
          if (node->is_bond_node()) {
            yield(node);
          }
        }
      }
    };

    // obtains contexts for a child of a bond node
    auto hook_to_contexts = [&] (const Node *bond, std::size_t idx, auto yield) {
      const auto &node = std::get< BondNode >( *bond );

      auto parents_from   = idx ? node.children_parents[idx - 1] : 0;
      auto parents_to     = node.children_parents[idx];
      const auto &parents = egraph.parents(egraph.find(bond));

      for (auto i = parents_from; i < parents_to; i++) {
        for (auto ctx : contexts(egraph, egraph.find(parents[i]))) {
          yield(ctx);
        }
      }
    };

    // traverse all children of bond nodes that are linked to an adviced edge
    for (const auto &edge : adviced) {
      adviced_bonds(edge, [&] (const Node* bond) {
        enumerate(bond->children(), [&] (auto bonded_idx, auto bonded_eclass) {
          for (const auto &bonded : egraph.eclass(bonded_eclass).nodes) {
            // constrain adviced value by child of bonded node
            auto arg = bonded->children().at(edge.index);
            auto advice = egraph.find(edge.child);
            auto constraint  = builder.constrain(arg, advice);
            // hook constraints to dedicated contexts
            hook_to_contexts(bond, bonded_idx, [&] (auto ctx) {
              const auto &children = ctx->children();
              if (auto it = std::find(children.begin(), children.end(), constraint) == children.end()) {
                ctx->children().push_back(constraint);
              }
            });
          }
        });
      });
    }
  }

} // namespace circ::eqsat
