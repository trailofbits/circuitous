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
#include <vector>
#include <string>
#include <string_view>

#include <circuitous/IR/IR.h>
#include <circuitous/ADT/UnionFind.hpp>
#include <circuitous/Transforms/Pattern.hpp>

namespace circ::eqsat {

  using Id = UnionFind::Id;

  struct OpCode
  {
    std::string op_code_name;
  };

  struct SizedOp
  {
    std::string op_code_name;
    std::optional< std::uint32_t > size;
  };

  struct RegOp
  {
    std::string op_code_name;
    std::uint32_t size;
    std::string reg_name;
  };

  struct ConstOp
  {
    std::string op_code_name;
    std::uint32_t size;
    std::string bits;
  };

  struct MemOp
  {
    std::string op_code_name;
    std::uint32_t mem_idx;
  };

  struct ExtractOp
  {
    std::string op_code_name;
    std::uint32_t low_bit_inc, high_bit_exc;
  };

  struct SelectOp
  {
    std::string op_code_name;
    std::uint32_t size;
    std::uint32_t bits;
  };

  using OpTemplate = std::variant< OpCode, SizedOp, RegOp, ConstOp, MemOp, ExtractOp, SelectOp >;

  std::string to_string(const OpTemplate &op)
  {
    return std::visit([] (const auto &o) { return o.op_code_name; }, op);
  }

  using CircuitENode = ENode< OpTemplate >;
  using CircuitEGraph = EGraph< CircuitENode >;

  static inline std::string name(const CircuitENode *node)
  {
    return std::visit( [] (const auto &value) { return value.op_code_name; }, node->term );
  }

  static inline bool is_context_node(const CircuitENode *node)
  {
    return name(node) == "VerifyInstruction";
  }

  static inline std::optional<std::int64_t> extract_constant(const CircuitENode *node)
  {
    throw std::runtime_error("not implemented");
  }

  template< typename Graph, typename ENode = typename Graph::ENode >
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

  private:
    std::vector< Id > _mapping;
    std::vector< bool > _matched;
  };

  using Substitutions = std::set< Substitution >;

  using MatchLabel = std::variant< std::monostate, label >;

  static inline constexpr MatchLabel anonymous_label = std::monostate{};

  // Single match of labeled expressions, maps to each matching label
  // a single equality class.
  // Maintains possible substitutions of pattern plces for current match.
  // A special case is anonymous match for unlabeled pattern.
  struct LabeledMatch
  {
    using labels_map = std::unordered_map< MatchLabel, Id >;

    // matching of labels to equality nodes
    labels_map labels;
    // corresponding substitutions for the matching
    Substitutions substitutions;

    bool is_anonymous() const { return labels.count(anonymous_label); }
  };

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

    Matches filter_constrained(Matches &&matches) const
    {
      if (pattern.constraints.contexts.size() < 2)
        return std::move(matches);

      matches.erase(
        std::remove_if(matches.begin(), matches.end(), [&](const auto &match) {
          return constrained(match);
        }),
        matches.end()
      );

      return std::move(matches);
    }

    using context_node = typename Graph::ENode*;
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

    bool constrained(const LabeledMatch &match) const
    {
      auto named = get_named_contexts(match.labels);

      std::unordered_map< context_node, int > counts;
      for (const auto &name : pattern.constraints.contexts)
        for (const auto &node : named.at(name.ref()))
          counts[node]++;

      return std::any_of(counts.begin(), counts.end(), [] (const auto &elem) {
        const auto &[_, count] = elem;
        return count > 1;
      });
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
      // MatchedClasses matched;
      std::vector< Matches > matches;
      for (const auto &label : e.labels) {
        matches.push_back(match(pattern.subexprs.at(label), label));
      }

      return combine_matches(matches);
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

        // gather substitutions for all children
        std::vector< Substitutions > subs;
        auto child = enode->children.begin();
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
    Substitutions match_atom(const auto &enode, const label &lab) const
    {
      return match_enode(enode, pattern.subexprs.at(lab));
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
        [&] (const match_expr & ) {
          throw std::runtime_error("match clause is forbidden in the rewrite pattern");
        }
      }, e.get());
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
    pattern lhs;
    pattern rhs;

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

} // namespace circ::eqsat