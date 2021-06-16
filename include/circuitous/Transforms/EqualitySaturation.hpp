/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once


#include <algorithm>
#include <cstddef>
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
#include <circuitous/Transforms/Pattern.hpp>

namespace circuitous::eqsat {

  // helper for pattern visitor
  template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
  template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

  // substitution mapping from places (variables) to equality classes
  template< typename Graph >
  struct Substitution
  {
    using Id = typename Graph::Id;
    using Place = ASTNode::Place;

    using value_type = std::pair< Id, bool >;

    Substitution(size_t size) : _mapping(size, 0) , _matched(size, false) {}

    bool test(size_t idx) const
    {
      return _matched[idx];
    }

    void set(size_t idx, Id id)
    {
      _matched[idx] = true;
      _mapping[idx] = id;
    }

    bool try_set(Place place, Id id)
    {
      auto idx = place.ref();
      if (!test(idx))
        return false;
      set(idx, id);
      return true;
    }

    Id id(size_t idx) const
    {
      CHECK(test(idx));
      return _mapping[idx];
    }

    size_t size() const { return _mapping.size(); }

    value_type get(size_t idx) const
    {
      return { _mapping[idx], _matched[idx] };
    }

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

  template< typename Graph >
  using Substitutions = std::set< Substitution< Graph > >;

  // Result of matched nodes rooting in the given equality class
  template< typename Graph >
  using EClassMatch = std::pair< typename Graph::Id, Substitutions< Graph > >;

  template< typename Graph >
  using Matches = std::vector< EClassMatch< Graph > >;

  template< typename Graph >
  struct EGraphMatcher
  {
    using PatternNode = ASTNodePtr;
    using ENode = typename Graph::ENode;
    using EClass = typename Graph::EClass;
    using Id = typename Graph::Id;

    using Constant = ASTNode::Constant;
    using Place    = ASTNode::Place;
    using Op       = ASTNode::Op;

    using Substitution = Substitution< Graph >;
    using Substitutions = Substitutions< Graph >;

    EGraphMatcher(const Graph &egraph, const Pattern &pattern)
      : _egraph(egraph), _pattern(pattern)
    {}

    using MatchResult = std::optional< Substitutions >;

    MatchResult empty_match(bool is_matched)
    {
      if (is_matched)
        return Substitutions{Substitution(_pattern.places.size())};
      return std::nullopt;
    }

    static MatchResult unmatched() { return std::nullopt; }

    MatchResult match(const EClass &eclass)
    {
      if (auto result = match(eclass, _pattern.ast)) {
        // filter out non-fully matched substitutions
        std::erase_if(result.value(), [](const auto &sub) {
          return !sub.fully_matched();
        });
        return result;
      }

      return std::nullopt;
    }

    MatchResult match(const EClass &eclass, const PatternNode &pattern)
    {
      // performs union of all matches on the root nodes in the given eclass
      MatchResult result = unmatched();
      for (const auto *node : eclass.nodes) {
        if (auto subs = match(node, pattern)) {
          if (!result)
            result = std::move(subs);
          else
            result->merge(std::move(subs.value()));
        }
      }
      return result;
    }

    MatchResult match_one(const ENode *root, const PatternNode &pattern)
    {
      return std::visit( overloaded {
        [&] (const Constant &con) -> MatchResult {
          if (auto node_con = root->constant())
            return empty_match(node_con == con);
          return unmatched();
        },
        [&] (const Place &plc) -> MatchResult {
          auto id = _egraph.find(root);
          Substitution sub( _pattern.places.size() );
          sub.set(plc.ref(), id);
          return Substitutions{sub};
        },
        [&] (const Op &op) -> MatchResult {
          return empty_match(root->name() == op);
        },
        [&] (const auto&) -> MatchResult { return unmatched(); /* unsupported kind */ },
      }, pattern->value );
    }

    MatchResult match(const ENode *root, const PatternNode &pattern)
    {
      auto head = match_one(root, pattern);
      if (!head)
        return unmatched();
      if (pattern->children.empty())
        return head;

      std::vector< Substitutions > children;
      {
        auto child = root->children.begin();
        for (const auto &node : pattern->children) {
          auto child_class = _egraph.eclass(*child);
          if (auto sub = match(child_class, node)) {
            children.push_back(std::move(sub.value()));
          } else {
            return unmatched();
          }
          ++child;
        }
      }

      auto merge = [] (auto lhs, const auto &rhs) -> std::optional< Substitution > {
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
      };

      Substitutions product = head.value();
      // make product of all matches from all children
      for (auto child : children) {
        if (child.empty())
          continue;

        if (product.empty()) {
          product = std::move(child);
          continue;
        }

        Substitutions next;
        // make product of all alternatives with all already processed children
        for (const auto &substitution : child) {
          for (const auto &matched : product) {
            if (auto prod = merge(matched, substitution))
              next.insert(std::move(prod.value()));
          }
        }
        std::swap(product, next);
      }

      return product;
    }

    template< typename stream >
    static void print_substitutions(const Substitutions &substitutions, stream &out)
    {
      int i = 0;
      if (substitutions.empty())
        out << "empty substitutions\n";
      for (const auto &sub : substitutions) {
        out << "alternative " << i++ << '\n';
        for (size_t i = 0; i < sub.size(); ++i) {
          if (sub.test(i))
            out << i << " -> " << sub.id(i) << '\n';
        }
      }
    }

  private:
    const Graph &_egraph;
    const Pattern &_pattern;
  };

  template< typename Graph >
  struct Matcher
  {
    using Matches = Matches< Graph >;

    Matcher(const Pattern &pattern) : _pattern(pattern) {}

    Matches match(const Graph &egraph) const
    {
      // TODO(Heno): add operation caching to egraph

      Matches matches;
      for (const auto &[id, eclass] : egraph.classes()) {
        EGraphMatcher matcher(egraph, _pattern);

        // pattern matches with a root in the eclass
        if (auto substitutions = matcher.match(eclass))
          matches.emplace_back( id, std::move(substitutions.value()) );
      }

      return matches;
    }

  private:
    const Pattern &_pattern;
  };

  template< typename Graph, typename Builder >
  struct Applier
  {
    using Matches = Matches< Graph >;
    using EClassMatch = EClassMatch< Graph >;

    Applier(const Pattern &pattern, Builder builder)
      : _pattern(pattern), _builder(builder)
    {}

    void apply_on_matches(Graph &egraph, const Matches &matches) const
    {
      for (const auto &match : matches) {
        apply_on(egraph, match);
      }
    }

    void apply_on(Graph &egraph, const EClassMatch &match) const
    {
      const auto &[id, substitutions] = match;
      // TODO(Heno): perform once for all substitutions
      for (const auto &sub : substitutions) {
        auto patch = _builder.synthesize(_pattern, sub);
        egraph.merge(id, patch);
      }
    }

  private:
    const Pattern &_pattern;
    Builder _builder;
  };

  template< typename Graph >
  struct Rule
  {
    Rule(std::string_view name, std::string_view lhs_, std::string_view rhs_)
      : name(name), lhs(lhs_), rhs(rhs_, lhs.places)
    {}

    using Matches = Matches< Graph >;

    Matches match(const Graph &egraph) const
    {
      using Matcher = Matcher< Graph >;

      return Matcher(lhs).match(egraph);
    }

    template< typename Builder >
    void apply(Graph &egraph, Builder builder, const Matches &matches) const
    {
      using Applier = Applier< Graph, Builder >;

      Applier(rhs, builder).apply_on_matches(egraph, matches);
    }

    template< typename Builder >
    void apply(Graph &egraph, Builder builder) const
    {
      auto matches = match(egraph);
      apply(egraph, builder, matches);
    }

    const std::string name;
    // Rewrite rule 'lhs -> rhs' that allows to match
    // left-hand-side and replace it with right-hand-side
    Pattern lhs;
    Pattern rhs;
  };


  template< typename Graph >
  using Rules = std::vector< Rule< Graph > >;

  template< typename Graph, typename Builder >
  struct BasicRulesScheduler
  {
    using Rule = Rule< Graph >;
    using Matches = Matches< Graph >;

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

} // namespace circuitous::eqsat