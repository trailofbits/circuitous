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
#include <span>
#include <string_view>

#include <circuitous/Transforms/Pattern.hpp>

namespace circuitous::eqsat {

  template< typename Graph >
  struct Rule
  {
    using Id = typename Graph::Id;
    using Place = ASTNode::Place;

    // substitution mapping from places (variables) to equality classes
    struct Substitution
    {
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
        assert(test(idx));
        return _mapping[idx];
      }

      size_t size() const { return _mapping.size(); }

      value_type get(size_t idx) const
      {
        return { _mapping[idx], _matched[idx] };
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

    // Result of matched nodes rooting in the given equality class
    using EClassMatch = std::pair< Id, Substitutions >;
    using Matches = std::vector< EClassMatch >;

    Rule(std::string_view name, std::string_view lhs, std::string_view rhs)
      : name(name), _lhs(Pattern(lhs)), _rhs(Pattern(rhs))
    {}

    struct EGraphMatcher
    {
      using PatternNode = ASTNodePtr;
      using ENode = typename Graph::ENode;
      using EClass = typename Graph::EClass;
      using Id = typename Graph::Id;

      using Constant = ASTNode::Constant;
      using Place    = ASTNode::Place;
      using Op       = ASTNode::Op;

      EGraphMatcher(const Graph &egraph, const Pattern &pattern)
        : _egraph(egraph), _pattern(pattern)
      {}

      // helper for patter visitor
      template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
      template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

      using MatchResult = std::optional< Substitutions >;

      MatchResult empty_match(bool is_matched)
      {
        if (is_matched)
          return Substitutions{Substitution(_pattern.places)};
        return std::nullopt;
      }

      static MatchResult unmatched() { return std::nullopt; }

      MatchResult match(const EClass &eclass) { return match(eclass, _pattern.value); }

      MatchResult match(const EClass &eclass, const PatternNode &pattern)
      {
        // performs union of all matches on the root nodes in the given eclass
        MatchResult result = unmatched();
        for (const auto *node : eclass.nodes) {
          if (auto &&subs = match(node, pattern)) {
            // TODO(Heno): check all places are matched
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
            Substitution sub( _pattern.places );
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

        auto child = root->children.begin();
        for (const auto &node : pattern->children) {
          auto child_class = _egraph.eclass(*child);
          if (auto &&sub = match(child_class, node)) {
            children.push_back(std::move(sub.value()));
          } else {
            return unmatched();
          }
          ++child;
        }

        Substitutions product = head.value();
        // make product of all matches from all children
        for (auto &&child : children) {
          if (child.empty())
            continue;

          if (product.empty()) {
            product = std::move(child);
            continue;
          }

          Substitutions next;
          for (const auto &substitution : child) {
            std::optional< Substitution > partial;
            // make product of all alternatives with all alreadz processed children
            for (const auto &matched : product) {
              for (size_t idx = 0; idx < substitution.size(); ++idx) {
                auto [id, set] = substitution.get(idx);
                if (!set)
                  continue;
                // check whether matches between children do not conflict
                // placeholder no yet matched
                if (!matched.test(idx)) {
                  partial = matched;
                  partial->set(idx, id);
                // already matched and does not conflict with other children match
                } else if (matched.id(idx) == id) {
                  partial = matched;
                }
                // otherwise we skip match
              }
            }

            if (partial.has_value())
              next.insert(std::move(partial.value()));
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

    struct Matcher
    {
      Matcher(Pattern &&pattern) : _pattern(std::move(pattern)) {}

      Matches match(const Graph &egraph) const
      {
        // TODO(Heno): add operation caching to egraph

        Matches matches;
        for (const auto &[id, eclass] : egraph.classes()) {
          EGraphMatcher matcher(egraph, _pattern);

          // pattern matches with a root in the eclass
          if (auto &&substitutions = matcher.match(eclass))
            matches.emplace_back( id, std::move(substitutions.value()) );
        }
        return matches;
      }

    private:
      Pattern _pattern;
    };

    struct Applier
    {
      Applier(Pattern &&pattern) : _pattern(std::move(pattern)) {}

      void apply_on_matches(Graph &egraph, const Matches &matches) const
      {
        for (const auto &match : matches) {
          apply_on(egraph, match);
        }
      }

      void apply_on(Graph &egraph, const EClassMatch &match) const
      {

      }

    private:
      Pattern _pattern;
    };

    Matches match(const Graph &egraph) const
    {
      return _lhs.match(egraph);
    }

    void apply(Graph &egraph, const Matches &matches) const
    {
      _rhs.apply_on_matches(egraph, matches);
    }

    void apply(Graph &egraph) const
    {
      auto matches = mathc(egraph);
      apply(egraph, matches);
    }

    const std::string name;

  private:
    // Rewrite rule 'lhs -> rhs' that allows to match
    // left-hand-side and replace it with right-hand-side
    Matcher _lhs;
    Applier _rhs;
  };

  template< typename Graph >
  using Rules = std::vector< Rule< Graph > >;


  template< typename Graph >
  struct BasicRulesScheduler
  {
    using Rule = Rule< Graph >;
    using Matches = typename Rule::Matches;

    Matches match_rule(const Graph &egraph, const Rule &rule) const
    {
      return rule.match(egraph);
    }

    void apply_rule(Graph &egraph, const Rule &rule, const Matches &matches) const
    {
      rule.apply(egraph, matches);
    }

    void apply_rule(Graph &egraph, const Rule &rule) const
    {
      rule.apply(egraph);
    }

  };

} // namespace circuitous::eqsat