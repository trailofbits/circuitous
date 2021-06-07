/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once


#include <cstddef>
#include <functional>
#include <iterator>
#include <map>
#include <optional>
#include <unordered_map>
#include <utility>
#include <vector>
#include <string>
#include <span>
#include <string_view>

#include <circuitous/Transforms/Pattern.hpp>

namespace circuitous {

  template< typename Place >
  struct PlaceHasher
  {
    std::size_t operator()(const Place &plc) const
    {
      return std::hash<std::string>()(plc.name);
    }
  };

  // Substitution(const place_t &plc, Id id)
  // {
  //   mapping.emplace(plc, id);
  // }

  template< typename Graph >
  struct Rule
  {
    using Pattern = Pattern< Graph >;
    using Id = typename Graph::Id;

    using Place = typename Pattern::Place;
    using PlaceHasher = PlaceHasher< Place >;

    // substitution mapping from places (variables) to equality classes
    using Substitution = std::unordered_map<Place, Id, PlaceHasher>;
    using Substitutions = std::vector< Substitution >;

    // Result of matched nodes rooting in the given equality class
    using EClassMatch = std::pair< Id, Substitutions >;
    using Matches = std::vector< EClassMatch >;

    Rule(std::string_view name, std::string_view lhs, std::string_view rhs)
      : name(name), _lhs(Pattern(lhs)), _rhs(Pattern(rhs))
    {}

    struct EGraphMatcher
    {
      using PatternNode = typename Pattern::ASTNodePtr;
      using ENode = typename Graph::ENode;
      using EClass = typename Graph::EClass;
      using Id = typename Graph::Id;

      using constant_t = typename Pattern::Constant;
      using place_t    = typename Pattern::Place;
      using op_t       = typename Pattern::Op;

      EGraphMatcher(const Graph &egraph, const Pattern &pattern)
        : _egraph(egraph), _pattern(pattern)
      {}

      // helper for patter visitor
      template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
      template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

      using MatchResult = std::optional< Substitutions >;

      static MatchResult empty_match(bool is_matched)
      {
        if (is_matched)
          return Substitutions{};
        return std::nullopt;
      }
      static MatchResult unmatched() { return std::nullopt; }

      MatchResult match(const EClass &eclass) { return match(eclass, _pattern.value); }

      MatchResult match(const EClass &eclass, const PatternNode &pattern)
      {
        // performs union of all matches on the root nodes in the given eclass
        // TODO(Heno): maybe we may have duplicits and we need to filter them out
        MatchResult result = unmatched();
        for (const auto *node : eclass.nodes) {
          if (auto &&subs = match(node, pattern)) {
            if (!result)
              result = std::move(subs);
            else
              result->insert(result->end(), std::make_move_iterator(subs->begin())
                                          , std::make_move_iterator(subs->end()));
          }
        }
        return result;
      }

      MatchResult match_one(const ENode *root, const PatternNode &pattern)
      {
        return std::visit( overloaded {
          [&] (const constant_t &con) -> MatchResult {
            if (auto node_con = root->constant())
              return empty_match(node_con == con);
            return unmatched();
          },
          [&] (const place_t &plc) -> MatchResult {
            auto id = _egraph.find(root);
            Substitution sub{{plc, id}};
            return Substitutions{sub};
          },
          [&] (const op_t &op) -> MatchResult {
            return empty_match(root->name() == op.name);
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
        // // TODO(Heno) match size

        // match children
        std::vector< Substitutions > results;

        auto child = root->children.begin();
        for (const auto &node : pattern->children) {
          auto child_class = _egraph.eclass(*child);
          if (auto &&sub = match(child_class, node)) {
            results.push_back(std::move(sub.value()));
          } else {
            return unmatched();
          }
          ++child;
        }

        Substitutions product = head.value();
        // make product of all matches from all children
        for (auto &&child : results) {
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
              for (const auto &[plc, id] : substitution) {
                // we need to check whether matches between children do not conflict
                auto found = matched.find(plc);
                // match not yet present
                if (found == matched.end()) {
                  partial = matched;
                  partial->emplace(plc, id);
                // already matched && does not conflict with other children match
                } else if (found->second == id) {
                  partial = matched;
                }
                // otherwise we skip match
              }
            }

            if (partial.has_value())
              next.push_back(std::move(partial.value()));
          }
          std::swap(product, next);
        }

        // std::cout << "merged alternatives\n";
        // print_substitutions(product, std::cout);
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
          if (sub.empty())
            out << "empty\n";
          for (const auto &[place, id] : sub) {
            out << place.name << " -> " << id << '\n';
          }
        }
      }

    private:
      const Graph &_egraph;
      const Pattern &_pattern;

      //std::unordered_map< Place, Id, PlaceHasher > _matched_places;
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

} // namespace circuitous