/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once


#include <cstddef>
#include <functional>
#include <map>
#include <unordered_map>
#include <vector>
#include <string>
#include <span>
#include <string_view>

#include <circuitous/Transforms/Pattern.hpp>

namespace circuitous {

  template< typename Place >
  struct place_hasher_t
  {
    std::size_t operator()(const Place &plc) const
    {
      return std::hash<std::string>()(plc.name);
    }
  };

  // substitution mapping from places (variables) to equality classes
  template< typename Graph >
  struct Substitutions
  {
    using Pattern = Pattern< Graph >;
    using PatternNode = typename Pattern::Value;
    using place_t     = typename PatternNode::Place;

    using Id = typename Graph::Id;

    using hasher = place_hasher_t< place_t >;
    std::unordered_map< place_t, Id, hasher > mapping;
  };

  // Result of matched nodes rooting in the given equality class
  template< typename Graph >
  struct EClassMatch
  {
    using Id = typename Graph::Id;
    using Substitutions = Substitutions< Graph >;

    Id eclass;
    std::vector< Substitutions > substitutions;
  };


  template< typename Graph >
  struct Rule
  {
    using Pattern = Pattern< Graph >;
    using EClassMatch = EClassMatch< Graph >;
    using Matches = std::vector< EClassMatch >;

    Rule(std::string_view name, std::string_view lhs, std::string_view rhs)
      : name(name), _lhs(Pattern(lhs)), _rhs(Pattern(rhs))
    {}

    struct EGraphMatcher
    {
      using PatternNode = typename Pattern::Value;
      using ENode = typename Graph::ENode;
      using EClass = typename Graph::EClass;
      using Substitutions = Substitutions< Graph >;
      using Id = typename Graph::Id;

      using constant_t = typename PatternNode::Constant;
      using place_t    = typename PatternNode::Place;
      using op_t       = typename PatternNode::Op;
      using list_t     = typename PatternNode::List;

      EGraphMatcher(const Graph &egraph, const Pattern &pattern)
        : _egraph(egraph), _pattern(pattern)
      {}

      // helper for patter visitor
      template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
      template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

      static auto get_tail(const list_t &lst)
      {
        auto beg = std::next(lst.begin());
        auto end = lst.end();
        return std::span(&(*beg), &(*end));
      }

      bool match(const EClass &eclass) { return match(eclass, _pattern.value); }

      bool match(const EClass &eclass, const PatternNode &pattern)
      {
        for (const auto *node : eclass.nodes) {
          if (!match(node, pattern))
            return false;
        }
        return true;
      }

      bool match(const ENode *root, const PatternNode &pattern)
      {
        return std::visit( overloaded {
          [&] (const constant_t &con) -> bool {
            if (auto node_con = root->constant())
              return node_con == con;
            return false;
          },
          [&] (const place_t &plc) -> bool {
            auto id = _egraph.find(root);
            if ( auto [it, inserted] = _matched_places.try_emplace(plc, id); !inserted ) {
              return it->second == id;
            }
            return true;
          },
          [&] (const op_t &op) -> bool {
            return root->name() == op.name;
          },
          [&] (const list_t &lst) -> bool {
            // match operation node
            const auto &head = lst.front();
            if (!match(root, head))
              return false;

            // match operation arguments
            auto tail = get_tail(lst);
            if (root->children.size() != tail.size())
              return false;

            auto child = root->children.begin();
            for (const auto &node : tail) {
              auto child_class = _egraph.eclass(*child);
              if (!match(child_class, node))
                return false;
              ++child;
            }

            return true;
          },
          [&] (const auto&) -> bool { return false; /* unsupported kind */ },
        }, pattern);
      }

      std::vector< Substitutions > substitutions() const { return _substs; }
    private:
      const Graph &_egraph;
      const Pattern &_pattern;

      std::vector< Substitutions > _substs;

      using hasher = place_hasher_t< place_t >;
      std::unordered_map< place_t, Id, hasher > _matched_places;
    };

    struct Matcher
    {
      Matcher(Pattern pattern) : _pattern(pattern) {}

      Matches match(const Graph &egraph) const
      {
        // TODO(Heno): add operation caching to egraph

        Matches matches;
        for (const auto &[id, eclass] : egraph.classes()) {
          EGraphMatcher matcher(egraph, _pattern);

          if (matcher.match(eclass))
            matches.push_back( EClassMatch{id, matcher.substitutions()} );
        }
        return matches;
      }

    private:
      Pattern _pattern;
    };

    struct Applier
    {
      Applier(Pattern pattern) : _pattern(pattern) {}

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
    using EClassMatches = EClassMatch< Graph >;
    using Matches = std::vector< EClassMatches >;
    using Rule = Rule< Graph >;

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