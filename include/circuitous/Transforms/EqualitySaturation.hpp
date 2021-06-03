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

  // substitution mapping from varibles to equality classes
  template< typename Graph >
  struct Substitutions
  {

  };

  // Result of matched nodes in one eclass
  template< typename Graph >
  struct EClassMatch
  {
    using Id = typename Graph::Id;

    Id eclass;

    std::vector< Substitutions< Graph > > substitutions;
  };


  template< typename Graph >
  struct Rule
  {
    using Pattern = Pattern< Graph >;
    using EClassMatches = EClassMatch< Graph >;
    using Matches = std::vector< EClassMatches >;

    Rule(std::string_view name, std::string_view lhs, std::string_view rhs)
      : name(name), _lhs(Pattern(lhs)), _rhs(Pattern(rhs))
    {}

    struct EGraphMatcher
    {
      using PatternNode = typename Pattern::Value;
      using ENode = typename Graph::ENode;
      using EClass = typename Graph::EClass;
      using Id = typename Graph::Id;

      using constant_t = typename PatternNode::Constant;
      using symbol_t   = typename PatternNode::Symbol;
      using place_t    = typename PatternNode::Place;
      using op_t       = typename PatternNode::Op;
      using list_t     = typename PatternNode::List;

      EGraphMatcher(const Graph &egraph, const Pattern &pattern)
        : _egraph(egraph), _pattern(pattern)
      {}

      Matches match()
      {
        // TODO(Heno): add operation caching to egraph
        Matches matches;
        for (const auto &[id, eclass] : _egraph.classes()) {
          if (match(eclass, _pattern.value)) {
            // TODO(Heno): fill substitutions
            matches.push_back( EClassMatches{id, {}} );
          }
        }
        return matches;
      }

      // helper for patter visitor
      template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
      template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

      static auto get_tail(const list_t &lst)
      {
        auto beg = std::next(lst.begin());
        auto end = lst.end();
        return std::span(&(*beg), &(*end));
      }

      bool match(const EClass &eclass, const PatternNode &pattern)
      {
        for (const auto *node : eclass.nodes) {
          std::cout << node->name() << std::endl;
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
          [&] (const symbol_t &sym) -> bool {
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


    private:
      const Graph &_egraph;
      const Pattern &_pattern;

      struct place_hasher_t
      {
        std::size_t operator()(const place_t &plc) const
        {
          return std::hash<std::string>()(plc.name);
        }
      };

      std::unordered_map< place_t, Id, place_hasher_t > _matched_places;
    };

    struct Matcher
    {
      Matcher(Pattern pattern) : _pattern(pattern) {}

      Matches match(const Graph &egraph) const
      {
        EGraphMatcher matcher(egraph, _pattern);
        return matcher.match();
      }

    private:
      Pattern _pattern;
    };

    struct Applier
    {
      Applier(Pattern pattern) : _pattern(pattern) {}

      void apply_on_matches(const Graph &egraph, const Matches &matches) const
      {

      }

    private:
      Pattern _pattern;
    };

    Matches match(const Graph &egraph) const
    {
      return _lhs.match(egraph);
    }

    void apply(const Graph &egraph, const Matches &matches) const
    {
      _rhs.apply_on_matches(egraph, matches);
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

    void apply_rule(const Graph &egraph, const Rule &rule, const Matches &matches) const
    {
      rule.apply(egraph, matches);
    }
  };

} // namespace circuitous