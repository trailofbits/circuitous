/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once


#include <vector>
#include <string>
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
  struct EClassMatches
  {
    using Id = typename Graph::Id;

    Id eclass;

    std::vector< Substitutions< Graph > > substitutions;
  };


  template< typename Graph >
  struct Rule
  {
    using Pattern = Pattern< Graph >;
    using EClassMatches = EClassMatches< Graph >;
    using Matches = std::vector< EClassMatches >;

    Rule(std::string_view name, std::string_view lhs, std::string_view rhs)
      : name(name), _lhs(Pattern(lhs)), _rhs(Pattern(rhs))
    {}

    struct Matcher
    {
      Matcher(Pattern pattern) : _pattern(pattern) {}

      Matches match(const Graph &egraph) const
      {
        return {};
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
    using EClassMatches = EClassMatches< Graph >;
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