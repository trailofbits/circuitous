/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <vector>
#include <string>

namespace circuitous {

  template < typename Graph >
  struct Pattern
  {

  };

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
    using EClassMatches = EClassMatches< Graph >;
    using Matches = std::vector< EClassMatches >;

    struct Matcher
    {
      Matches match(const Graph &egraph) const
      {
        return {};
      }
    };

    struct Applier
    {
      void apply_on_matches(const Graph &egraph, const Matches &matches) const
      {

      }
    };

    const std::string name;

    // Rewrite rule 'lhs -> rhs' that allows to match
    // left-hand-side and replace it with right-hand-side
    Matcher lhs;
    Applier rhs;

    Matches match(const Graph &egraph) const
    {
      return lhs.match(egraph);
    }

    void apply(const Graph &egraph, const Matches &matches) const
    {
      rhs.apply_on_matches(egraph, matches);
    }
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