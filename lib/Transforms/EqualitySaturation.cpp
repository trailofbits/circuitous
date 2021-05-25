/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/Transforms.h>

#include <circuitous/ADT/EGraph.hpp>
#include <functional>
#include <iostream>
#include <optional>
#include <vector>

namespace circuitous {

  struct CircuitNode {};
  using CircuitEGraph = EGraph< CircuitNode >;

  CircuitEGraph make_circuit_egraph(Circuit *circuit)
  {
    return {};
  }

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

  // Runner orchestrates a whole equality saturation
  template< typename Graph, template< typename > typename Scheduler >
  struct EqSatRunner
  {
    using Rule = Rule< Graph >;
    using Rules = Rules< Graph >;
    using RulesScheduler = Scheduler< Graph >;
    using EClassMatches = EClassMatches< Graph >;
    using Matches = std::vector< EClassMatches >;

    EqSatRunner(Graph &&egraph) : _egraph(std::move(egraph)) {}

    // return value of equality saturation
    enum class stop_reason
    {
      saturated, iteration_limit, node_limit, time_limit, unknown
    };

    using Status = std::optional< stop_reason >;

    // Run equality saturation with given rewrite rules until it stops,
    // i.e., hits any of limits or fully saturates graph
    stop_reason run(const Rules &rules)
    {
      _egraph.rebuild();

      Status stopped = std::nullopt;
      while (!stopped.has_value()) {
        stopped = step(rules);
      }

      return stopped.value();
    }

    // One iteration of the saturation loop
    Status step(const Rules &rules)
    {
      // TODO(Heno): check limits & timeout

      std::vector< std::pair< Rule, Matches > > matches;
      for (const auto &rule : rules) {
        matches.emplace_back(rule, _scheduler.match_rule(_egraph, rule));
        // TODO(Heno): check limits
      }

      for (const auto &[rule, match] : matches) {
        _scheduler.apply_rule(_egraph, rule, match);
        // TODO(Heno): check limits
      }

      _egraph.rebuild();

      // TODO(Heno): chack graph saturation

      return stop_reason::unknown;
    }

  private:
    RulesScheduler _scheduler;
    Graph _egraph;
  };

  using DefaultRunner = EqSatRunner< CircuitEGraph, BasicRulesScheduler >;
  using CircuitRules = Rules< CircuitEGraph >;

  bool EqualitySaturation(Circuit *circuit)
  {
    LOG(INFO) << "Start equality saturation";

    auto runner = DefaultRunner(make_circuit_egraph(circuit));

    auto rules = CircuitRules();
    runner.run(rules);

    LOG(INFO) << "Equality saturation stopped with";

    // extract best circuit

    return true;
  }

} // namesapce circuitous