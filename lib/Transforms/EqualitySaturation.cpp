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
  struct Matcher
  {
    using EClassMatches = EClassMatches< Graph >;

    std::vector< EClassMatches > match(const Graph &egraph) const
    {
      return {};
    }
  };

  template< typename Graph >
  struct Applier
  {

  };

  template< typename Graph >
  struct Rule
  {
    using Matcher = Matcher< Graph >;
    using Applier = Applier< Graph >;

    using EClassMatches = EClassMatches< Graph >;

    const std::string name;

    // Rewrite rule 'lhs -> rhs' that allows to match
    // left-hand-side and replace it with right-hand-side
    Matcher lhs;
    Applier rhs;

    std::vector< EClassMatches > match(const Graph &egraph) const
    {
      return lhs.match(egraph);
    }
  };

  template< typename Graph >
  using Rules = std::vector< Rule< Graph > >;


  template< typename Graph >
  struct BasicRulesScheduler
  {
    using EClassMatches = EClassMatches< Graph >;
    using Rule = Rule< Graph >;

    std::vector< EClassMatches > match_rule(const Graph &egraph, const Rule &rule) const
    {
      return rule.match(egraph);
    }
  };

  // Runner orchestrates a whole equality saturation
  template< typename Graph, template< typename > typename Scheduler >
  struct EqSatRunner
  {
    using Rules = Rules< Graph >;
    using RulesScheduler = Scheduler< Graph >;
    using EClassMatches = EClassMatches< Graph >;

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

      std::vector< EClassMatches > matches;
      for (const auto &rule : rules) {
        auto matched = _scheduler.match_rule(_egraph, rule);
        matches.insert(matches.end(), matched.begin(), matched.end());
        // TODO(Heno): check limits
      }

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