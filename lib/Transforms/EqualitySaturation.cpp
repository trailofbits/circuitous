/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/Transforms.h>

#include <circuitous/ADT/EGraph.hpp>
#include <functional>
#include <iostream>

namespace circuitous {

  struct CircuitNode {};
  using CircuitEGraph = EGraph< CircuitNode >;

  CircuitEGraph make_circuit_egraph(Circuit *circuit)
  {
    return {};
  }

  template< typename Graph >
  struct Matcher
  {

  };

  template< typename Graph >
  struct Applier
  {

  };

  template< typename Graph >
  struct Rule
  {
    using RMatcher = Matcher< Graph >;
    using RApplier = Applier< Graph >;

    const std::string name;

    // Rewrite rule 'lhs -> rhs' that allows to match
    // left-hand-side and replace it with right-hand-side
    RMatcher lhs;
    RApplier rhs;
  };

  template< typename Graph >
  using Rules = std::vector< Rule< Graph > >;

  // Runner orchestrates a whole equality saturation
  template< typename Graph >
  struct EqSatRunner
  {
    using GRules = Rules< Graph >;

    EqSatRunner(Graph &&egraph) : _egraph(std::move(egraph)) {}

    // return value of equality saturation
    enum class stop_reason
    {
      saturated, iteration_limit, node_limit, time_limit, unknown
    };

    // Run equality saturation with given rewrite rules until it stops,
    // i.e., hits any of limits or fully saturates graph
    stop_reason run(const GRules &rules)
    {
      while (!is_saturated_ot_timeout()) {

      }

      return stop_reason::unknown;
    }

    [[nodiscard]] bool is_saturated_ot_timeout() const
    {
      return true;
    }

  private:
    Graph _egraph;
  };


  using DefaultRunner = EqSatRunner< CircuitEGraph >;
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