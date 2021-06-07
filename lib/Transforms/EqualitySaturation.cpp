/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/Transforms.h>

#include <circuitous/ADT/EGraph.hpp>

#include <circuitous/Transforms/EqualitySaturation.hpp>

#include <functional>
#include <iostream>
#include <fstream>
#include <memory>
#include <optional>
#include <vector>

namespace circuitous {

  using CircuitEGraph = EGraph< ENode< Operation* > >;

  struct EGraphBuilder : public Visitor< EGraphBuilder >
  {
    using ENode = CircuitEGraph::ENode;
    using Id = CircuitEGraph::Id;

    Id add_node_recurse(Operation *op, CircuitEGraph &egraph)
    {
        ENode node(op);
        for (const auto &child : op->operands) {
          node.children.push_back(add_node_recurse(child, egraph));
        }
        return egraph.add(std::move(node));
    }

    CircuitEGraph build(Circuit *circuit)
    {
      CircuitEGraph egraph;
      circuit->AllAttributes::ForEachOperation([&] (Operation *op) {
        add_node_recurse(op, egraph);
      });
      return egraph;
    }
  };


  // Runner orchestrates a whole equality saturation
  template< typename Graph, template< typename > typename Scheduler >
  struct EqSatRunner
  {
    using Rule = Rule< Graph >;
    using Rules = Rules< Graph >;
    using RulesScheduler = Scheduler< Graph >;
    using Matches = typename Rule::Matches;

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

      using RuleRef = std::reference_wrapper< const Rule >;
      std::vector< std::pair< RuleRef, Matches > > matches;
      for (const auto &rule : rules) {
        matches.emplace_back(rule, _scheduler.match_rule(_egraph, rule));
        // TODO(Heno): check limits
      }

      for (const auto &[rule, match] : matches) {
        _scheduler.apply_rule(_egraph, rule, match);
        // TODO(Heno): check limits
      }

      _egraph.rebuild();

      // TODO(Heno): check graph saturation

      return stop_reason::unknown;
    }

    const Graph& egraph() const { return _egraph; }

  private:
    RulesScheduler _scheduler;
    Graph _egraph;
  };

  using DefaultRunner = EqSatRunner< CircuitEGraph, BasicRulesScheduler >;
  using CircuitRules = Rules< CircuitEGraph >;

  bool EqualitySaturation(Circuit *circuit)
  {
    LOG(INFO) << "Start equality saturation";

    EGraphBuilder ebuilder;
    auto runner = DefaultRunner( ebuilder.build(circuit) );

    auto rules = CircuitRules();
    runner.run(rules);

    LOG(INFO) << "Equality saturation stopped";

    // TODO(Heno) extract best circuit
    std::ofstream out("egraph.dot");
    to_dot(runner.egraph(), out, [] (auto *node) { return to_string(node->term->op_code); });

    return true;
  }

} // namesapce circuitous