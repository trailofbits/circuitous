/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/Transforms.h>
#include <llvm/ADT/StringRef.h>

#include <circuitous/ADT/EGraph.hpp>

#include <circuitous/Transforms/EqualitySaturation.hpp>

#include <functional>
#include <iostream>
#include <fstream>
#include <memory>
#include <optional>
#include <vector>
#include <span>

namespace circ {
namespace eqsat {

  struct EGraphBuilder : public Visitor< EGraphBuilder >
  {
    using ENode = CircuitEGraph::ENode;
    using Id = CircuitEGraph::Id;

    Id add_node_recurse(Operation *op, CircuitEGraph &egraph)
    {
        ENode node( OperationTemplate{op->op_code} );
        for (const auto &child : op->operands) {
          node.children.push_back(add_node_recurse(child, egraph));
        }
        auto [id, _] = egraph.add(std::move(node));
        return id;
    }

    CircuitEGraph build(Circuit *circuit)
    {
      CircuitEGraph egraph;
      add_node_recurse(circuit, egraph);
      return egraph;
    }
  };

  template< typename Graph >
  struct PatternCircuitBuilder
  {
    using Id       = typename Graph::Id;

    PatternCircuitBuilder(Circuit *circuit_) : circuit(circuit_) {}

    Operation* constant(const Constant &con) const
    {
      return nullptr;
    }

    Operation* operation(const eqsat::operation &op, std::span< Operation * > args) const
    {
      auto res = [&] () -> Operation* {
        LOG(FATAL) << "unsupported operation";
      } ();

      for (auto arg : args)
        res->AddUse(arg);

      return res;
    }

    Id synthesize(const expr &e, const auto &subs, const auto &places, const auto &subexprs) const
    {
      CHECK(subs.size() == places.size());

      // TODO(Heno): addnodes to egraph
      return Id(0);
    }

    Circuit *circuit;
  };


  // Runner orchestrates a whole equality saturation
  template< typename Graph, template< typename > typename Scheduler >
  struct EqSatRunner
  {
    using Rule = Rule< Graph >;
    using Rules = Rules< Graph >;
    using RulesScheduler = Scheduler< Graph >;
    using PatternCircuitBuilder = PatternCircuitBuilder< Graph >;

    EqSatRunner(Circuit *circuit)
      : _egraph(EGraphBuilder().build(circuit))
      , _builder(circuit)
      , _scheduler(_builder)
    {}

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
    Graph _egraph;
    PatternCircuitBuilder _builder;
    RulesScheduler _scheduler;
  };

  template< typename Graph >
  using Scheduler = BasicRulesScheduler< Graph, PatternCircuitBuilder< Graph > >;
  using DefaultRunner = EqSatRunner< CircuitEGraph, Scheduler >;
  using CircuitRewriteRules = Rules< CircuitEGraph >;

} // namespace eqsat

  bool EqualitySaturation(Circuit *circuit)
  {
    using GraphBuilder = eqsat::EGraphBuilder;
    using Runner = eqsat::DefaultRunner;
    using RewriteRules = eqsat::CircuitRewriteRules;

    LOG(INFO) << "Start equality saturation";

    auto runner = Runner(circuit);

    RewriteRules rules;
    runner.run(rules);

    LOG(INFO) << "Equality saturation stopped";

    // TODO(Heno) extract best circuit
    std::ofstream out("egraph.dot");
    to_dot(runner.egraph(), out, [] (auto *node) {
      auto str = to_string(node->term.op_code);
      return llvm::StringRef(str).split(':').second.str();
    });

    return true;
  }

} // namesapce circuitous