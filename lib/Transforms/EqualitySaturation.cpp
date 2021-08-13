/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/Transforms.h>
#include <llvm/ADT/StringRef.h>

#include <circuitous/ADT/EGraph.hpp>

#include <circuitous/Transforms/Pattern.hpp>
#include <circuitous/Transforms/EqualitySaturation.hpp>

#include <functional>
#include <iostream>
#include <fstream>
#include <memory>
#include <optional>
#include <string>
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
    using ENode = typename Graph::ENode;
    using Id    = typename Graph::Id;

    PatternCircuitBuilder(Graph &graph) : graph(graph) {}

    Id make_constant(std::string_view con) const
    {
      LOG(FATAL) << "not implemented";
    }

    unsigned to_op_code(std::string_view name) const
    {
      LOG(FATAL) << "unknown op code" << name;
    }

    Id synthesize(const expr &e, const auto &subs, const auto &places, const auto &subexprs) const
    {
      CHECK(subs.size() == places.size());
      auto synth = [&] (const auto &sub) { return synthesize(sub, subs, places, subexprs); };

      return std::visit( overloaded {
        [&] (const constant &con) -> Id { return make_constant(std::to_string(con.ref())); },
        [&] (const place &plc)    -> Id { return subs.id(places.at(plc)); },
        [&] (const label &lab)    -> Id { return synth(subexprs.at(lab)); },
        [&] (const operation &op) -> Id {
          ENode node(to_op_code(op.ref()));
          for (const auto &child : children(e))
            node.children.push_back(synth(child));

          return graph.add(std::move(node)).first;
        },
        [&] (const auto&)         -> Id { LOG(FATAL) << "unsupported node"; },
      }, root(e));
    }

  private:
    Graph &graph;
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
      , _builder(_egraph)
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