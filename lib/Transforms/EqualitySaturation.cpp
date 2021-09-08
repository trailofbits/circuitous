/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/Transforms.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/StringSwitch.h>

#include <algorithm>
#include <circuitous/ADT/EGraph.hpp>

#include <circuitous/Transforms/Pattern.hpp>
#include <circuitous/Transforms/EqualitySaturation.hpp>

#include <cstddef>
#include <cstdint>
#include <functional>
#include <iostream>
#include <fstream>
#include <memory>
#include <numeric>
#include <optional>
#include <string>
#include <vector>
#include <queue>
#include <span>

namespace circ::eqsat {

  struct OpTemplateBuilder : NonRecursiveVisitor< OpTemplateBuilder >
  {
    using Base = Visitor< OpTemplateBuilder >;

    OpCode    opcode(auto *op)      { return {op->op_code_str()}; }
    SizedOp   sized(auto *op)       { return {op->op_code_str(), op->size}; }
    RegOp     regop(auto *op)       { return {op->op_code_str(), op->size, op->reg_name}; }
    ConstOp   constop(Constant *op) { return {op->op_code_str(), op->size, op->bits}; }
    MemOp     memop(Memory *op)     { return {op->op_code_str(), op->mem_idx}; }
    ExtractOp extract(Extract *op)  { return {op->op_code_str(), op->low_bit_inc, op->high_bit_exc}; }
    SelectOp  select(Select *op)    { return {op->op_code_str(), op->size, op->bits}; }

    OpTemplate Visit(InputRegister *op)  { return regop(op); }
    OpTemplate Visit(OutputRegister *op) { return regop(op); }

    OpTemplate Visit(InputTimestamp *op)  { return sized(op); }
    OpTemplate Visit(OutputTimestamp *op) { return sized(op); }
    OpTemplate Visit(InputErrorFlag *op)  { return sized(op); }
    OpTemplate Visit(OutputErrorFlag *op) { return sized(op); }

    OpTemplate Visit(Undefined *op) { return sized(op); }

    OpTemplate Visit(Memory *op)   { return memop(op); }

    OpTemplate Visit(Constant *op) { return constop(op); }

    OpTemplate Visit(Advice *op) { return sized(op); }

    OpTemplate Visit(InputInstructionBits *op) { return sized(op); }

    OpTemplate Visit(RegConstraint *op)       { return opcode(op); }
    OpTemplate Visit(AdviceConstraint *op)    { return opcode(op); }
    OpTemplate Visit(PreservedConstraint *op) { return opcode(op); }
    OpTemplate Visit(CopyConstraint *op)      { return opcode(op); }
    OpTemplate Visit(WriteConstraint *op)     { return opcode(op); }
    OpTemplate Visit(ReadConstraint *op)      { return opcode(op); }
    OpTemplate Visit(UnusedConstraint *op)    { return opcode(op); }

    OpTemplate Visit(Add *op) { return sized(op); }
    OpTemplate Visit(Sub *op) { return sized(op); }
    OpTemplate Visit(Mul *op) { return sized(op); }

    OpTemplate Visit(UDiv *op) { return sized(op); }
    OpTemplate Visit(SDiv *op) { return sized(op); }

    OpTemplate Visit(Shl *op)  { return sized(op); }
    OpTemplate Visit(LShr *op) { return sized(op); }
    OpTemplate Visit(AShr *op) { return sized(op); }

    OpTemplate Visit(Trunc *op) { return sized(op); }
    OpTemplate Visit(ZExt *op)  { return sized(op); }
    OpTemplate Visit(SExt *op)  { return sized(op); }

    OpTemplate Visit(Icmp_ult *op)  { return sized(op); }
    OpTemplate Visit(Icmp_slt *op)  { return sized(op); }
    OpTemplate Visit(Icmp_ugt *op)  { return sized(op); }
    OpTemplate Visit(Icmp_eq *op)   { return sized(op); }
    OpTemplate Visit(Icmp_ne *op)   { return sized(op); }
    OpTemplate Visit(Icmp_uge *op)  { return sized(op); }
    OpTemplate Visit(Icmp_ule *op)  { return sized(op); }
    OpTemplate Visit(Icmp_sgt *op)  { return sized(op); }
    OpTemplate Visit(Icmp_sge *op)  { return sized(op); }
    OpTemplate Visit(Icmp_sle *op)  { return sized(op); }

    OpTemplate Visit(BSelect *op)   { return sized(op); }

    OpTemplate Visit(CAnd *op) { return sized(op); }
    OpTemplate Visit(COr *op)  { return sized(op); }
    OpTemplate Visit(CXor *op) { return sized(op); }

    OpTemplate Visit(InputImmediate *op) { return sized(op); }

    OpTemplate Visit(Extract *op) { return extract(op); }

    OpTemplate Visit(Concat *op) { return sized(op); }

    OpTemplate Visit(PopulationCount *op)     { return sized(op); }
    OpTemplate Visit(CountLeadingZeroes *op)  { return sized(op); }
    OpTemplate Visit(CountTrailingZeroes *op) { return sized(op); }

    OpTemplate Visit(Not *op) { return sized(op); }

    OpTemplate Visit(Parity *op) { return opcode(op); }

    OpTemplate Visit(Select *op) { return select(op); }

    OpTemplate Visit(DecodeCondition *op)   { return opcode(op); }
    OpTemplate Visit(VerifyInstruction *op) { return opcode(op); }
    OpTemplate Visit(OnlyOneCondition *op)  { return opcode(op); }

    OpTemplate Visit(Or *op)  { return opcode(op); }
    OpTemplate Visit(And *op) { return opcode(op); }
  };

  struct EGraphBuilder
  {
    using ENode = CircuitEGraph::ENode;
    using Id = CircuitEGraph::Id;

    using Nodes = std::map< Operation *, ENode * >;

    Id add_node_recurse(auto *op, CircuitEGraph &egraph)
    {
      if (nodes.count(op))
        return egraph.find(nodes[op]);
      ENode node(make_template(op));
      for (const auto &child : op->operands)
        node.children.push_back(add_node_recurse(child, egraph));
      auto [id, enode] = egraph.add(std::move(node));
      nodes[op] = enode;
      return id;
    }

    std::pair< CircuitEGraph, Nodes > build(Circuit *circuit)
    {
      CircuitEGraph egraph;
      add_node_recurse(circuit, egraph);
      return {std::move(egraph), nodes};
    }

    OpTemplate make_template(Circuit *ci)
    {
      return template_builder.opcode(ci);
    }

    OpTemplate make_template(Operation *op)
    {
      return template_builder.Dispatch(op);
    }

    Nodes nodes;
    OpTemplateBuilder template_builder;
  };

  template< typename Graph >
  struct PatternCircuitBuilder
  {
    using ENode = typename Graph::ENode;
    using Id    = typename Graph::Id;

    PatternCircuitBuilder(Graph &graph) : graph(graph) {}

    OpTemplate make_constant_template(const constant &con) const
    {
      LOG(FATAL) << "not implemented";
    }

    OpTemplate make_operation_template(const operation &op) const
    {
      auto name = op.ref();

      auto sized = [] (auto name) { return SizedOp{std::string(name), {}}; };
      auto opcode = [] (auto name) { return OpCode{std::string(name)}; };

      auto value = llvm::StringSwitch< std::optional< OpTemplate > >(name)
        // .Case("circuit")
        // .Case("in.register")
        // .Case("out.register")
        // .Case("in.timestamp")
        // .Case("out.timestamp")
        // .Case("in.error_flag")
        // .Case("out.error_flag")
        .Case("undefined", sized(name))
        // .Case("memory")
        .Case("advice", sized(name))
        // .Case("instruction_bits")
        .Case("register_constraint",  opcode(name))
        .Case("advice_constraint",    opcode(name))
        .Case("preserved_constraint", opcode(name))
        .Case("copy_constraint",      opcode(name))
        .Case("write_constraint",     opcode(name))
        .Case("read_constraint",      opcode(name))
        .Case("unused_constraint",    opcode(name))

        .Case("Add",   sized(name))
        .Case("+",     sized("Add"))
        .Case("Sub",   sized(name))
        .Case("-",     sized("Sub"))
        .Case("Mul",   sized(name))
        .Case("*",     sized("Mul"))
        .Case("UDiv",  sized(name))
        .Case("/",     sized("UDiv"))
        .Case("SDiv",  sized(name))
        .Case("Shl",   sized(name))
        .Case("<<",    sized("Shl"))
        .Case("LShr",  sized(name))
        .Case(">>",    sized("LShr"))
        .Case("AShr",  sized(name))
        .Case("Trunc", sized(name))
        .Case("ZExt",  sized(name))
        .Case("SExt",  sized(name))

        .Case("Icmp_ult", sized(name))
        .Case("<",        sized("Icmp_ult"))
        .Case("Icmp_slt", sized(name))
        .Case("Icmp_ugt", sized(name))
        .Case(">",        sized("Icmp_ugt"))
        .Case("Icmp_eq",  sized(name))
        .Case("==",       sized("Icmp_eq"))
        .Case("Icmp_ne",  sized(name))
        .Case("!=",       sized("Icmp_ne"))
        .Case("Icmp_uge", sized(name))
        .Case(">=",       sized("Icmp_uge"))
        .Case("Icmp_ule", sized(name))
        .Case("<=",       sized("Icmp_ule"))
        .Case("Icmp_sgt", sized(name))
        .Case("Icmp_sge", sized(name))
        .Case("Icmp_sle", sized(name))

        .Case("BSelect",  sized(name))

        .Case("CAnd", sized(name))
        .Case("COr",  sized(name))
        .Case("CXor", sized(name))

        .Case("input_immediate", opcode(name))
        // .Case("extract")

        .Case("concat", sized(name))

        .Case("pop_count",             sized(name))
        .Case("count_lead_zeroes",     sized(name))
        .Case("count_trailing_zeroes", sized(name))
        .Case("parity",                sized(name))
        .Case("not",                   sized(name))

        // .Case(Select::op_code_str())

        .Case("DecodeCondition",   opcode(name))
        .Case("VerifyInstruction", opcode(name))
        .Case("OnlyOneCondition",  opcode(name))
        .Case("Or",  opcode(name))
        .Case("And", opcode(name))

        .Default( std::nullopt );

        if ( !value )
          LOG(FATAL) << "unhadled operation " << name;
        return value.value();
    }

    Id make_constant(const constant &con) const
    {
      return graph.add(ENode(make_constant_template(con))).first;
    }

    Id synthesize(const expr &e, const auto &subs, const auto &places, const auto &subexprs) const
    {
      CHECK(subs.size() == places.size());
      auto synth = [&] (const auto &sub) { return synthesize(sub, subs, places, subexprs); };

      return std::visit( overloaded {
        [&] (const constant &con) -> Id { return make_constant(con); },
        [&] (const place &plc)    -> Id { return subs.id(places.at(plc)); },
        [&] (const label &lab)    -> Id { return synth(subexprs.at(lab)); },
        [&] (const operation &op) -> Id {
          ENode node(make_operation_template(op));
          for (const auto &child : children(e))
            node.children.push_back( synth(child) );
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

    using EGraph = Graph;
    using ENode  = typename Graph::ENode;
    using Nodes  = std::map< Operation *, ENode * >;

    static EqSatRunner make_runner(const CircuitPtr &circuit)
    {
      EGraphBuilder builder;
      auto [graph, nodes] = builder.build(circuit.get());
      return {std::move(graph), std::move(nodes)};
    }

    EqSatRunner(Graph &&graph, Nodes &&nodes)
      : _egraph(std::move(graph))
      , _nodes(std::move(nodes))
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

    ENode* node(Operation *op) { return _nodes[op]; }

  private:
    Graph _egraph;
    Nodes _nodes;
    PatternCircuitBuilder _builder;
    RulesScheduler _scheduler;
  };

  template< typename Graph >
  using Scheduler = BasicRulesScheduler< Graph, PatternCircuitBuilder< Graph > >;
  using DefaultRunner = EqSatRunner< CircuitEGraph, Scheduler >;
  using CircuitRewriteRules = Rules< CircuitEGraph >;

  // CostGraph precomputes cost for all equality nodes
  // in the underlying egraph according to a given cost function
  // CostFunction :: EnodePtr -> std::uintptr_t
  //
  // For all non-leaf nodes, the cost is equal to the sum of costs of its
  // children and the cost of a given node. Evaluator takes the minimal cost
  // from nodes children equality class.
  template< typename Graph, typename CostFunction >
  struct CostGraph
  {
    using ENode = typename Graph::ENode;
    using EClass = typename Graph::EClass;
    using CostMap = std::map< const ENode*, std::uint64_t >;

    CostGraph(const Graph &graph, CostFunction eval)
      : graph(graph), eval(eval)
    {
      for (const auto &[node, id] : graph.nodes())
        costs.try_emplace(node, eval_cost(node));
    }

    // OptimalGraphView serves as a view on equality graph where for each equality
    // class is picked a single node with the lowest cost. GraphView provides
    // helper functions that return an optimal substitution for a given node or a
    // list of optimal children.
    struct OptimalGraphView
    {
      using EClassPtr = const EClass *;
      using ENodePtr  = const ENode *;

      OptimalGraphView(const CostGraph &costgraph)
        : graph(costgraph.graph)
      {
        for (const auto &[id, eclass] : graph.classes())
          optimal.emplace( &eclass, costgraph.minimal(&eclass) );
      }

      // Returns an optimal substitution for a given enode.
      ENodePtr node(ENodePtr enode) const
      {
        return optimal.at( &graph.eclass(enode) );
      }

      // Returns an optimal set of children for a given enode.
      std::vector< ENodePtr > children(ENodePtr enode) const
      {
        std::vector< ENodePtr > res;
        for (const auto &child : enode->children)
          res.push_back( optimal.at( &graph.eclass(child) ) );
        return res;
      }

    private:
      const Graph &graph;
      std::map< EClassPtr, ENodePtr > optimal;
    };

    OptimalGraphView optimal() const { return {*this}; }

    const ENode* minimal(const EClass *eclass) const
    {
      const auto &nodes = eclass->nodes;
      auto cmp = [&] (const auto &a, const auto &b) {
        return costs.at(a) < costs.at(b);
      };
      return *std::min_element(nodes.begin(), nodes.end(), cmp);
    }

  private:

    std::uint64_t eval_cost(const ENode *node)
    {
      if (auto cost = costs.find(node); cost != costs.end())
        return cost->second;

      auto fold = [&] (unsigned cost, auto id) -> std::uint64_t {
        const auto &eclass = graph.eclass(id);
        for (const auto *node : eclass.nodes)
          costs.try_emplace(node, eval_cost(node));

        return cost + costs[minimal(&eclass)];
      };

      const auto &children = node->children;
      return std::accumulate(children.begin(), children.end(), eval(node), fold);
    }

    const Graph &graph;

    CostFunction eval;
    CostMap costs;
  };

  namespace detail {

    Operation* make_operation(const OpCode &op, Circuit *circuit)
    {
      return llvm::StringSwitch< Operation* >(op.op_code_name)
        .Case("register_constraint",  circuit->Create< RegConstraint >())
        .Case("advice_constraint",    circuit->Create< AdviceConstraint >())
        .Case("preserved_constraint", circuit->Create< PreservedConstraint >())
        .Case("copy_constraint",      circuit->Create< CopyConstraint >())
        .Case("write_constraint",     circuit->Create< WriteConstraint >())
        .Case("read_constraint",      circuit->Create< ReadConstraint >())
        .Case("unused_constraint",    circuit->Create< UnusedConstraint >())

        .Case("parity",                circuit->Create< Parity >())

        .Case("DecodeCondition",      circuit->Create< DecodeCondition >())
        .Case("VerifyInstruction",    circuit->Create< VerifyInstruction >())
        .Case("OnlyOneCondition",     circuit->Create< OnlyOneCondition >())

        .Case("Or",  circuit->Create< Or >())
        .Case("And", circuit->Create< And >())

        .Default( nullptr );

    }

    Operation* make_operation(const SizedOp &op, Circuit *circuit)
    {
      CHECK(op.size.has_value());
      auto size = op.size.value();
      return llvm::StringSwitch< Operation* >(op.op_code_name)
        .Case("in.timestamp",     circuit->Create< InputTimestamp >( size ))
        .Case("out.timestamp",    circuit->Create< OutputTimestamp >( size ))
        .Case("in.error_flag",    circuit->Create< InputErrorFlag >( size ))
        .Case("out.error_flag",   circuit->Create< OutputErrorFlag >( size ))
        .Case("undefined",        circuit->Create< Undefined >( size ))
        .Case("advice",           circuit->Create< Advice >( size ))
        .Case("instruction_bits", circuit->Create< InputInstructionBits >( size ))

        .Case("Add",   circuit->Create< Add >( size ))
        .Case("Sub",   circuit->Create< Sub >( size ))
        .Case("Mul",   circuit->Create< Mul >( size ))
        .Case("UDiv",  circuit->Create< UDiv >( size ))
        .Case("SDiv",  circuit->Create< SDiv >( size ))
        .Case("Shl",   circuit->Create< Shl >( size ))
        .Case("LShr",  circuit->Create< LShr >( size ))
        .Case("AShr",  circuit->Create< AShr >( size ))
        .Case("Trunc", circuit->Create< Trunc >( size ))
        .Case("ZExt",  circuit->Create< ZExt >( size ))
        .Case("SExt",  circuit->Create< SExt >( size ))

        .Case("Icmp_ult", circuit->Create< Icmp_ult >( size ))
        .Case("Icmp_slt", circuit->Create< Icmp_slt >( size ))
        .Case("Icmp_ugt", circuit->Create< Icmp_ugt >( size ))
        .Case("Icmp_eq",  circuit->Create< Icmp_eq >( size ))
        .Case("Icmp_ne",  circuit->Create< Icmp_ne >( size ))
        .Case("Icmp_uge", circuit->Create< Icmp_uge >( size ))
        .Case("Icmp_ule", circuit->Create< Icmp_ule >( size ))
        .Case("Icmp_sgt", circuit->Create< Icmp_sgt >( size ))
        .Case("Icmp_sge", circuit->Create< Icmp_sge >( size ))
        .Case("Icmp_sle", circuit->Create< Icmp_sle >( size ))

        .Case("BSelect",  circuit->Create< BSelect >( size ))

        .Case("CAnd", circuit->Create< CAnd >( size ))
        .Case("COr",  circuit->Create< COr >( size ))
        .Case("CXor", circuit->Create< CXor >( size ))

        .Case("input_immediate", circuit->Create< InputImmediate >( size ))

        .Case("concat", circuit->Create< Concat >( size ))

        .Case("pop_count",             circuit->Create< PopulationCount >( size ))
        .Case("count_lead_zeroes",     circuit->Create< CountLeadingZeroes >( size ))
        .Case("count_trailing_zeroes", circuit->Create< CountTrailingZeroes >( size ))
        .Case("not",                   circuit->Create< Not >( size ))

        .Default( nullptr );
    }

    Operation* make_operation(const RegOp &op, Circuit *circuit)
    {
      return llvm::StringSwitch< Operation* >(op.op_code_name)
        .Case("in.register",  circuit->Create< InputRegister >( op.reg_name, op.size ))
        .Case("out.register", circuit->Create< OutputRegister >( op.reg_name, op.size ))
        .Default( nullptr );
    }

    Operation* make_operation(const ConstOp &op, Circuit *circuit)
    {
      return circuit->Create< Constant >( op.bits, op.size );
    }

    Operation* make_operation(const MemOp &op, Circuit *circuit)
    {
      return circuit->Create< Memory >( op.mem_idx );
    }

    Operation* make_operation(const ExtractOp &op, Circuit *circuit)
    {
      return circuit->Create< Extract >( op.low_bit_inc, op.high_bit_exc );
    }

    Operation* make_operation(const SelectOp &op, Circuit *circuit)
    {
      return circuit->Create< Select >( op.bits, op.size );
    }

    Operation* make_operation(const OpTemplate &op, Circuit *circuit)
    {
      auto value = std::visit( [circuit] (const auto &op) {
        return make_operation(op, circuit);
      }, op);

      if ( !value )
        LOG(FATAL) << "unhadled opcode " << to_string(op);
      return value;
    }
  } // namespace detail

  // Lowers equality graph back to circuit using a optimal view graph.
  template< typename OptimalGraphView >
  struct CircuitExtractor
  {
    using ENodePtr = typename OptimalGraphView::ENodePtr;

    CircuitExtractor(const OptimalGraphView &graph) : graph(graph) {}

    std::unique_ptr<circ::Circuit> run(ENodePtr root, uint32_t ptr_size)
    {
      auto circuit = std::make_unique<Circuit>(ptr_size);

      auto node = graph.node(root);
      CHECK(name(node) == "circuit");

      for (const auto &child : graph.children(node))
        circuit->AddUse( extract(child, circuit.get()) );
      return circuit;
    }

  private:

    Operation *make_operation(ENodePtr enode, Circuit *circuit) const
    {
      return detail::make_operation(enode->term, circuit);
    }

    Operation *extract(ENodePtr enode, Circuit *circuit)
    {
      if (cached.count(enode))
        return cached.at(enode);

      auto op = make_operation(enode, circuit);
      cached.emplace(enode, op);

      for (const auto &child : graph.children(enode))
        op->AddUse( extract(child, circuit) );

      return op;
    }

    std::map< ENodePtr, Operation * > cached;
    const OptimalGraphView &graph;
  };

  CircuitPtr EqualitySaturation(const CircuitPtr &circuit)
  {
    using Runner = eqsat::DefaultRunner;
    using RewriteRules = eqsat::CircuitRewriteRules;
    using EGraph = typename Runner::EGraph;

    LOG(INFO) << "Start equality saturation";

    auto runner = Runner::make_runner(circuit);

    RewriteRules rules;
    rules.emplace_back( "binary-unification",
      "((let A (?opa ?xa ?ya)) (let B (?opb ?xb ?yb)) (equiv ?opa ?opb) (equiv ?xa ?xb) (equiv ?ya ?yb) (match $A $B))",
      "(union $A $B)"
    );
    rules.emplace_back( "unary-unification",
      "((let A (?opa ?xa)) (let B (?opb ?xb)) (equiv ?opa ?opb) (equiv ?xa ?xb) (match $A $B))",
      "(union $A $B)"
    );

    std::ofstream outb("egraph-before.dot");
    to_dot(runner.egraph(), outb, [] (auto *node) {
      return to_string(node->term);
    });

    runner.run(rules);

    LOG(INFO) << "Equality saturation stopped";

    std::ofstream outa("egraph-after.dot");
    to_dot(runner.egraph(), outa, [] (auto *node) {
      return to_string(node->term);
    });

    auto eval = [] (const auto &node) -> std::uint64_t {
      // TODO(Heno) implement cost function
      auto name = to_string(node->term);
      if (name == "Mul")
        return 100;
      if (name == "Add")
        return 10;
      return 1;
    };

    auto costgraph = eqsat::CostGraph(runner.egraph(), eval);
    auto optimal = costgraph.optimal();

    eqsat::CircuitExtractor extractor(optimal);
    return extractor.run(runner.node(circuit.get()), circuit->ptr_size);
  }

} // namespace circ::eqsat
