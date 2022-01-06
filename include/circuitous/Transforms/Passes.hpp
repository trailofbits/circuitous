/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Cost.hpp>
#include <circuitous/IR/Verify.hpp>

#include <circuitous/Transforms/PassBase.hpp>
#include <circuitous/Transforms/EqualitySaturation.hpp>
#include <optional>
#include <string>
#include <vector>

namespace circ
{
  namespace detail
  {
    inline auto tail(const auto &vec)
    {
      auto tail = std::next(vec.begin());
      return std::span( &(*tail), vec.size() - 1 );
    }
  } // namesapce detail


  struct EqualitySaturationPass : PassBase
  {
    CircuitPtr run(CircuitPtr &&circuit) override
    {
      return eqsat::EqualitySaturation(std::move(circuit));
    }

    static Pass get() { return std::make_shared< EqualitySaturationPass >(); }
  };


  // Merge all of the hint inputs into a single "wide" input hint that is of
  // sufficient size to support all verifiers. In place of the individual hints,
  // the verifiers pull out slices of wide hint value with an EXTRACT.
  bool MergeAdvices(Circuit *circuit);

  struct MergeAdvicesPass : PassBase
  {
    CircuitPtr run(CircuitPtr &&circuit) override
    {
      MergeAdvices(circuit.get());
      return std::move(circuit);
    }

    static Pass get() { return std::make_shared< MergeAdvicesPass >(); }
  };


  struct DummyPass : PassBase
  {
    CircuitPtr run(CircuitPtr &&circuit) override { return std::move(circuit); }

    static Pass get() { return std::make_shared< DummyPass >(); }
  };

  struct PassesBase
  {
    // list of recognized passes
    static inline std::map< std::string, Pass > known_passes
    {
      { "eqsat",           EqualitySaturationPass::get() },
      { "merge-advices",   MergeAdvicesPass::get() },
      { "dummy-pass",      DummyPass::get() }
    };

    void add_pass(const std::string &name)
    {
      auto pass = known_passes.at(name);
      log_info() << "Adding pass: " << name;
      passes.emplace_back(name, pass);
    }

    CircuitPtr run_pass(const Pass &pass, CircuitPtr &&circuit)
    {
      auto result = pass->run(std::move(circuit));
      result->RemoveUnused();
      return result;
    }

    CircuitPtr run_pass(const NamedPass &npass, CircuitPtr &&circuit)
    {
      const auto &[_, pass] = npass;
      return run_pass(pass, std::move(circuit));
    }

    std::string report() const { return "no report recorded"; }

    std::vector< NamedPass > passes;
  };

  template<typename Next>
  struct WithHistory : Next
  {
    using Snapshot = std::pair< std::string, RawNodesCounter >;

    CircuitPtr run_pass(const NamedPass &npass, CircuitPtr &&circuit)
    {
      const auto &[name, pass] = npass;

      if (history.size() == 0)
        make_snapshot(circuit);

      auto result = this->Next::run_pass(npass, std::move(circuit));
      make_snapshot(result, name);
      return result;
    }

    std::string report() const
    {
      using printer = Printer<RawNodesCounter>;

      if (history.empty()) {
        return "empty history";
      }

      std::stringstream ss;
      const auto &[name, def] = history[0];
      ss << name << ":" << std::endl;
      printer::Print(ss, def);
      for (std::size_t i = 1; i < history.size(); ++i) {
        auto &[name, def] = history[i];
        ss << name << ":" << std::endl;
        printer::Diff(ss, history[i - 1].second, def);
      }

      ss << std::endl << "In the end:" << std::endl;
      printer::Print(ss, history.back().second);
      ss << std::endl;
      return ss.str();
    }

  private:

    void make_snapshot(const CircuitPtr &circuit, std::optional< std::string > after = std::nullopt)
    {
      auto name = after ? after.value() : "start";
      log_info() << "Start capturing statistics.";
      RawNodesCounter collector;
      collector.Run(circuit.get());
      log_info() << "Done capturing statistics.";
      history.emplace_back(name, std::move(collector));
    }

    std::vector< Snapshot > history;
  };

  template< typename Next >
  struct Defensive : Next
  {
    CircuitPtr run_pass(const NamedPass &npass, CircuitPtr &&circuit)
    {
      const auto &[name, pass] = npass;

      log_info() << "Going to run transformation" << name;
      auto result = this->Next::run_pass(npass, std::move(circuit));
      log_info() << "Done. Running verify pass:";

      const auto &[status, msg, warnings] = VerifyCircuit(result.get());

      if (!status)
        log_kill() << "Verify failed!\n"
                   << warnings << "\n"
                   << msg;

      if (!warnings.empty())
        log_error() << warnings;

      log_info() <<"Circuit is okay.";
      return result;
    }
  };

  template< typename Next >
  struct Passes : Next
  {
    CircuitPtr run(CircuitPtr &&circuit)
    {
      if (this->passes.empty())
        return std::move(circuit);

      CircuitPtr result = run_pass(this->passes.front(), std::move(circuit));
      for (const auto &pass : detail::tail(this->passes))
        result = run_pass(pass, std::move(result));

      return result;
    }

    CircuitPtr run_pass(const NamedPass &pass, CircuitPtr &&circuit)
    {
      return this->Next::run_pass(pass, std::move(circuit));
    }

    std::string report() const { return this->Next::report(); }
  };

} // namespace circ
