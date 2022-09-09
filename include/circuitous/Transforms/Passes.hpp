/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Cost.hpp>
#include <circuitous/IR/Verify.hpp>

#include <circuitous/Transforms/PassBase.hpp>
#include <circuitous/Transforms/MergeAdvices.hpp>
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
      return eqsat::EqualitySaturation(std::move(circuit), rulesets);
    }

    static Pass get() { return std::make_shared< EqualitySaturationPass >(); }

    void add_rules(std::vector< eqsat::RuleSet > &&ruleset) {
      rulesets.insert(
        rulesets.end(),
        std::make_move_iterator(ruleset.begin()),
        std::make_move_iterator(ruleset.end())
      );
    }

    std::vector<eqsat::RuleSet> rulesets;
  };

    /*
     * Semantics from remill calculate the overflow flag directly from the values instead of
     * re-using existing flags. This leads to unnecessary computation as we have access
     * to both input/output carry flags
     *
     * This pass tries to match the generated remill semantics tree completely and patches it out
     * It does make implicit assumptions on the order of operands
     */
    bool has_remill_overflow_flag_semantics(RegConstraint* op);

    struct RemillOFPatch : PassBase
    {
        CircuitPtr run(CircuitPtr &&circuit) override
        {
            auto output_of = circuit->fetch_reg<OutputRegister>("OF");
            auto input_cf = circuit->fetch_reg<InputRegister>("CF");
            auto output_cf = circuit->fetch_reg<OutputRegister>("CF");
            if(output_of == nullptr || input_cf == nullptr || output_cf == nullptr)
                return std::move(circuit);

            for ( auto regC : circuit->attr< RegConstraint >() )
            {
                if ( regC->operands_size() != 2 ||
                     regC->operand( 1 ) != output_of || !has_remill_overflow_flag_semantics( regC ))
                continue;

                auto xor_node = circuit.operator->()->create<Xor>(1u);
                xor_node->add_operand(input_cf);
                xor_node->add_operand(output_cf);
                regC->replace_operand(0, xor_node);
            }

            return std::move(circuit);
        }

        static Pass get() { return std::make_shared< RemillOFPatch >(); }
    };


    struct MergeAdviceConstraints : PassBase
    {
        CircuitPtr run(CircuitPtr &&circuit) override
        {
            for ( auto ac : circuit->attr< AdviceConstraint >() )
            {
                if ( ac->operands_size() != 2 || !isa< Advice >( ac->operand( 0 ) ) ||
                     !isa< Advice >( ac->operand( 1 ) ))
                    continue;

                auto lhs = ac->operand( 0 );
                auto rhs = ac->operand( 1 );

                /*
                 * It is important to clear usages of the AC before we replace the uses
                 * since otherwise advice_1 will gain two uses of the AC.
                 * Which at the time of writing can cause trouble when deleting.
                 */
                ac->destroy();
                rhs->replace_all_uses_with( lhs );
            }
            return std::move( circuit );
        }

        static Pass get() { return std::make_shared< MergeAdviceConstraints >(); }
    };


  struct DummyPass : PassBase
  {
    CircuitPtr run(CircuitPtr &&circuit) override { return std::move(circuit); }

    static Pass get() { return std::make_shared< DummyPass >(); }
  };

  struct TrivialConcatRemovalPass : PassBase
  {
      CircuitPtr run( CircuitPtr &&circuit ) override
      {
          for ( auto c : circuit->attr< Concat >() )
              if ( c->operands_size() == 1 )
                  c->replace_all_uses_with( c->operand( 0 ) );

          return std::move( circuit );
      }

      static Pass get() { return std::make_shared< TrivialConcatRemovalPass >(); }
  };

  struct PassesBase
  {
      Pass make_known_pass( const std::string &name )
      {
            if ( name == "eqsat" )                    return EqualitySaturationPass::get();
            if ( name == "dummy-pass" )               return DummyPass::get();
            if ( name == "trivial-concat-removal" )   return TrivialConcatRemovalPass::get();
            if ( name == "overflow-flag-fix" )        return RemillOFPatch::get();
            if ( name == "merge-transitive-advices" ) return MergeAdviceConstraints::get();
            if ( name == "wip-merge-with-advices" )   return MergeWithAdvicesPass::get();
            log_kill() << "Unkown pass option:" << name;
      }

      NamedPass &add_pass( const std::string &name )
      {
          auto pass = make_known_pass( name );
          log_info() << "Adding pass: " << name;
          return passes.emplace_back( name, pass );
      }

      CircuitPtr run_pass( const Pass &pass, CircuitPtr &&circuit )
      {
          auto result = pass->run( std::move( circuit ) );
          result->remove_unused();
          return result;
      }

      CircuitPtr run_pass( const NamedPass &npass, CircuitPtr &&circuit )
      {
          const auto &[ _, pass ] = npass;
          return run_pass( pass, std::move( circuit ) );
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

      {
        const auto &[name, def] = history[0];
        ss << name << ":" << std::endl;
        printer::Print(ss, def);
      }

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

      auto verify_res = verify_circuit(result.get());

      if (verify_res.has_errors())
        log_kill() << "Verify failed!\n"
                   << verify_res;

      if (verify_res.has_warnings())
        log_error() << verify_res.get_warnings();

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
