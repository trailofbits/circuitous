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
        using rules_t = std::vector< eqsat::RuleSet >;

        EqualitySaturationPass() = default;
        EqualitySaturationPass( rules_t &&rules )
        {
            add_rules( std::move( rules ) );
        }

        CircuitPtr run(CircuitPtr &&circuit) override
        {
            return eqsat::EqualitySaturation(std::move(circuit), rulesets);
        }

        void add_rules( rules_t &&ruleset )
        {
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
     * to both input/output carry flags.
     *
     * This pass tries to match the generated remill semantics tree completely
     * and patches it out.
     * It does make implicit assumptions on the order of operands.
     */

    bool has_remill_overflow_flag_semantics( RegConstraint *op );

    struct RemillOFPatch : PassBase
    {
        CircuitPtr run( CircuitPtr &&circuit ) override
        {
            auto output_of = circuit->fetch_reg< OutputRegister >( "OF" );
            auto output_cf = circuit->fetch_reg< OutputRegister >( "CF" );

            auto input_cf = circuit->fetch_reg< InputRegister >( "CF" );

            if ( !output_of || !input_cf || !output_cf )
                return std::move(circuit);

            for ( auto reg_constraint : circuit->attr< RegConstraint >() )
            {
                if ( reg_constraint->operands_size() != 2 ||
                     reg_constraint->operand( 1 ) != output_of ||
                     !has_remill_overflow_flag_semantics( reg_constraint ))
                {
                    continue;
                }

                auto xor_node = circuit->create< Xor >( 1u );
                xor_node->add_operands( input_cf, output_cf );

                reg_constraint->replace_operand( 0, xor_node );
            }

            return std::move(circuit);
        }
    };


    struct MergeAdviceConstraints : PassBase
    {
        CircuitPtr run( CircuitPtr &&circuit ) override
        {
            for ( auto ac : circuit->attr< AdviceConstraint >() )
            {
                check( ac->operands_size() == 2 );
                if ( !isa< Advice >( ac->advice() ) ||
                     !isa< Advice >( ac->runtime_value() ) )
                {
                    continue;
                }

                ac->advice()->replace_all_uses_with( ac->runtime_value() );
                ac->destroy();
            }
            return std::move( circuit );
        }
    };


    struct DummyPass : PassBase
    {
        CircuitPtr run( CircuitPtr &&circuit ) override { return std::move( circuit ); }
    };


    template< typename ... Ops >
    struct CollapseUnary : PassBase
    {

        template< typename H, typename ... Tail >
        static void do_run( Circuit *circuit )
        {
            collapse< H >( circuit );
            if constexpr ( sizeof ... ( Tail ) != 0 )
                return do_run< Tail ... >( circuit );
        }

        template< typename Op >
        static void collapse( Circuit *circuit )
        {
            for ( auto op : circuit->attr< Op >() )
                if ( op->operands_size() == 1 )
                    op->replace_all_uses_with( op->operand( 0 ) );
        }

        CircuitPtr run( CircuitPtr &&circuit ) override
        {
            do_run< Ops ... >( circuit.get() );
            return std::move( circuit );
        }
    };

    template< typename ... Ops >
    struct CollapseUnary< tl::TL< Ops ... > > : CollapseUnary< Ops ... >
    {
        using CollapseUnary< Ops ... >::run;
    };

    using CollapseOpsPass = CollapseUnary< collapsable >;

    struct PassesBase
    {
        NamedPass &add_pass( const std::string &name, Pass pass )
        {
            log_info() << "Adding pass: " << name;
            return passes.emplace_back( name, std::move( pass ) );
        }

        template< typename P, typename ... Args >
        std::shared_ptr< P > emplace_pass( const std::string &name, Args && ... args )
        {
            log_info() << "Creating pass" << name;
            auto pass = std::make_shared< P >( std::forward< Args >( args ) ... );
            add_pass( name, pass );
            return pass;
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

    template< typename Next >
    struct WithHistory : Next
    {
        using Snapshot = std::pair< std::string, RawNodesCounter >;

        CircuitPtr run_pass( const NamedPass &npass, CircuitPtr &&circuit )
        {
            const auto &[ name, pass ] = npass;

            if ( history.size() == 0 )
              make_snapshot(circuit);

            auto result = this->Next::run_pass( npass, std::move( circuit ) );
            make_snapshot( result, name );
            return result;
        }

        std::string report() const
        {
            using printer = Printer< RawNodesCounter >;

            if (history.empty())
              return "empty history";

            std::stringstream ss;

            {
                const auto &[ name, def ] = history[ 0 ];
                ss << name << ":" << std::endl;
                printer::Print( ss, def );
            }

            for ( std::size_t i = 1; i < history.size(); ++i )
            {
                auto &[ name, def ] = history[ i ];
                ss << name << ":" << std::endl;
                printer::Diff( ss, history[ i - 1 ].second, def );
            }

            ss << std::endl << "In the end:" << std::endl;
            printer::Print( ss, history.back().second );
            ss << std::endl;
            return ss.str();
        }

    private:

        void make_snapshot( const CircuitPtr &circuit,
                            std::optional< std::string > after = std::nullopt )
        {
            auto name = after ? after.value() : "start";
            log_info() << "Start capturing statistics.";
            RawNodesCounter collector;
            collector.Run( circuit.get() );
            log_info() << "Done capturing statistics.";
            history.emplace_back( name, std::move( collector ) );
        }

        std::vector< Snapshot > history;
    };

    template< typename Next >
    struct Defensive : Next
    {
        CircuitPtr run_pass( const NamedPass &npass, CircuitPtr &&circuit )
        {
            const auto &[ name, pass ] = npass;

            log_info() << "Going to run transformation" << name;
            auto result = this->Next::run_pass( npass, std::move( circuit ) );
            log_info() << "Done. Running verify pass:";

            auto verify_res = verify_circuit( result.get() );

            if ( verify_res.has_errors() )
              log_kill() << "Verify failed!\n" << verify_res;

            if ( verify_res.has_warnings() )
              log_error() << verify_res.get_warnings();

            log_info() <<"Circuit is okay.";
            return result;
        }
    };

    template< typename Next >
    struct Passes : Next
    {
        CircuitPtr run( CircuitPtr &&circuit )
        {
            if ( this->passes.empty() )
                return std::move( circuit );

            CircuitPtr result = run_pass( this->passes.front(), std::move( circuit ) );
            for ( const auto &pass : detail::tail( this->passes ) )
                result = run_pass( pass, std::move( result ) );

            return result;
        }

        CircuitPtr run_pass( const NamedPass &pass, CircuitPtr &&circuit )
        {
          return this->Next::run_pass( pass, std::move( circuit ) );
        }

        std::string report() const { return this->Next::report(); }
    };

} // namespace circ
