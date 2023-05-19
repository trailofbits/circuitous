/*
 * Copyright (c) 2020-2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Run/Interpreter.hpp>
#include <circuitous/Run/Trace.hpp>
#include <circuitous/Support/Check.hpp>

namespace circ::run
{
    template< typename Interpreter, typename ... Args >
    Interpreter make_tester(Args && ... args)
    {
        auto out = Interpreter(std::forward< Args >(args) ...);
        out.derive_memory().derive_advices();
        return out;
    }

    struct ExportMemory
    {
        std::string result;
        std::vector< Memory::Parsed > hints;

      private:
        ExportMemory(const std::string &str) : result(str) {}

      public:

        static ExportMemory error() { return ExportMemory("error"); }
        static ExportMemory reject() { return ExportMemory("reject"); }

        static ExportMemory accept(const auto &spawn)
        {
            ExportMemory self("accept");
            self.hints = spawn.get_derived_mem();
            return self;
        }
    };

    struct ExportRawTraces
    {
        using step_trace_t = std::tuple< std::string, std::string >;
        step_trace_t states;

      private:
        ExportRawTraces(step_trace_t &&states) : states(std::move(states)) {}
        ExportRawTraces() = default;

      public:
        static ExportRawTraces error() { return {}; }
        static ExportRawTraces reject() { return {}; }
        static ExportRawTraces accept(const auto &spawn)
        {
            return ExportRawTraces(spawn.to_traces());
        }
    };

    template< typename Self >
    struct ControlBase
    {
        auto inner_join( result_t a, result_t b )
        {
            if ( accepted( a ) && rejected( b ) ) return a;
            if ( accepted( b ) && rejected( a ) ) return b;

            if ( error( a ) ) return a;
            if ( error( b ) ) return b;

            if ( accepted( a ) && accepted( b ) )
                return a;

            if ( rejected( a ) && rejected( b ) )
                return a;

            unreachable() << "inner join did encounter invalid combination"
                          << to_string(a) << " and " << to_string(b);
        }

        auto join( const auto &res )
        {
            std::optional< result_t > out;
            for (const auto &[r, _] : res)
            {
                if (out)
                    out = inner_join(*out, r);
                else
                    out = r;
            }

            dcheck(out, [](){ return "Optional does not have value."; });
            return *out;
        }
    };

    // A really simple mechanism to keep track of how transitions between states went,
    // possible results to export and signalisation of early termination.
    // Object is expected to be stateful. It is guaranteed that before each `next_memory`
    // there will be at least one call to `process` - all of which must return `true`.
    template< typename ToExport >
    struct DefaultControl : ControlBase< DefaultControl< ToExport > >
    {
        std::optional< Memory > memory;
        // TODO(lukas): Make configurable - we may want to export full traces for example.
        // TODO(lukas): We also want this probably to be its own object that can hold
        //              a collection if more entries are relevant.
        std::vector< ToExport > to_export;

        auto result() const
        {
            check(!to_export.empty());
            return to_export.back().result;
        }

        auto get_acceptor( auto &&results ) const
        {
            for (auto &&[r, spawn] : results)
                if (accepted(r))
                    return std::move(spawn);
            unreachable() << "Was not able to retrieve accepting Spawn object!";
        }

        template< typename I >
        bool process(std::size_t idx, typename I::result_vector_t &&results, I &&interpreter)
        {
            auto joined = this->join(results);
            if (!accepted(joined))
            {
                if (rejected(joined))
                    to_export.emplace_back(ToExport::reject());
                if (error(joined))
                    to_export.emplace_back(ToExport::error());
                return false;
            }

            auto acceptor = get_acceptor(std::move(results));
            check(acceptor);

            to_export.emplace_back(ToExport::accept(*acceptor));
            memory = acceptor->take_memory();

            return true;

        }

        Memory next_memory()
        {
            check(memory);
            return std::move(*memory);
        }
    };

    using parsed_mem_hint = Memory::Parsed;
    using parsed_mem_hints = std::vector< parsed_mem_hint >;

    template< typename Interpreter = SVI >
    struct StatelessControl : ControlBase< StatelessControl< Interpreter > >
    {
        using statuses_t = std::vector< result_t >;

        template< typename I >
        bool process( std::size_t idx, typename I::result_vector_t &&results, I &&interpreter )
        {
            return accepted( this->join( results ) );
        }

        auto process_results( auto &&results )
        {
            return this->join( results );
        }

        auto fill_unreachable( auto results, std::size_t total_size, auto &&yield )
        {
            while ( results.size() != total_size )
            {
                results.push_back( result_t::unreachable );
                typename Interpreter::result_vector_t arg;
                arg.emplace_back( result_t::unreachable, typename Interpreter::spawn_ptr_t{} );
                yield( arg );
            }
            return results;
        }

        auto run_step( circuit_ref_t circuit, const auto &step, auto &&yield )
        {
            auto node_state = NodeStateBuilder( circuit )
                .set( step )
                .fill_memory()
                .template all< Undefined >( {} )
                .take();
            auto interpreter = SVI( circuit, std::move( node_state ) );
            auto result_spawn_pairs = interpreter.run_all();
            yield( result_spawn_pairs );
            return result_spawn_pairs;
        }

        auto test( circuit_ref_t circuit, auto trace, auto &&yield ) -> statuses_t
        {
            check( trace.entries.size() >= 2 );

            statuses_t statuses;

            for ( std::size_t i = 0; i < trace.size() - 1; ++i )
            {
                auto step = trace::native::make_step_trace( circuit, trace[ i ],
                                                            trace[ i + 1 ] );
                auto status = process_results( run_step( circuit, step, yield ) );
                statuses.push_back( status );

                if ( !accepted( statuses.back() ) )
                    return fill_unreachable( statuses, trace.size() - 1, yield );
            }
            return statuses;
        }
    };


    template< typename Trace, typename Executor >
    auto test_trace(Circuit *circuit, Trace trace, Executor &&exec)
    {
        check(trace.entries.size() >= 2);

        MemoryBuilder memory_builder(circuit);
        for (const auto &[addr, val] : trace.initial_memory)
            memory_builder.set(addr, val);

        auto memory = memory_builder.take();

        for (std::size_t i = 0; i < trace.size() - 1; ++i)
        {
            auto step = trace::native::make_step_trace(circuit, trace[i], trace[i + 1]);
            auto node_state = NodeStateBuilder(circuit)
                .set(step)
                .template all< Undefined >({})
                .take();
            auto interpreter = make_tester< Interpreter >(
                    circuit, std::move(node_state),
                    std::move(memory));

            auto status = interpreter.run_all();

            if (!exec.process(i, std::move(status), std::move(interpreter)))
                return;

            memory = exec.next_memory();
        }
    }

}  // namespace circ::run
