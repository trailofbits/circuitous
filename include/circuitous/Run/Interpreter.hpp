/*
 * Copyright (c) 2020-2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Run/Base.hpp>
#include <circuitous/Run/Spawn.hpp>

#include <circuitous/Support/Check.hpp>

namespace circ::run
{
    // For each context a `Spawn` object is created and run to interpreter it. Initial node
    // state and memory is copied to each spawn - there is no option to specialize the input
    // per spawn.
    // If one is interested in output states, `Spawn` classes do contain those (and can be
    // retrieved as result of call to `run_all`).
    // NOTE(lukas): Currently there is no way to run only one spawn - if it is desired
    // the `Spawn` class can be constructed directly instead of using this manager.
    template< typename Spawn >
    struct QueueInterpreter
    {
        using self_t = QueueInterpreter< Spawn >;

        using spawn_t = Spawn;
        using spawns_t = std::vector< std::unique_ptr< Spawn > >;
        using spawn_state_t = std::tuple< Memory, NodeState >;

        Circuit *circuit;
        // For each context, we want to only interpret operations that are relevant for it.
        CtxCollector collector;

        NodeState initial_node_state;
        Memory initial_memory;

        std::unordered_set< Operation * > to_derive;

        enum class result_t : uint32_t
        {
            valid = 0,
            not_decoded = 1,
            multiple_ctx_satisfied = 2,
            runtime_error = 3
        };

        struct Result
        {
            result_t raw;

            operator bool() const
            {
                return raw == result_t::valid;
            }

            std::string to_string() const
            {
                switch(raw)
                {
                    case result_t::valid : return "valid";
                    case result_t::not_decoded : return "not decoded";
                    case result_t::multiple_ctx_satisfied : return "multiple_ctx_satisfied";
                    case result_t::not_decoded : return "runtime_error";
                }
            }
        };


        QueueInterpreter(Circuit *circuit,
                         const NodeState &node_state, const Memory &memory)
            : circuit(circuit), initial_node_state(node_state), initial_memory(memory)
        {
            collector.Run(circuit);
        }

        // Returns all the spawns that accepted this run. In case of successful run,
        // there should be one - if there are more there is probably a bug in the lifter
        // or runner.
        spawns_t run_all()
        {
            spawns_t out;
            for (auto ctx : circuit->attr< VerifyInstruction >())
            {
                auto runner = std::make_unique< Spawn >(
                        circuit, ctx, &collector, initial_node_state, initial_memory);
                runner->derive( to_derive );
                if (runner->run())
                    out.push_back(std::move(runner));
            }
            return out;
        }

        // Mark some `Operation` types as being able to derive values for some of their
        // operands. This will almost always be `AdviceConstraint` for example.
        // Value is derived only if the operation does not have a value - otherwise normal
        // semantic is used.
        template< typename T, typename ... Ts >
        self_t &derive()
        {
            auto &ops = circuit->attr< T >();
            to_derive.insert( ops.begin(), ops.end() );
            if constexpr (sizeof ... (Ts) != 0)
                return derive< Ts ... >();
            return *this;
        }

        self_t &derive_memory()
        {
            return derive< ReadConstraint, WriteConstraint, UnusedConstraint >();
        }

        self_t &derive_advices()
        {
            return derive< AdviceConstraint >();
        }
    };

    using Interpreter = QueueInterpreter< Spawn< Base > >;

}  // namespace circ::run
