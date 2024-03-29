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
        using spawn_ptr_t = std::unique_ptr< Spawn >;
        using spawns_t = std::vector< spawn_ptr_t >;
        using spawn_state_t = std::tuple< Memory, NodeState >;

        Circuit *circuit;
        // For each context, we want to only interpret operations that are relevant for it.
        CtxCollector ctx_info;

        NodeState initial_node_state;
        Memory initial_memory;

        std::unordered_set< Operation * > to_derive;

        QueueInterpreter(Circuit *circuit,
                         const NodeState &node_state, const Memory &memory)
            : circuit(circuit),
              ctx_info(circuit),
              initial_node_state(node_state), initial_memory(memory)
        {}

        using result_t = typename Spawn::result_t;
        // Result of the run + the entire spawn for end state investigation.
        using spawn_result_t = std::tuple< typename Spawn::result_t, spawn_ptr_t >;
        // Grouped for the whole batch.
        using result_vector_t = std::vector< spawn_result_t >;

        // Returns all the spawns that accepted this run. In case of successful run,
        // there should be one - if there are more there is probably a bug in the lifter
        // or runner.
        result_vector_t run_all()
        {
            result_vector_t results;

            log_dbg() << "[QueueInterpreter]:" << "Gping to run:"
                      << circuit->attr< VerifyInstruction >().size()
                      << "runs.";
            for (auto ctx : circuit->attr< VerifyInstruction >())
            {
                auto runner = std::make_unique< Spawn >(
                        circuit, ctx, ctx_info, initial_node_state, initial_memory);
                runner->derive( to_derive );
                auto status = runner->run();
                log_dbg() << "[QueueInterpreter]:" << to_string( status );
                results.push_back(std::make_tuple(status, std::move(runner)));
            }
            return results;
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

    using Interpreter = QueueInterpreter< DerivingSpawn< Base > >;

    // For each context a `Spawn` object is created and run to interpreter it. Initial node
    // state and memory is copied to each spawn - there is no option to specialize the input
    // per spawn.
    // If one is interested in output states, `Spawn` classes do contain those (and can be
    // retrieved as result of call to `run_all`).
    // NOTE(lukas): Currently there is no way to run only one spawn - if it is desired
    // the `Spawn` class can be constructed directly instead of using this manager.
    template< typename Spawn >
    struct StrictVerifyInterpreter
    {
        using self_t = StrictVerifyInterpreter< Spawn >;

        using spawn_t = Spawn;
        using spawn_ptr_t = std::unique_ptr< Spawn >;
        using spawns_t = std::vector< spawn_ptr_t >;
        using spawn_state_t = std::tuple< NodeState >;

        Circuit *circuit;
        NodeState initial_node_state;

        StrictVerifyInterpreter( Circuit *circuit,
                                 const NodeState &node_state )
            : circuit( circuit ),
              initial_node_state( node_state )
        {}

        using result_t = typename Spawn::result_t;
        // Result of the run + the entire spawn for end state investigation.
        using spawn_result_t = std::tuple< typename Spawn::result_t, spawn_ptr_t >;
        // Grouped for the whole batch.
        using result_vector_t = std::vector< spawn_result_t >;

        // Returns all the spawns that accepted this run. In case of successful run,
        // there should be one - if there are more there is probably a bug in the lifter
        // or runner.
        result_vector_t run_all()
        {
            result_vector_t results;
            log_dbg() << "[run:SVI]:" << "Going to spawn & execute for each memory"
                                      <<  "permutation";

            for ( auto state : initial_node_state.permutate_memory( circuit ) )
            {
                auto runner = std::make_unique< Spawn >( circuit, state );
                auto status = runner->run();

                log_dbg() << "[run:SVI]:" << "spawn result:" << to_string( status );
                results.emplace_back( status, std::move( runner ) );
            }

            log_dbg() << "[run:SVI]:" << "Results count:" << results.size();
            return results;
        }

        // TODO( run ): Keeping this just to make it compatible, we want it removed in the
        //              future.
        template< typename T, typename ... Ts >
        self_t &derive()
        {
            return *this;
        }

        self_t &derive_memory()
        {
            return *this;
        }

        self_t &derive_advices()
        {
            return *this;
        }
    };

    using SVI = StrictVerifyInterpreter< spawn_verifier >;
}  // namespace circ::run
