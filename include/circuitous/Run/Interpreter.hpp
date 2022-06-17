/*
 * Copyright (c) 2020-2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Run/Base.hpp>
#include <circuitous/Run/Spawn.hpp>

#include <circuitous/Support/Check.hpp>

namespace circ::run
{
    template< typename Spawn >
    struct QueueInterpreter
    {
        using spawn_t = Spawn;
        using spawns_t = std::vector< std::unique_ptr< Spawn > >;
        using spawn_state_t = std::tuple< Memory, NodeState >;

        Circuit *circuit;
        CtxCollector collector;

        NodeState initial_node_state;
        Memory initial_memory;

        std::unordered_set< Operation * > to_derive;

        QueueInterpreter(Circuit *circuit,
                         const NodeState &node_state, const Memory &memory)
            : circuit(circuit), initial_node_state(node_state), initial_memory(memory)
        {
            collector.Run(circuit);
        }

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

        template< typename T >
        void derive()
        {
            auto &ops = circuit->attr< T >();
            to_derive.insert( ops.begin(), ops.end() );
        }
    };

    using Interpreter = QueueInterpreter< Spawn< Base > >;

    struct NodeStateBuilder
    {
        using self_t = NodeStateBuilder;

      private:
        NodeState node_state;
        Circuit *circuit;

      public:
        NodeStateBuilder(Circuit *circuit) : circuit(circuit) {}

        auto take() { return std::move(node_state); }
        self_t &input_trace(const trace::Entry &in);
        self_t &output_trace(const trace::Entry &out);

        template< typename T >
        self_t &all(const value_type &v)
        {
            for (auto op : circuit->attr< T >())
                node_state.set(op, v);
            return *this;
        }
    };

    struct MemoryBuilder
    {
        using self_t = MemoryBuilder;
      private:
        Memory memory;

      public:
        MemoryBuilder(Circuit *circuit) : memory(circuit) {}

        auto take() { return std::move(memory); }
        self_t &set(std::size_t addr, const std::string &val);
        self_t &set(const trace::Entry &trace);
    };


}  // namespace circ::run
