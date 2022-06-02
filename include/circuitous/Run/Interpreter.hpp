/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
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

        Circuit *circuit;

        CtxCollector collector;
        std::vector< std::pair< VerifyInstruction *, std::unique_ptr< Spawn > > > runners;

        Spawn *acceptor = nullptr;

        QueueInterpreter(Circuit *circuit_) : circuit(circuit_)
        {
            collector.Run(circuit);

            for (auto vi : circuit->attr<VerifyInstruction>())
                runners.emplace_back( vi, std::make_unique< Spawn >(circuit, vi, &collector) );
        }

        void runners_do(auto &&f)
        {
            for (auto &[_, runner] : runners)
                f(*runner);
        }

        void set_input_state(const trace::Entry &in)
        {
            return runners_do([ & ]( auto &runner ){ runner.set_input_state(in); } );
        }

        void set_output_state(const trace::Entry &out)
        {
            return runners_do([ & ]( auto &runner ){ runner.set_output_state(out); } );
        }

        void set_memory(uint64_t addr, const std::string &val)
        {
            return runners_do([ & ]( auto &runner ){ runner.set_memory(addr, val); } );
        }

        template< typename T, typename ...Ts >
        void init()
        {
          for (auto c : circuit->attr< T >())
              runners_do([ & ]( auto &runner ){ runner.visit(c); } );

          if constexpr (sizeof...(Ts) != 0) {
              init< Ts... >();
          }
        }

        auto values() const
        {
            // TODO(lukas): This is dubious once we consider verify mode
            check(acceptor);
            return acceptor->node_state.take();
        }

        void init()
        {
            init< Undefined, Constant >();
        }

        bool result()
        {
            uint32_t acceptors = 0;
            runners_do([ & ](auto &runner ){ if ( *runner.result ) ++acceptors; });
            return acceptors == 1;
        }

        std::string dump_spawn(VerifyInstruction *v, Spawn &runner)
        {
            std::stringstream ss;
            ss << runner.GetNodeVal(v)->toString(16, false) << std::endl;;
            ss << pretty_print(v) << std::endl;
            for (auto op : v->operands)
            {
                ss << "\t" << op_code_str(op->op_code) << " " << op->id() << " "
                   << runner.GetNodeVal(op)->toString(16, false) << std::endl;
            }
            return ss.str();
        }

        // TODO(lukas): If runners do is `const` this can be as well.
        std::string dump_runners()
        {
            std::stringstream ss;
            runners_do([ & ](auto &runner){
                ss << "TODO(lukas); Implement\n";
            });
            return ss.str();
        }

        bool Run()
        {
            init();

            std::unordered_set<Spawn *> successes;
            for (auto &[_, runner] : runners)
                if (runner->Run())
                    successes.emplace(runner.get());
            if (successes.size() == 1)
                acceptor = *successes.begin();

            check(!(successes.size() > 1)) << "Multiple contexts satisfied."
                                           << successes.size();
            return successes.size() == 1;
        }
    };

    using DQueueInterpreter = QueueInterpreter<DSpawn>;
    using VQueueInterpreter =  QueueInterpreter<VSpawn>;

}  // namespace circ::run
