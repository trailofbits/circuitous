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

    // Yield accepts `result vector, interpreter` and return `bool`, which signifies whether
    // verification should continue.
    template< typename Trace, typename Yield  >
    auto test_trace(Circuit *circuit, Trace trace, Yield &&yield)
    {
        check(trace.entries.size() >= 2);

        MemoryBuilder memory_builder(circuit);
        for (const auto &[addr, val] : trace.initial_memory)
            memory_builder.set(addr, val);

        auto memory = memory_builder.take();

        for (std::size_t i = 0; i < trace.size() - 1; ++i)
        {
            auto step = trace::native::make_step_trace(circuit, trace[i], trace[i + 1]);
            auto node_state = NodeStateBuilder(circuit).set(step).take();
            auto interpreter = make_tester< Interpreter >(circuit, std::move(node_state),
                                                                   std::move(memory));

            auto status = interpreter.run_all();

            if (!yield(status, interpreter))
                return;
        }
    }

}  // namespace circ::run
