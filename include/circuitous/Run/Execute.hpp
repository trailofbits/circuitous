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

    // A really simple mechanism to keep track of how transitions between states went,
    // possible results to export and signalisation of early termination.
    // Object is expected to be stateful. It is guaranteed that before each `next_memory`
    // there will be at least one call to `process` - all of which must return `true`.
    struct DefaultControl
    {
        std::optional< Memory > memory;
        // TODO(lukas): Make configurable - we may want to export full traces for example.
        std::vector< ExportMemory > to_export;

        auto result() const { check(!to_export.empty()); return to_export.back().result; }

        auto get_acceptor(auto &&results) const
        {
            for (auto &&[r, spawn] : results)
                if (accepted(r))
                    return std::move(spawn);
            unreachable() << "Was not able to retrieve accepting Spawn object!";
        }

        template< typename I >
        bool process(std::size_t idx, typename I::result_vector_t &&results, I &&interpreter)
        {
            static const auto join = [](const auto &res)
            {
                using R = typename I::result_t;

                static const auto inner_join = [](auto a, auto b)
                {
                    if (accepted(a) && rejected(b)) return a;
                    if (accepted(b) && rejected(a)) return b;

                    if (error(a)) return a;
                    if (error(b)) return b;

                    if (accepted(a) && accepted(b))
                        return R::multiple_ctx_satisfied;

                    if (rejected(a) && rejected(b))
                        return a;

                    unreachable() << "inner join did encounter invalid combination"
                                  << to_string(a) << " and " << to_string(b);
                };

                std::optional< typename I::result_t > out;
                for (const auto &[r, _] : res)
                {
                    if (out)
                        out = inner_join(*out, r);
                    else
                        out = r;
                }

                dcheck(out, [](){ return "Optional does not have value."; });
                return *out;
            };

            auto joined = join(results);
            if (!accepted(joined))
            {
                if (rejected(joined))
                    to_export.emplace_back(ExportMemory::reject());
                if (error(joined))
                    to_export.emplace_back(ExportMemory::error());
                return false;
            }

            auto acceptor = get_acceptor(std::move(results));
            check(acceptor);

            to_export.emplace_back(ExportMemory::accept(*acceptor));
            memory = acceptor->take_memory();

            return true;

        }

        Memory next_memory()
        {
            check(memory);
            return std::move(*memory);
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
            auto interpreter = make_tester< Interpreter >(circuit, std::move(node_state),
                                                                   std::move(memory));

            auto status = interpreter.run_all();

            if (!exec.process(i, std::move(status), std::move(interpreter)))
                return;

            memory = exec.next_memory();
        }
    }

}  // namespace circ::run
