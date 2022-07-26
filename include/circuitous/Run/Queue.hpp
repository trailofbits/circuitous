/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <circuitous/IR/Shapes.hpp>

#include <deque>
#include <unordered_map>

namespace circ::run
{
    struct MemoryOrdering
    {
        using mem_ops_t = std::unordered_set< Operation * >;
        using level_t = std::tuple< uint32_t, mem_ops_t >;
        using constraints_t = std::vector< level_t >;

      private:

        CtxCollector *collector;
        constraints_t constraints;

      public:
        uint32_t allowed = 0;

        MemoryOrdering(Circuit *circuit, CtxCollector *collector, VerifyInstruction *current);

        std::optional< uint64_t > mem_idx(Operation *op) const;

        bool enable(Operation *op);

      private:

        template< typename MO >
        void init(Circuit *circuit, VerifyInstruction *current)
        {
            for (auto op : circuit->attr< MO >())
            {
                if (!collector->op_to_ctxs[op].count(current))
                    continue;

                auto idx = op->mem_idx();
                extend(idx);
                auto &[count, ops] = constraints[idx];
                ++count;
                ops.insert(op);
            }
        }

        void raise_level();

        bool do_enable(Operation *op, uint64_t mem_idx);
        void extend(uint32_t desired);

    };
} // namespace circ::run
