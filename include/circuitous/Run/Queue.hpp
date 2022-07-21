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
    // Memory operations are ordered by `op->mem_idx()`. They are allowed to be interpreted in
    // levels - to interpret op on level `N + 1` first all operation with level lower than
    // `N + 1` must be resolved.
    // NOTE(lukas): In pure verification mode, this may be redundant - but it does not hurt
    //              correctness.
    struct MemoryOrdering
    {
        using mem_ops_t = std::unordered_set< Operation * >;
        // [count of snd, set of operations]
        using level_t = std::tuple< uint32_t, mem_ops_t >;
        using constraints_t = std::vector< level_t >;

      private:

        CtxCollector *collector;
        constraints_t constraints;

      public:
        uint32_t allowed = 0;

        MemoryOrdering(Circuit *circuit, CtxCollector *collector, VerifyInstruction *current);

        // Check what is memory index of given operation.
        std::optional< uint64_t > mem_idx(Operation *op) const;

        // Returns `true` if next level should be enabled.
        bool enable_next_level();
        void remove_constraint(Operation *op);

      private:

        // Initialise ordering for operation `MO` - usually for all memory constraints.
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

        void extend(uint32_t desired);
    };

    struct TodoQueue
    {
      private:
        std::deque< Operation * > todo;
        std::unordered_map< uint64_t, std::vector< Operation * > > waiting;
        std::unordered_map< Operation *, uint64_t > blocked;

        MemoryOrdering mem_order;

      public:
        TodoQueue(MemoryOrdering mem_order_) : mem_order(std::move(mem_order_)) {}
        TodoQueue(const TodoQueue &) = default;
        TodoQueue(TodoQueue &&) = default;

        TodoQueue& operator=(TodoQueue) = delete;

        Operation *pop()
        {
            auto x = todo.front();
            todo.pop_front();
            return x;
        }

        // Notify all users of `op` that satisfy predicate `p`.
        template< typename Predicate >
        void notify_from(Operation *op, Predicate &&p)
        {
            for (auto user : op->users())
                if (p(user))
                    notify(op, user);
        }

        // All users of `op` are notified.
        void notify_from(Operation *op)
        {
            return notify_from(op, [](const auto &){ return true; });
        }

        // Operation `from` was set and therefore `to` is notified.
        void notify(Operation *from, Operation *to);
        void notify_self(Operation *op) { notify(op, op); }

        bool empty() const { return todo.empty(); }

        std::string status(Operation *op);
      private:

        void push(Operation *op);
        void _notify(Operation *op);
    };

} // namespace circ::run
