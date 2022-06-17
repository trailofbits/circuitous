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
        // [count of snd, set of operations]
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

        template< typename Predicate >
        void notify_from(Operation *op, Predicate &&p)
        {
            for (auto user : op->users)
                if (p(user))
                    notify(op, user);
        }

        void notify_from(Operation *op)
        {
            return notify_from(op, [](const auto &){ return true; });
        }

        void notify(Operation *from, Operation *to);
        void notify_self(Operation *op) { notify(op, op); }

        bool empty() const { return todo.empty(); }

        std::string status(Operation *op);
      private:

        void push(Operation *op);
        void _notify(Operation *op);
        void notify_mem(Operation *op);
    };

} // namespace circ::run
