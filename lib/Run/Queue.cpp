/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/IR/Circuit.hpp>

#include <circuitous/Run/Queue.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/ADT/APInt.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ::run
{
    void MemoryOrdering::extend(uint32_t desired)
    {
        if (desired < constraints.size())
            return;
        constraints.resize(desired + 1);
    }

    void MemoryOrdering::raise_level()
    {
        ++allowed;
    }

    bool MemoryOrdering::do_enable(Operation *op, uint64_t mem_idx)
    {
        // mem_idx must be on current level
        if (mem_idx != allowed)
            return false;
        auto &[count, ops] = constraints[mem_idx];
        if (!ops.count(op))
            return false;

        --count;
        ops.erase(op);
        // There are still operations on this level
        if (count != 0)
            return false;

        // There are no more operations on this level -> raise it.
        raise_level();
        return true;
    }

    std::optional< uint64_t > MemoryOrdering::mem_idx(Operation *op) const
    {
        if (!is_one_of<ReadConstraint, WriteConstraint>(op))
            return {};

        if (auto x = dynamic_cast< WriteConstraint * >(op)) return x->mem_idx();
        if (auto x = dynamic_cast< ReadConstraint * >(op)) return x->mem_idx();
        unreachable() << "Unreachable";
    }

    bool MemoryOrdering::enable(Operation *op)
    {
        if (auto mi = mem_idx(op))
            return do_enable(op, *mi);
        return false;
    }

    MemoryOrdering::MemoryOrdering(Circuit *circuit,
                                   CtxCollector *collector,
                                   VerifyInstruction *current)
        : collector(collector)
    {
        init< WriteConstraint >(circuit, current);
        init< ReadConstraint >(circuit, current);
    }


    std::string TodoQueue::status(Operation *op)
    {
        std::stringstream ss;
        if (blocked.count(op))
            ss << "[ " << blocked[op] << " / " << op->operands.size() << "]";
        else
            ss << " [ NOT SET ]";
        return ss.str();
    }

    void TodoQueue::push(Operation *op)
    {
        // TODO(lukas): What about `UnusedConstraint`.
        if (!is_one_of< ReadConstraint, WriteConstraint >(op))
            return todo.push_back(op);

        // Fetch ordering id for this memory op.
        auto mem_idx = mem_order.mem_idx(op);
        check(mem_idx);

        if (*mem_idx > mem_order.allowed)
        {
            waiting[*mem_idx].push_back(op);
            return;
        }

        if (*mem_idx == mem_order.allowed)
            todo.push_back(op);

        if (!mem_order.enable(op))
            return;

        for (auto x : waiting[mem_order.allowed])
            push(x);
        waiting[mem_order.allowed].clear();
    }

    // General notify that does no extra work
    void TodoQueue::notify(Operation *from, Operation *to)
    {
        _notify(to);
        log_dbg() << pretty_print< false >(from) << " -- notifies --> "
                  << pretty_print< false >(to) << status(to);
    }

    // Implementation
    void TodoQueue::_notify(Operation *op)
    {
        auto [it, inserted] = blocked.emplace(op, op->operands.size());
        if (it->second <= 1) {
            push(it->first);
            it->second = 0;
            return;
        }
        --it->second;
    }

    void TodoQueue::notify_mem(Operation *op)
    {
        if (!mem_order.enable(op))
            return;

        for (auto x : waiting[mem_order.allowed])
            push(x);
        waiting[mem_order.allowed].clear();
    }

} // namespace circ::run
