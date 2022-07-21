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

    void MemoryOrdering::remove_constraint(Operation *op)
    {
        auto idx = mem_idx(op);
        check(idx);
        auto &[count, ops] = constraints[*idx];
        check(ops.count(op));

        --count;
        ops.erase(op);
    }

    std::optional< uint64_t > MemoryOrdering::mem_idx(Operation *op) const
    {
        if (!is_one_of<ReadConstraint, WriteConstraint>(op))
            return {};

        if (auto x = dynamic_cast< WriteConstraint * >(op)) return x->mem_idx();
        if (auto x = dynamic_cast< ReadConstraint * >(op)) return x->mem_idx();
        unreachable() << "Unreachable";
    }

    bool MemoryOrdering::enable_next_level()
    {
        auto &[count, ops] = constraints[allowed];
        if (count == 0)
            raise_level();
        return count == 0;
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
            ss << "[ " << blocked[op] << " / " << op->operands().size() << "]";
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

        if (mem_idx < mem_order.allowed)
            return;

        if (*mem_idx > mem_order.allowed)
        {
            waiting[*mem_idx].push_back(op);
            return;
        }

        todo.push_back(op);
        mem_order.remove_constraint(op);
        if (mem_order.enable_next_level())
        {
            for (auto x : waiting[mem_order.allowed])
                push(x);
            waiting[mem_order.allowed].clear();
        }

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
        auto [it, inserted] = blocked.emplace(op, op->unique_operands_count());
        if (it->second <= 1) {
            push(it->first);
            it->second = 0;
            return;
        }
        --it->second;
    }

} // namespace circ::run
