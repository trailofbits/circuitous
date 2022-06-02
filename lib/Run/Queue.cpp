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
        check(mem_idx == allowed);
        auto &[count, ops] = constraints[mem_idx];
        if (!ops.count(op))
            return false;

        --count;
        ops.erase(op);
        if (count != 0)
            return false;

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


    std::string State::status(Operation *op)
    {
        std::stringstream ss;
        ss << "[ " << blocked[op] << " / " << op->operands.size() << "]";
        return ss.str();
    }

    void State::push(Operation *op)
    {
        if (!is_one_of<ReadConstraint, WriteConstraint>(op))
            return todo.push_back(op);
        auto mem_idx = mem_order.mem_idx(op);
        if (*mem_idx == mem_order.allowed)
            return todo.push_back(op);
        waiting[*mem_idx].push_back(op);
    }

    // Verbose notification for debug purposes
    void State::notify_verbose(Operation *from, Operation *to)
    {
        auto dbg_info = [&](auto from, auto to) {
            auto tail = [&]() -> std::string
            {
                if (blocked.count(to))
                    return std::to_string(blocked[to]);
                return "(unknown)";
            }();

            log_dbg() << pretty_print< false >(from) << " -- notifies -> "
                      << pretty_print< false >(to)
                      << "which is blocked by: [ " << tail << " / "
                      << to->operands.size() << " ].";
        };
        notify(from, to);
        dbg_info(from, to);
    }

    // General notify that does no extra work
    void State::notify(Operation *from, Operation *to)
    {
        return _notify(to);
    }

    // Implementation
    void State::_notify(Operation *op)
    {
        auto [it, inserted] = blocked.emplace(op, op->operands.size());
        if (it->second <= 1) {
            push(it->first);
            it->second = 0;
            return;
        }
        --it->second;
    }

    void State::notify_mem(Operation *op)
    {
        if (!mem_order.enable(op))
            return;

        for (auto x : waiting[mem_order.allowed])
            push(x);
        waiting[mem_order.allowed].clear();
    }

    void State::notify_from(Operation *op)
    {
        for (auto user : op->users)
            notify(op, user);
        notify_mem(op);
    }
} // namespace circ::run
