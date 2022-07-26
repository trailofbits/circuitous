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
} // namespace circ::run
