/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Run/Trace.hpp>

#include <circuitous/IR/Circuit.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/ADT/APInt.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ::run::trace
{
    value_type native::Entry::get_mem_hint(const std::string &key) const
    {
        // TODO(lukas): I do not want to include from `IR` here.
        //              It would probably help to pull out all constants into
        //              separate lightweight header.
        auto it = mem_hints.find(key);
        if (it == mem_hints.end()) {
            return {};
        }
        return it->second;
    }
} // namespace circ::run::trace
