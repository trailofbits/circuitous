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
    std::string Entry::to_string(uint8_t indent, bool skip_header) const
    {
        std::stringstream ss;
        std::string prefix(indent * 2, ' ');

        if (!skip_header) {
            ss << prefix << "trace_id: " << trace_id << std::endl;
            ss << prefix << "......" << std::endl;
        }
        ss << prefix << "timestamp: " << timestamp << std::endl;
        ss << prefix << "ibits: " << inst_bits << std::endl;
        ss << prefix << "ebits: " << ebit << std::endl;
        ss << prefix << "regs:" << std::endl;
        for (const auto &[reg, val] : regs) {
            ss << prefix << "  |- " << reg << " -> " << val << std::endl;
        }
        return ss.str();
    }

    std::optional< llvm::APInt > Entry::get_mem_hint(const std::string &key) const
    {
        // TODO(lukas): I do not want to include from `IR` here.
        //              It would probably help to pull out all constants into
        //              separate lightweight header.
        auto it = mem_hints.find(key);
        if (it == mem_hints.end()) {
            return {};
        }
        return { llvm::APInt(208, it->second, 10) };
    }

    llvm::APInt Entry::get_inst_bits(uint32_t size) const
    {
        std::string reoredered;
        check(inst_bits.size() >= 2);
        for (int i = static_cast< int >(inst_bits.size() - 2); i >= 0; i -= 2)
        {
            reoredered += inst_bits.substr(static_cast< unsigned long >(i), 2);
        }
        return llvm::APInt(size, reoredered, /* radix = */ 16U);
    }

} // namespace circ::run::trace
