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
    namespace native
    {
        std::string Trace::to_string() const
        {
            static const auto fmt = [](const auto &what) -> std::string
            {
                if ( !what )
                    return "( none )";

                return llvm::toString( *what, 16, false );
            };

            static const auto fmt_inst_bits = []( const auto &what ) -> std::string
            {
                std::string out;
                for ( unsigned int i = 0; i < what.getBitWidth(); i += 8 )
                {
                    auto slice = what.extractBits( 8, i );
                    out += llvm::toString(slice, 16, false);
                    out += " ";
                }
                return out;
            };

            static const auto fmt_entry = [&]( auto key, const auto &what ) -> std::string
            {
                if ( !what )
                    return "( none )";

                if ( key == "instruction_bits" )
                    return fmt_inst_bits( *what );

                return fmt( what );
            };

            std::stringstream ss;
            ss << "Trace:\n"
               << "\tidx: " << id << "\n"
               << "\tInitial memory:\n";

            for (auto &[addr, val] : initial_memory)
                ss << "\t | [ " << std::hex << "0x" << addr << " ] <- "
                   << fmt(val) << "\n";
            ss << "\tEntries:\n";
            for (std::size_t i = 0; i < entries.size(); ++i)
            {
                ss << "\t" << i << "\n";
                for (const auto &[name, val] : entries[i])
                    ss << "\t | " << name << " = " << fmt_entry(name, val) << "\n";
            }

            return ss.str();

        }

    } // namespace native

} // namespace circ::run::trace
