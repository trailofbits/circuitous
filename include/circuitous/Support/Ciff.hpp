/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */
#pragma once

#include <fstream>
#include <tuple>

#include <circuitous/Util/InstructionBytes.hpp>
#include <circuitous/Util/Logging.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <remill/Arch/Instruction.h>
CIRCUITOUS_UNRELAX_WARNINGS


namespace circ
{
    // CIF(F) - circuitous input file (format)
    struct CIFWriter
    {
        using self_t = CIFWriter;

      private:
        std::ofstream out;

      public:
        CIFWriter(const std::string &filepath) : out(filepath) {}

        explicit operator bool() const { return static_cast< bool >(out); }

        self_t &operator<<(const remill::Instruction &rinst)
        {
            out << InstBytes(rinst.bytes)
                << " " << rinst.function << std::endl;
            return *this;
        }

        void flush() { out.flush(); }
    };

    using bytes_and_iform_t = std::tuple< InstBytes, std::string >;
    using CIF = std::vector< bytes_and_iform_t >;

    struct CIFReader
    {
        using self_t = CIFReader;

      private:
        CIF cif;

      public:

        self_t &read(const std::string &filename)
        {
            std::ifstream in(filename);
            for (std::string line; std::getline(in, line);)
            {
                auto [bytes, iform] = llvm::StringRef(line).split(' ');
                cif.emplace_back(convert(bytes), iform.str());
            }
            return *this;
        }

        CIF  take() { return std::move(cif); }
        CIF &get_ref() { return cif; }

      private:
        InstBytes convert(llvm::StringRef text)
        {
            check(text.size() % 2 == 0) << "Trying to convert" << text.str() << "to InstBytes"
                                        << "but it's size is not aligned:" << text.size();
            InstBytes out;
            for (std::size_t i = 0; i < text.size(); i += 2)
            {
                std::string aux = { text[i], text[i + 1] };
                out.push_back(static_cast< char >(std::strtoul(aux.data(), nullptr, 16)));
            }
            return out;
        }
    };

} // namespace circ
