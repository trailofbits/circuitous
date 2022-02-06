/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <iomanip>
#include <string>
#include <sstream>

#include <circuitous/Support/Check.hpp>

namespace circ
{
    // Just a wrapper to provide
    //  * common operations we need to do often on instruction bytes
    //  * separate type to avoid confusion with `std::string`.
    struct InstBytes
    {
        using self_t = InstBytes;
        using underlying_t = std::string;

        underlying_t data;

        InstBytes() = default;
        explicit InstBytes(std::string data_) : data(std::move(data_)) {}

        std::string as_hex_str() const
        {
            std::stringstream ss;
            ss << std::setw(2) << std::setfill('0') << std::hex;
            for (auto c : data)
                ss << static_cast< unsigned >(static_cast< uint8_t >(c));
            return ss.str();
        }

        const underlying_t &raw() const { return data; }

        const auto &push_back(char what)
        {
            data.push_back(what);
            return data.back();
        }

        template< uint64_t N >
        std::bitset< N > to_enc() const
        {
            std::bitset< N > out;
            std::size_t i = 0;
            for (char byte_ : underlying_t(data.rbegin(), data.rend()))
            {
                const auto byte = static_cast< uint8_t >(byte_);
                for (auto b = 0u; b < 8u; ++b, ++i)
                    if ((byte >> b) & 1u)
                        out.set(i);
            }
            return out;
        }
    };

    // TODO(lukas): Make generic and make sure it compiles properly.
    static inline std::ostream &operator<<(std::ostream &os, const InstBytes &bytes)
    {
        auto str = bytes.as_hex_str();
        // TODO(lukas): I guess you wonder why is there this conversion to string_view. Well,
        //              without it it won't compile on same configurations.
        os << std::string_view(str);
        return os;
    }
}
