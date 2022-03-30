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

    std::string enc_to_str(const auto &enc)
    {
        std::string out;
        for (std::size_t i = 0; i < enc.size(); ++i)
            out += (enc[i]) ? '1' : '0';
        return out;
    }

    static inline std::string inst_bytes_as_str(const std::string &str)
    {
        std::stringstream ss;
        for (std::size_t i = 0; i < str.size();)
        {
            dcheck(str[i] == '0' || str[i] == '1', []() { return "Unexpected chars."; });
            ss << str[i];

            ++i;
            if (i % 8 == 0 && i < str.size())
                ss << " ";
        }
        return ss.str();

    }

    template< unsigned long N > requires (N % 8 == 0)
    static inline std::string inst_bytes_as_str(const std::bitset< N > &enc)
    {
        return inst_bytes_as_str(enc_to_str(enc));
    }

    static inline std::string inst_bytes_as_str(const std::vector< bool > &enc)
    {
        return inst_bytes_as_str(enc_to_str(enc));
    }

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
            for (auto c : data)
                ss << std::setw(2) << std::setfill('0') << std::hex
                   << static_cast< unsigned >(static_cast< uint8_t >(c));
            return ss.str();
        }

        const underlying_t &raw() const { return data; }

        const auto &push_back(char what)
        {
            data.push_back(what);
            return data.back();
        }

        template< uint64_t N > requires (N % 8 == 0)
        std::bitset< N > to_enc() const
        {
            std::bitset< N > out;
            dcheck(N >= data.size(), [&]() { return "to_enc cannot hold entire data!"; });
            std::size_t i = 0;
            for (char byte_ : data)
            {
                const auto byte = static_cast< uint8_t >(byte_);
                for (int b = 0; b < 8; ++b, ++i)
                {
                    dcheck( i < N, [&](){
                        std::stringstream out;
                        out << "Trying to write to bitset at " << i
                            << " whereas max size is " << N << "\n";
                        return out.str();
                    });

                    if (byte & (1u << b))
                        out.set(i);
                }
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
