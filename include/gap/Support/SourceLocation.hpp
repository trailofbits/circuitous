/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <cassert>
#include <filesystem>
#include <sstream>
#include <string>
#include <string_view>
#include <filesystem>
#include <unordered_map>

// TODO(lukas): Replace by `std::source_location` where supported and once supported
//              universally remove this implementation.
namespace gap
{
    struct source_location
    {
        std::string_view _file;
        std::string_view _function;
        unsigned _line;
        unsigned _column;

        constexpr source_location(std::string_view file_, std::string_view fn_,
                                  unsigned line_, unsigned col_) noexcept
            : _file( file_ ), _function( fn_ ), _line( line_ ), _column( col_ )
        {}

        constexpr auto file() const noexcept     { return _file; }
        constexpr auto function() const noexcept { return _function; }
        constexpr auto line() const noexcept     { return _line; }
        constexpr auto column() const noexcept   { return _column; }


        static constexpr source_location current(
                std::string_view file = __builtin_FILE(),
                std::string_view fn = __builtin_FUNCTION(),
                unsigned line = __builtin_LINE(),
                unsigned col = __builtin_COLUMN()) noexcept
        {
            return source_location(file, fn, line, col);
        }
    };

    auto operator<<(auto &os, const source_location &loc) -> decltype(os << "")
    {
        // TODO(lukas): Implement.
        return os;
    }
} // namespace gap
