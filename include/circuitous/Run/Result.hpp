/*
 * Copyright (c) 2020-2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Run/Base.hpp>
#include <circuitous/Run/Spawn.hpp>

#include <circuitous/Support/Check.hpp>

namespace circ::run
{
    enum class result_t : uint32_t
    {
        accepted = 0,
        rejected,
        not_decoded,
        multiple_ctx_satisfied,
        runtime_error,
        value_not_reached,
    };

    std::string to_string(result_t raw)
    {
        switch(raw)
        {
            case result_t::accepted : return "accepted";
            case result_t::rejected : return "rejected";
            case result_t::not_decoded : return "not decoded";
            case result_t::multiple_ctx_satisfied : return "multiple_ctx_satisfied";
            case result_t::value_not_reached: return "value not reached";
            case result_t::runtime_error : return "runtime_error";
        }
    }

    bool accepted(result_t raw)
    {
        return raw == result_t::accepted;
    }

    bool rejected(result_t raw)
    {
        switch(raw)
        {
            case result_t::rejected:
            case result_t::not_decoded: return true;
            default: return false;
        }
    }

    bool error(result_t raw)
    {
        switch(raw)
        {
            case result_t::multiple_ctx_satisfied:
            case result_t::value_not_reached:
            case result_t::runtime_error: return true;
            default: return false;
        }
    }

}  // namespace circ::run
