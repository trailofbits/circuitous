/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <cassert>
#include <filesystem>
#include <sstream>
#include <string>
#include <filesystem>
#include <unordered_map>
#include <unordered_set>

#include <gap/Support/SourceLocation.hpp>
#include <gap/Support/Log.hpp>

namespace circ
{
    namespace severity
    {
        template< uint32_t I >
        using make_level = gap::log::severity::make_level< I >;

        template< typename Severity >
        using LogMessage = gap::log::msg::LogMessage< Severity >;

        template< typename Severity >
        using KillMessage = gap::log::msg::KillMessage< Severity >;

        struct kill :  make_level< 0 > { using message_t = KillMessage< kill >; };
        struct error : make_level< 1 > { using message_t = LogMessage< error >; };
        struct warn :  make_level< 2 > { using message_t = LogMessage< warn >; };
        struct info :  make_level< 3 > { using message_t = LogMessage< info >; };
        struct dbg  :  make_level< 4 > { using message_t = LogMessage< dbg >; };
        struct trace  :  make_level< 5 > { using message_t = LogMessage< trace >; };

    } // namespace severity

    // We need to store references to all storages after all.
    struct RegisteredSinks { static inline gap::log::Sinks sinks = {}; };

    template< typename L >
    auto log() -> typename L::message_t
    {
        return gap::log::log< L, RegisteredSinks >();
    }

    template< typename L, typename ... Args >
    void add_sink(Args &&...args)
    {
        gap::log::add_sink< RegisteredSinks, L >( std::forward< Args >(args)... );
    }

    static inline auto log_info() { return log< severity::info >(); }
    static inline auto log_dbg() { return log< severity::dbg >(); }
    static inline auto log_error() { return log< severity::error >(); }
    static inline auto log_kill() { return log< severity::kill >(); }


    struct Tracers
    {
        static inline bool trace_all = false;
        static inline std::unordered_set< std::string > allowed;

        static bool should_trace(gap::source_location loc = gap::source_location::current()) {
            if (trace_all)
                return true;
            return allowed.count(std::string(loc.file()));
        }
    };

    static inline auto trace()
    {
        return gap::log::log< severity::trace, RegisteredSinks >();
    }

    #define CIRC_TRACE() \
        if (Tracers::should_trace()) trace().location(gap::source_location::current())

} // namespace circ
