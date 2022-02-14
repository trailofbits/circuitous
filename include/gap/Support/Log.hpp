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
#include <vector>

#include <gap/Support/SourceLocation.hpp>

// Really simple framework that is built on top of two concepts
//  * There are `Sink`s that are configurable & local per library/component etc.
//  * Severity levels are configured to print into one sink and can cause program
//    to abort if configured in that way.
// Program is expected to initialize sinks at the beginning (or at reasonable place for
// them not to became dangling references) - if no initialization happens, nothing is ever
// printed anywhere.
//
// Generic version of log method would look as follow
// `log< custom_severity, sink storage >() << "Logging some random stuff";`
//
// Users are encouraged to wrap this in handy wrappers e.g.
// `log< custom_severity >() << ... ;`
// `log_info() << ... ;`
//
// Initialization of sinks can be done via (severity can map to multiple sinks)
// `add_sink< sink storage, custom severity >( ... args )`
// Current implementation expects that `sink storage` is a class with `static`
// attribute that holds all sinks.
//
// Mechanism behind the logging is that `log()` return `Message` object which
// can take extra arguments via `<<` and in its dtor flushes content appropriately.
//
// Pros:
//  * Logging functions are scoped.
//  * No macros.
//  * Not dependent on any other framework and should not clash with others.
//
// Meh:
//  * Setup of custom severities.
//  * `log< ... >() << X()` will always invoke `X()` even if no logging happens.
//    If `X()` can be expensive, explicit macro or guard with `if` can be used instead,
//    but this utility does not provide any macros to avoid polluting global namespace.
//    NOTE(lukas): I don't know how to achieve that ^ does not happen without macro.
//
//  Cons:
//  * No thread-safety yet.
//  * No custom sinks yet.


namespace gap::log {

    // Helpers to define severity levels
    namespace severity {

        struct level
        {
            using id_t = uint32_t;
        };

        template< uint32_t I >
        struct make_level : level
        {
            static inline level::id_t id = I;
        };

    } // namespace severity


    // TODO(lukas): Eventually probably just make `Sink` an interface.
    struct Sink
    {
        std::ostream &out;
        Sink(std::ostream &out_) : out(out_) {}

        auto operator->() { return &out; }
    };

    struct Sinks
    {
        using sinks_t = std::vector< Sink >;
        using alert_id_t = severity::level::id_t;
        std::unordered_map< alert_id_t, sinks_t > storage;

        template< typename L, typename ... Args >
        Sinks &add(Args && ... args)
        {
            storage[L::id].push_back(Sink(std::forward< Args >(args) ...));
            return *this;
        }

        template< typename L >
        void log(const std::string &str)
        {
            for (auto &sink : storage[L::id])
                sink.out << str;
            for (auto &sink : storage[L::id])
                sink->flush();
        }
    };

    // Example of sink storage structure:
    // struct Registered { static inline Sinks sinks = {}; };

    template< typename SinkStorage, typename L, typename ...Args >
    void add_sink(Args && ...args) {
        SinkStorage::sinks.template add< L >(std::forward< Args >(args) ... );
    }

    namespace msg
    {
        // TODO(lukas): `abort()` vs `exit()`?
        // NOTE(lukas): I believe there can be some problems with `abort` not being
        //              marked as `noreturn` on some platforms, that's why unreachable
        //              is used as well.
        struct Dies
        {
            [[ noreturn ]] void do_kill() { std::abort(); __builtin_unreachable(); }
        };

        template< typename Derived >
        struct Message
        {
            using underlying_stream_t = std::ostringstream;
            underlying_stream_t ss;
            bool fst = true;

            Derived &emit_delim()
            {
                if (!fst) ss << " ";
                fst = false;
                return static_cast< Derived & >( *this );
            }

            template< typename T >
            Derived &operator<<( T &&t )
            {
                emit_delim();
                ss << std::forward< T >(t);
                return static_cast< Derived & >( *this );
            }

            Derived &location(std::string file, int line, std::string fn)
            {
                ss << std::filesystem::path(file).filename().c_str() << " - " << line << ": ";
                return static_cast< Derived & >( *this );
            }

            // `loc` does not have default value as this method is not supposed to be
            // called from top-level code.
            Derived &location( const source_location &loc )
            {
                ss << "[ "<< std::filesystem::path(loc.file()).filename().c_str()
                   << ":" << loc.line() << " (" << loc.function() << ") ] ~ ";
                return static_cast< Derived & >( *this );
            }
        };

        // Message is bound to a severity level (so it knows which sinks to match)
        template< typename L, typename Next >
        struct WithLevel : Next
        {
            using Next::Next;
            using Level = L;
        };

        // Bind sinks as well, so content can be dumped into them.
        template< typename Next >
        struct WithSinks : Next
        {
            using Level = typename Next::Level;
            Sinks &sinks;

            WithSinks(Sinks &sinks_) : Next(), sinks(sinks_) {}

            void finalize()
            {
                this->ss << "\n";
                sinks.log< Level >( this->ss.str() );
            }
        };

        template< typename L, typename Derived >
        using LogBase = WithSinks< WithLevel< L, Message< Derived > > >;

        // Two main message classes

        // Simple messages, logs content on destruction.
        template< typename L >
        struct LogMessage : LogBase< L, LogMessage< L > >
        {
            using parent_t = LogBase< L, LogMessage< L > >;
            using parent_t::parent_t;
            using parent_t::operator<<;

            ~LogMessage() { this->finalize(); }

        };

        // Logs content on destruction and tries to kill program.
        template< typename L >
        struct KillMessage : LogBase< L, KillMessage< L > >, Dies
        {
            using parent_t = LogBase< L, KillMessage< L > >;
            using parent_t::parent_t;
            using parent_t::operator<<;

            [[ noreturn ]] ~KillMessage()
            {
                this->finalize();
                this->do_kill();
            }
        };

    } // namespace msg

    template< typename L, typename SinkStorage >
    auto log() -> typename L::message_t
    {
        return { SinkStorage::sinks };
    }
} // namespace gap::log
