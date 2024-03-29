/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <cassert>
#include <filesystem>
#include <iostream>
#include <sstream>
#include <string>

#include <gap/Support/Log.hpp>
#include <gap/core/source_location.hpp>

namespace gap {

    template< typename Derived >
    struct CheckBaseMessage : log::msg::Message< Derived >
    {
        using Base = log::msg::Message< Derived >;

        std::ostream &os;
        CheckBaseMessage(std::ostream &os_, const source_location &loc)
            : os(os_)
        {
            this->Base::location(loc);
        }

        [[ noreturn ]] void kill()
        {
            abort();
            __builtin_unreachable();
        }

        [[ noreturn ]] void do_kill(std::string_view extra = "")
        {
            this->ss << extra << "\n";
            os << this->ss.str();
            os.flush();
            kill();
        }
    };

    struct TerminatingMessage : CheckBaseMessage< TerminatingMessage >
    {
        using Base = CheckBaseMessage< TerminatingMessage >;
        using Base::Base;

        using Base::operator<<;

        [[ noreturn ]] ~TerminatingMessage() { this->Base::do_kill(); }
    };

    struct CheckMessage : CheckBaseMessage< CheckMessage >
    {
        using Base = CheckBaseMessage< CheckMessage >;

        bool failed;

        CheckMessage( std::ostream &os_, const source_location &loc_, bool condition )
            : Base(os_, loc_), failed(!condition)
        {}

        ~CheckMessage()
        {
            if (failed)
                this->Base::do_kill();
        }

        template< typename T >
        CheckMessage& operator<<( T &&t )
        {
            // No need to write in case nothing is gonna get printed.
            if (failed)
                this->Base::operator<<( std::forward< T >(t) );
            return *this;
        }
    };


    // TODO(lukas): I am not sold on names, `check` is kind of name I do not want
    //              to take. Keeping it uppercase for now to make transition easier.
    [[ noreturn ]] static inline void not_implemented(
            source_location loc = source_location::current())
    {
        TerminatingMessage(std::cerr, std::move(loc)).do_kill("not implemented");
    }

    void check(auto &&condition, auto msg_gen, source_location loc = source_location::current())
    {
        if (!condition)
        {
            CheckMessage(std::cerr, std::move(loc), static_cast< bool >(condition))
                    << msg_gen();
        }

    }

    auto check(auto &&condition, source_location loc = source_location::current())
    -> CheckMessage
    {
        return { std::cerr, std::move( loc ), static_cast< bool >(condition) };
    }

    // TODO(lukas): Turn off in non-debug build.
    void dcheck(auto &&condition, auto msg_gen,
                source_location loc = source_location::current())
    {
        if (!condition)
        {
            CheckMessage(std::cerr, std::move(loc), static_cast< bool >(condition))
                    << msg_gen();
        }

    }


    static inline auto unreachable(source_location loc = source_location::current())
    -> TerminatingMessage
    {
        return TerminatingMessage( std::cerr, std::move( loc ) );
    }
} // namespace gap
