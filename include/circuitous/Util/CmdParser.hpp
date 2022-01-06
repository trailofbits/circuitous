/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <cassert>
#include <concepts>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <optional>
#include <type_traits>
#include <sstream>
#include <string>
#include <unordered_set>
#include <unordered_map>
#include <vector>

namespace circ
{
    struct cmd_opt_tag_t {};

    struct DefaultCmdOpt : cmd_opt_tag_t {};

    struct CmdOpt
    {
        using aliases_t = std::unordered_set< std::string >;

        const std::string primary;
        const aliases_t aliases;
        const bool required;

        CmdOpt(const std::string &primary_, const aliases_t &aliases_ , bool req_)
            : primary(primary_), aliases(aliases_), required(req_)
        {}

        CmdOpt(const std::string &primary_, bool req_)
            : CmdOpt(primary_, {}, req_)
        {}

        bool matches(const std::string &what) const
        {
            return what == primary || aliases.count(what);
        }
    };

    template< typename S >
    struct HasAllowed
    {
        static std::optional< std::string > validate(const std::vector< std::string > &tokens)
        {
            std::stringstream ss;
            for (auto &token : tokens)
                if (!S::allowed.count(token))
                    ss << token << " is not allowed.\n";
            if (auto str = ss.str(); !str.empty())
                return { std::move(str) };
            return {};
        }
    };

    template< typename Cmd > requires (std::is_base_of_v< cmd_opt_tag_t, Cmd >)
    std::string to_string()
    {
        return "[ " + Cmd::opt.primary + " ]";
    }

    template< typename T >
    concept Validates = requires( T a ) {
        { a.validate( std::vector< std::string >{} ) }
            -> std::same_as< std::optional< std::string > >;
    };

    template< typename ... Cmds >
    struct CmdOpts
    {
        using tokens_t = std::vector< std::string >;
        using tokens_view_t = const tokens_t &;

        using parse_map_t = std::unordered_map< std::string, tokens_t >;

        parse_map_t parsed;

        std::size_t current = 0;
        tokens_t tokens;

        /** Access **/

        // TODO(lukas): See if the `-> decltype( ... )` can be removed
        template< typename Cmd >
        auto get() const -> decltype( Cmd::cast( {} ) )
        {
            if (auto raw = get_raw< Cmd >())
                return Cmd::cast( std::move(*raw) );
            return {};
        }

        template< typename Cmd >
        auto get_or_die() -> decltype( *get< Cmd >() )
        {
            if (auto box = get< Cmd >())
                return std::move( *box );
            std::cerr << "Failed on get_or_die< "<< to_string< Cmd >() << ">\n";
            std::exit(1);
        }

        template< typename Cmd >
        std::optional< tokens_t > get_raw() const
        {
            auto it = parsed.find(Cmd::opt.primary);
            if (it == parsed.end())
                return {};

            return { it->second };
        }

        template< typename Cmd >
        bool matched() const
        {
            return present< Cmd >();
        }

        /** Debugging/Visualization **/

        std::string dbg_str() const
        {
            std::stringstream ss;
            ss << "Matched results of parsing:\n";
            for (const auto &[lopt, tokens] : parsed)
            {
                ss << " * " << lopt;
                if (tokens.empty())
                {
                    ss << "\n";
                    continue;
                }
                ss << " ->\n";
                for (const auto &token : tokens)
                    ss << "    " << token << "\n";
            }
            return ss.str();
        }

        /** Validation **/

        // Validate the parsed result
        //  * required opts must be present
        //  * opts that are present must be valid if such term is defined for them
        //    (i.e. they have `validate` method)
        // In case problems are found `yield` is called with `(primary, error_msg)`
        // and false is returned.
        // NOTE(lukas): `yield` can (and very often will) have state. Watch out for copies.
        template< typename Yield >
        bool validate(Yield &&yield)
        {
            return validate< Yield, Cmds ... >(yield);
        }

        template< typename Yield, typename H, typename ... Ts >
        bool validate(Yield &yield)
        {
            auto current = validate_one< Yield, H >(yield);
            if constexpr ( sizeof ... (Ts) != 0 )
                return current && validate< Yield, Ts ... >(yield);
            else
                return current;

        }

        template< typename Yield, typename Cmd >
        bool validate_one(Yield &yield)
        {
            auto report_fail = [&](auto msg) {
                yield(Cmd::opt.primary, msg);
                return false;
            };

            if (!matched< Cmd >() && Cmd::opt.required)
                return report_fail("Required but not present.");

            // It is not present, therefore nothing else needs to be checked
            if (!matched< Cmd >())
                return true;

            if constexpr (Validates< Cmd >)
                if (auto msg = Cmd::validate(*get_raw< Cmd >()))
                    return report_fail("Validate failed: " + *msg);
            return true;
        }

        /** Parsing **/

        void parse(std::string_view str)
        {
            tokens = tokenize(str);
            match_opt();
        }

        void parse_argv(int argc, char **argv)
        {
            for (int i = 1; i < argc; ++i)
                tokens.push_back(argv[i]);
            match_opt();
        }

        bool eof() { return current >= tokens.size(); }
        const auto &next() { return tokens[ current++ ]; }

        const std::string &peek()
        {
            if (eof())
                fail("Out of bonds read");
            return tokens[current];
        }


        template< typename H, typename ... Ts >
        void match_on(auto &&f)
        {
            if ( f( H{}, peek() ) )
                return parse_vals< H >();
            if constexpr (sizeof ... (Ts) != 0)
                return match_on< Ts ... >( std::forward< decltype(f) >( f ) );
            else
                fail("Unrecognized option " + peek());
        }

        auto compare_opt() { return [](auto x, auto y) { return x.opt.matches( y ); }; }

        template< typename Cmd >
        void account() { parsed[std::string(Cmd::opt.primary)] = {}; }

        template< typename Cmd >
        void account(const std::string &t) { parsed[Cmd::opt.primary].push_back(t); }

        template< typename Cmd >
        bool present() const { return parsed.count(Cmd::opt.primary); }

        template< typename Cmd, typename P >
        void take_while(P &&pred)
        {
            // We are either at the end, or next one is no longer valid
            if (eof() || !pred(peek()))
                return;
            account< Cmd >(next());
            return take_while< Cmd >(std::forward< P >(pred));
        }

        template< typename Cmd >
        void take_n(int8_t count)
        {
            if (count <= 0)
                return;

            if (matches_any< Cmds ... >(peek()))
            {
                fail(to_string< Cmd >() + " expected " + std::to_string( count )
                     + " more args, instead matched"
                     + peek());
            }

            account< Cmd >(next());
            return take_n< Cmd >(count - 1);

        }

        template< typename H, typename ... Tail >
        bool matches_any(const auto &token)
        {
            if (H::opt.matches(token))
                return true;
            if constexpr (sizeof ... (Tail) == 0)
                return false;
            else
                return matches_any< Tail ... >(token);
        }

        template< typename Cmd >
        void parse_vals()
        {
            if (present< Cmd >())
                fail( to_string< Cmd >() + " is present more than once" );

            next();

            if (Cmd::arity == 0)
                return account< Cmd >();
            if (Cmd::arity < 0)
                return take_while< Cmd >([&](const auto &token) {
                        return !matches_any< Cmds ... >(token);
                });
            return take_n< Cmd >(Cmd::arity);
        }

        void match_opt()
        {
            if (peek()[0] == '-')
                match_on< Cmds ... >(compare_opt());
            else
                fail("Could not match opt " + peek());

            if (eof())
                return;

            return match_opt();
        }

        void fail(const std::string &msg)
        {
            // NOTE(lukas): Print is done on stderr since some other logging
            //              may depend on parsed flags.
            std::cerr << msg << std::endl;
            std::exit(1);
        }

        tokens_t tokenize(std::string_view str)
        {
            tokens_t out;
            std::stringstream ss(std::string{str});
            for (std::string token; std::getline(ss, token, ' ');)
            {
                if (token.empty())
                    continue;
                out.push_back( std::move( token ) );
            }
            return out;
        }
    };
} // namespace circ
