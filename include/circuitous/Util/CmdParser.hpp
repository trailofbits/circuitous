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

#include <circuitous/Util/TypeList.hpp>

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
                    ss << S::opt.primary << " does not allow option: " << token;
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

    template< typename T >
    constexpr bool is_cmd_opt() { return std::is_base_of_v< cmd_opt_tag_t, T >; }

    template< typename Self >
    struct Printable
    {
        auto dbg_str() const
        {
            std::stringstream ss;
            ss << "Matched results of parsing:\n";
            for (const auto &[lopt, tokens] : static_cast< const Self & >(*this).parsed)
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
    };

    // NOTE(lukas): To simplify we are dropping the `template< typename ... Ops >` here
    //              as it complicates code a lot and does not bring much upside.
    struct ParsedCmd : Printable< ParsedCmd >
    {
        using self_t = ParsedCmd;
        using tokens_t = std::vector< std::string >;
        using parse_map_t = std::unordered_map< std::string, tokens_t >;

        parse_map_t parsed;
        bool is_valid;

        ParsedCmd(parse_map_t parsed_, bool v) : parsed(std::move(parsed_)), is_valid(v) {}

        /** Additional validation - make extra middle class **/

        template< typename ... Ts >
        self_t& exactly_one_present(std::tuple< Ts ... >)
        {
            is_valid &= count_matched< Ts ... >() == 1;
            return *this;
        }

        template< typename H, typename ... Ts >
        uint64_t count_matched() const
        {
            uint64_t self = present< H >();
            if constexpr (sizeof ... (Ts) == 0) return self;
            else return self + count_matched< Ts ... >();
        }

        operator bool() const { return is_valid; }

        /** Access **/

        std::size_t size() const { return parsed.size(); }

        template< typename Cmd >
        std::optional< tokens_t > get_raw() const
        {
            auto it = parsed.find(Cmd::opt.primary);
            if (it == parsed.end())
                return {};

            return { it->second };
        }
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
        bool present() const { return present(Cmd::opt.primary); }
        bool present(const std::string &primary) const { return parsed.count(primary); }
    };


    // Statefull - empty on creation.
    // Call to `validate` invalidates the object and it is never ready to be used again.
    struct ValidationRule
    {
        using error_t = std::optional< std::string >;
        using parsed_t = ParsedCmd;

      protected:

        template< typename ... Ts >
        static std::vector< std::string > opts_to_primary()
        {
            std::vector< std::string > out;
            _opts_to_primary< Ts ... >(out);
            return out;
        }


        template< typename H, typename ... Ts >
        static void _opts_to_primary(std::vector< std::string > &out)
        {
            out.push_back(H::opt.primary);
            if constexpr (sizeof ... (Ts) != 0)
                return _opts_to_primary< Ts ... >(out);
        }

        template< typename ... Args >
        static std::vector< std::string > filter_present(const parsed_t &parsed)
        {
            auto cond = [](const auto &p, const auto &o) { return p.present(o); };
            // TODO(lukas): Will need manual specification.
            return _filter< Args ... >(parsed, std::move( cond ));
        }

        template< typename ... Args >
        static std::vector< std::string > filter_missing(const parsed_t &parsed)
        {
            auto cond = [](const auto &p, const auto &o) { return !p.present(o); };
            // TODO(lukas): Will need manual specification.
            return _filter< Args ... >(parsed, std::move( cond ));
        }

        template< typename ... Args, typename C >
        static std::vector< std::string > _filter(const parsed_t &parsed, C &&cond)
        {
            std::vector< std::string > filtered;
            for (const auto &opt : opts_to_primary< Args ... >())
                if (cond(parsed, opt))
                    filtered.push_back(opt);
            return filtered;
        }


        static std::string format_opts(const std::vector< std::string > &opts)
        {
            std::stringstream ss;
            ss << "[";
            for (const auto &opt : opts)
                ss << " " << opt;
            ss << " ]";
            return ss.str();
        }
    };

    template< typename Self >
    struct CountPresent : ValidationRule
    {
      protected:
        std::vector< std::string > filtered;

        Self &self() { return static_cast< Self & >(*this); }
        const Self &self() const { return static_cast< const Self & >(*this); }

        error_t do_validation()
        {
            if (self().satisfies())
                return {};
            return self().make_error();

        }

        template< typename ... Args > requires (sizeof ... (Args) != 0)
        error_t init_and_validate(const parsed_t &parsed)
        {
            filtered = filter_present< Args ... >(parsed);
            return do_validation();
        }

    };

    template< typename ... Args >
    struct Exclusive : CountPresent< Exclusive< Args ... > >
    {
        // It does not make sense to have less than 2 arguments as exclusive.
        static_assert( sizeof ... (Args) >= 2 );

        using parent_t = CountPresent< Exclusive< Args ... > >;
        using parsed_t = typename parent_t::parsed_t;
        using error_t = typename parent_t::error_t;

        error_t validate(const parsed_t &parsed)
        {
            return this->template init_and_validate< Args ... >(parsed);
        }

        bool satisfies() const { return this->filtered.size() <= 1; }

        error_t make_error() const
        {
            return "Following should be exclusive: " + parent_t::format_opts(this->filtered);
        }
    };


    template< uint64_t N, typename ... Args >
    struct ExactlyNOf : CountPresent< ExactlyNOf< N, Args ... > >
    {
        static_assert( sizeof ... (Args) >= N + 1 );

        using parent_t = CountPresent< ExactlyNOf< N, Args ... > >;
        using parsed_t = typename parent_t::parsed_t;
        using error_t = typename parent_t::error_t;

        error_t validate(const parsed_t &parsed) { return this->init_and_validate(parsed); }
        bool satisfies() const { return this->filtered.size() == N; }

        error_t make_error() const
        {
            return "Expected " + std::to_string(N) + " of: "
                               + parent_t::format_opts(this->filtered);
        }
    };

    template< typename L, typename ... Rhs >
    struct Implies : ValidationRule
    {
        static_assert( sizeof ... (Rhs) >= 1 );

        using parent_t = ValidationRule;
        using parsed_t = typename parent_t::parsed_t;
        using error_t = parent_t::error_t;

        error_t validate(const parsed_t &parsed)
        {
            // If left side is `false` there is no need to check.
            if (!parsed.template present< L >())
                return {};

            auto missing = this->parent_t::filter_missing< Rhs ... >();
            if (missing.size() == 0)
                return {};

            std::stringstream ss;
            ss << L::primary << " implies " << parent_t::format_opts(missing);
            return std::make_optional( ss.str() );
        }
    };

    template< typename ... Args >
    struct Present : ValidationRule
    {
        static_assert( sizeof ... (Args) >= 1 );

        using parent_t = ValidationRule;
        using parsed_t = typename parent_t::parsed_t;
        using error_t = typename parent_t::error_t;

        error_t validate(const parsed_t &parsed)
        {
            auto missing = this->parent_t::filter_missing< Args ... >();
            if (missing.size() == 0)
                return {};
            return "Missing: " << parent_t::format_opts(missing);
        }
    };

    template< typename T >
    struct IsSingleton : ValidationRule
    {
        using parent_t = ValidationRule;
        using parsed_t = typename parent_t::parsed_t;
        using error_t = typename parent_t::error_t;

        error_t validate(const parsed_t &parsed)
        {
            if (!parsed.template present< T >() || parsed.size() == 1)
                return {};

            return "Expected " + T::opt.primary + " to be a singleton option.";
        }
    };

    template< typename ... Args > struct are_exclusive : Exclusive< Args ... > {};
    template< typename ... Args >
    struct are_exclusive< tl::TL< Args ... > > : Exclusive< Args ... > {};

    template< typename L, typename ... Rhs >
    auto implies() { return Implies< L, Rhs ... >(); }

    template< typename ... Args >
    auto one_of() { return ExactlyNOf< 1ul, Args ... >(); }

    template< typename ... Args >
    auto is_present() { return Present< Args ... >(); }

    template< typename T >
    auto is_singleton() { return IsSingleton< T >(); }

    struct Validator
    {
        using self_t = Validator;
        using error_t = std::string;
        using errors_t = std::vector< error_t >;

        errors_t errs;
        const ParsedCmd &parsed;

        Validator( const ParsedCmd &parsed_ ) : parsed( parsed_ ) {}
        Validator spawn() { return Validator( *this ); }

        bool has_errors() const { return !errs.empty(); }
        errors_t take() { return std::move(errs); }

        template< typename Yield >
        bool process_errors(Yield &&yield)
        {
            for (const auto &err : errs)
                yield(err);
            if (!has_errors())
                return false;

            errs.clear();
            return true;
        }

        template< typename C, typename ... Cs >
        self_t &check( C &&c, Cs &&... cs )
        {
            run_check(std::forward< C >(c));
            if constexpr (sizeof ... (Cs) != 0)
                return check(std::forward< Cs >(cs) ...);
            else
                return *this;
        }

        template< typename ... Ts >
        self_t &validate_leaves(tl::TL< Ts ... >)
        {
            return do_validate_leaves< Ts ... >();
        }

      private:

        template< typename H, typename ... Ts > requires (is_cmd_opt< H >())
        self_t &do_validate_leaves()
        {
            if constexpr (Validates< H >)
                if (auto tokens = parsed.get_raw< H >())
                    if (auto msg = H::validate(*tokens))
                        errs.push_back(std::move(*msg));

            if constexpr (sizeof ... (Ts) == 0)
                return *this;
            else
                return do_validate_leaves< Ts ... >();
        }

        template< typename C >
        void run_check(C &&c)
        {
            if (auto err = c.validate(parsed))
                errs.push_back(std::move(*err));
        }


    };

    template< typename ... Cmds >
    struct CmdParser_impl : Printable< CmdParser_impl< Cmds ... > >
    {
        using cmd_ts = tl::TL< Cmds ... >;
      private:
        using self_t = CmdParser_impl< Cmds ... >;

        using tokens_t = std::vector< std::string >;
        using tokens_view_t = const tokens_t &;

        using parse_map_t = std::unordered_map< std::string, tokens_t >;

        parse_map_t parsed;

        std::size_t current = 0;
        tokens_t tokens;

      public:

        static ParsedCmd parse_argv(int argc, char **argv)
        {
            self_t parser;
            for (int i = 1; i < argc; ++i)
                parser.tokens.push_back(argv[i]);
            parser.match_opt();
            return { std::move(parser.parsed), true };
        }

      private:

        /** Access **/

        template< typename Cmd >
        std::optional< tokens_t > get_raw() const
        {
            auto it = parsed.find(Cmd::opt.primary);
            if (it == parsed.end())
                return {};

            return { it->second };
        }

        /** Parsing **/

        void parse(std::string_view str)
        {
            tokens = tokenize(str);
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

        template< typename Cmd >
        bool matched() const { return present< Cmd >(); }

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

    template< typename ... Ts >
    struct CmdParser : CmdParser_impl< Ts ... > {};

    template< typename ... Ts >
    struct CmdParser< tl::TL< Ts ... > > : CmdParser_impl< Ts ... > {};

} // namespace circ
