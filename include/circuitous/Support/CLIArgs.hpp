/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Util/CmdParser.hpp>

namespace circ::cli
{
    template< typename Self >
    struct derive_short_help
    {
        static std::string short_help() { return Self::help(); }
    };

    template< int8_t arity_ >
    struct Arity { static inline int8_t arity = arity_; };

    template< typename T > struct As{};

    template<>
    struct As< std::string >
    {
        using tokens_t = std::vector< std::string >;
        static std::optional< std::string > cast(tokens_t tokens)
        {
            return (tokens.size() == 1)
                ? std::make_optional( std::move( *tokens.begin() ) )
                : std::nullopt;
        }

        static std::optional< std::string > validate(const tokens_t &tokens)
        {
            if (tokens.size() == 1)
                return {};
            std::stringstream ss;
            ss << "Expected 1 argument instead got " << tokens.size();
            return std::make_optional( ss.str() );
        }
    };

    struct PathArg : Arity< 1 >, As< std::string > {};

    struct SMTOut : circ::DefaultCmdOpt, PathArg
    {
        static inline const auto opt = circ::CmdOpt("--smth-out", false);
    };

    struct JsonOut : circ::DefaultCmdOpt, PathArg
    {
        static inline const auto opt = circ::CmdOpt("--json-out", false);
    };

    struct VerilogOut : circ::DefaultCmdOpt, PathArg
    {
        static inline const auto opt = circ::CmdOpt("--verilog-out", false);
    };

    struct BitBlastSmtOut : circ::DefaultCmdOpt, PathArg
    {
        static inline const auto opt = circ::CmdOpt("--bitblast-smt-out", false);
    };

    struct DotOut : circ::DefaultCmdOpt, PathArg
    {
        static inline const auto opt = circ::CmdOpt("--dot-out", false);
    };

    struct BitBlastStats : circ::DefaultCmdOpt, PathArg
    {
        static inline const auto opt = circ::CmdOpt("--bitblast-stats", false);
    };

    struct BytesIn : circ::DefaultCmdOpt, Arity< -1 >
    {
        static inline const auto opt = circ::CmdOpt("--bytes-in", false);

        static std::optional< std::vector< uint8_t > > cast(std::vector< std::string > tokens)
        {
            std::string full;
            for (auto &token : tokens)
             full += std::move( token );

            std::vector< uint8_t > out;
            for (auto i = 0U; i < full.size(); i += 2)
            {
              std::string aux = {full[i], full[i + 1]};
              auto casted = static_cast< uint8_t >(std::strtoul(aux.data(), nullptr, 16));
              out.push_back(casted);
            }
            return out;
        }
    };

    struct CiffIn : circ::DefaultCmdOpt, PathArg
    {
        static inline const auto opt = circ::CmdOpt("--ciff-in", false);
    };

    struct LogToStderr : circ::DefaultCmdOpt, Arity< 0 >
    {
        static inline const auto opt = circ::CmdOpt("--logtostderr", false);
    };

    struct LogDir : circ::DefaultCmdOpt, PathArg
    {
        static inline const auto opt = circ::CmdOpt("--log-dir", false);
    };


    struct IROut : circ::DefaultCmdOpt, derive_short_help< IROut >, PathArg
    {
        static inline const auto opt = circ::CmdOpt("--ir-out", false);

        static std::string help()
        {
            return "Path of file where serialized circuit in cirIR will be stored.";
        }
    };

    struct IRIn : circ::DefaultCmdOpt, PathArg
    {
        static inline const auto opt = circ::CmdOpt("--ir-in", false);

        static std::string help()
        {
            std::stringstream ss;
            ss << "Path to file containing circuit in circuitIR serialization format.\n";
            return ss.str();
        }
        static std::string short_help() { return help(); }
    };

    struct SMTIn : circ::DefaultCmdOpt, PathArg
    {
        static inline const auto opt = circ::CmdOpt("--smt-in", false);

        static std::string help()
        {
            std::stringstream ss;
            ss << "Path to file containing circuit in encoded as smt formulae.\n";
            return ss.str();
        }
        static std::string short_help() { return help(); }
    };

    struct Dbg : circ::DefaultCmdOpt, derive_short_help< Dbg >, Arity< 0 >
    {
        static inline const auto opt = circ::CmdOpt("--dbg", false);

        static std::string help() { return "Enable various debug prints.\n"; }
    };

    struct Help : circ::DefaultCmdOpt, derive_short_help< Help >, Arity< 0 >
    {
        static inline const auto opt = circ::CmdOpt("--help", false);
        static std::string help() { return "Print help messages.\n"; }
    };

    struct Version : circ::DefaultCmdOpt, derive_short_help< Version >, Arity< 0 >
    {
        static inline const auto opt = circ::CmdOpt("--version", false);
        static std::string help() { return "Print version.\n"; }
    };

    struct Arch : circ::DefaultCmdOpt, Arity< 1 >, circ::HasAllowed< Arch >
    {
        static inline const auto opt = circ::CmdOpt("--arch", true);
        static inline const std::unordered_set< std::string > allowed = {
            "x86", "amd64", "amd64_avx"
        };

        static std::string help()
        {
            std::stringstream ss;
            ss << "Forwarded to remill, used to initialize proper lifting components.\n"
               << "Currently supported:\n"
               << " * x86 - 32b (tiny86 for example)\n"
               << " * amd64 - 64 optionally `amd64_avx` can be used, but since floating\n"
               << "           point operations are not supported yet it makes no difference.\n";
            return ss.str();
        }

        static std::string short_help()
        {
            std::stringstream ss;
            ss << "Architecture specifier to be forwarded to remill.\n";
            return ss.str();
        }

        static std::optional< std::string > cast(std::vector< std::string > tokens)
        {
            if (tokens.size() != 1)
                return {};
            return { std::move( tokens[0] ) };
        }
    };

    struct OS : circ::DefaultCmdOpt, PathArg, circ::HasAllowed< OS >
    {
        using circ::HasAllowed< OS >::validate;

        static inline const auto opt = circ::CmdOpt("--os", true);
        static inline const std::unordered_set< std::string > allowed =
        {
            "macos", "linux"
        };

        static std::string help()
        {
            std::stringstream ss;
            ss << "Forwarded to remill, used to unitialize proper lifting components.\n";
            return ss.str();
        }

        static std::string short_help() { return help(); }
    };

    struct EqSat : circ::DefaultCmdOpt, Arity< 0 >
    {
        static inline const auto opt = circ::CmdOpt("--eqsat", false);
    };

    struct Patterns : circ::DefaultCmdOpt, PathArg {
      static inline const auto opt = circ::CmdOpt("--patterns", false);

      static std::string help() {
        return "Path to file containing patterns for equality saturation.\n";
      }

      static std::string short_help() {
        return help();
      }
    };

} // namespace circ::cli

