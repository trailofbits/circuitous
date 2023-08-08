/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <circuitous/Support/CLIArgs.hpp>

#include <circuitous/Run/Execute.hpp>
#include <circuitous/Run/Inspect.hpp>
#include <circuitous/Run/Interpreter.hpp>
#include <circuitous/Run/Trace.hpp>
#include <circuitous/Run/TraceConversion.hpp>

#include <circuitous/IR/Verify.hpp>
#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Serialize.hpp>
#include <circuitous/Printers.hpp>

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Lifter/Decoder.hpp>
#include <circuitous/Util/InstructionBytes.hpp>

#include <fstream>
#include <iostream>
#include <unordered_map>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/Support/JSON.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
CIRCUITOUS_UNRELAX_WARNINGS

// `circuitous-run` specific command line flags.
namespace circ::cli::run
{
    struct ExportDerived : DefaultCmdOpt, PathArg
    {
        static inline const auto opt = CmdOpt("--export-derived", false);
        static std::string help()
        {
            std::stringstream ss;
            ss << "Path to store derived values into.\n";
            return ss.str();
        };
    };

    struct Traces : DefaultCmdOpt, PathArg
    {
        static inline const auto opt = CmdOpt("--traces", false);
        static std::string help()
        {
            return "Path to traces.";
        }
    };

    struct Memory : DefaultCmdOpt, PathArg
    {
        static inline const auto opt = CmdOpt("--memory", false);
        static std::string help()
        {
            return "addr,hex_val.\n";
        }
    };

    struct Derive : DefaultCmdOpt, Arity< 0 >
    {
        static inline const auto opt = CmdOpt("--derive", false);
    };

    struct Verify : DefaultCmdOpt, Arity< 0 >
    {
        static inline const auto opt = CmdOpt("--verify", false);
    };

    struct ConvertTrace : DefaultCmdOpt, Arity< 1 >, HasAllowed< ConvertTrace >
    {
        static inline const auto opt = CmdOpt("--convert-trace", false);
        static std::string help()
        {
            return "Convert trace from format specified as argument.";
        }

        static inline std::unordered_set< std::string > allowed =
        {
            "mttn"
        };
    };

    struct ParseTrace : DefaultCmdOpt, Arity< 1 >, HasAllowed< ParseTrace >
    {
        static inline const auto opt = CmdOpt("--parse-trace", false);
        static std::string help()
        {
            return "Parse alien trace. If output arg is missing, cout is used.";
        }

        static inline std::unordered_set< std::string > allowed =
        {
            "mttn"
        };
    };

    struct CircuitFromTrace : DefaultCmdOpt, Arity< 0 >
    {
        static inline const auto opt = CmdOpt("--construct-circuit", false);
    };

    struct Output : DefaultCmdOpt, PathArg
    {
        static inline const auto opt = CmdOpt( "--output", { "-o" }, false );
        static std::string help()
        {
            return "File to store output to.";
        }
    };

    struct Die : DefaultCmdOpt, Arity< 0 >
    {
        static inline const auto opt = CmdOpt("--die", false);
    };

    struct IRIn : DefaultCmdOpt, PathArg
    {
        static inline const auto opt = CmdOpt("--ir-in", true);
    };

    struct Ctl : DefaultCmdOpt, PathArg
    {
        static inline const auto opt = CmdOpt("--ctl", false);
    };

} // namespace circ::cli::run

auto load_circ(const std::string &file)
{
    // Deserialize circuit from binary IR file
    auto circuit = circ::deserialize(file);

    auto verify_res = circ::verify_circuit(circuit.get());
    circ::check(!verify_res.has_errors(), [&]() {
            std::ostringstream os;
            os << "Loaded IR is not valid -- Aborting.\n" << verify_res;
            return os.str();
    });
    if (verify_res.has_warnings())
        circ::log_info() << "Warnings produced while loading IR.\n"
                         << verify_res.get_warnings();

    return circuit;
}

/** Interpreter & testing related functions. **/

std::string str(const llvm::APInt &what) { return llvm::toString(what, 16, false); }
template< typename I > requires ( std::is_integral_v< I > )
std::string hex(I what) { std::stringstream ss; ss << std::hex << what; return ss.str(); }

llvm::json::Object serialize_mem_hint(const auto &val)
{
    llvm::json::Object mem_hint;
    mem_hint["used"]  = str(val.used());
    mem_hint["mode"]  = str(val.mode());
    mem_hint["id"]    = str(val.id());
    mem_hint["size"]  = str(val.size());
    mem_hint["addr"]  = str(val.addr());
    mem_hint["value"] = str(val.value());
    mem_hint["ts"]    = str(val.timestamp());
    return mem_hint;
}

template< typename E >
llvm::json::Object serialize_mem_hints(const std::vector< E > &vals)
{
    llvm::json::Object out;
    for (const auto &val : vals)
        out[str(val.id())] = serialize_mem_hint(val);

    return out;
}

llvm::json::Object serialize(const circ::run::ExportMemory &entry)
{
    llvm::json::Object out;
    out["result"] = entry.result;
    out["memory_hints"] = serialize_mem_hints(entry.hints);
    return out;
}

// TODO(lukas): Generalize.
llvm::json::Object serialize(const auto &ctl)
{
    llvm::json::Object out;

    out["result"] = ctl.result();

    llvm::json::Object traces;
    for (std::size_t i = 0; i < ctl.to_export.size(); ++i)
        traces[hex(i)] = serialize(ctl.to_export[i]);

    out["traces"] = std::move(traces);
    return out;
}

llvm::json::Object serialize( const std::vector< circ::run::result_t > &results,
                              const auto &memory_hints )
{
    llvm::json::Object out;

    auto export_result = [ & ]( auto r ) -> std::string
    {
        if ( circ::run::accepted( r ) )
            return "accept";
        if ( circ::run::rejected( r ) )
            return "reject";
        return "error";
    };

    circ::log_dbg() << to_string( results.back() );
    out[ "result" ] = export_result( results.back() );
    circ::log_info() << "result:" << export_result( results.back() );

    llvm::json::Object traces;
    for ( std::size_t i = 0; i < results.size(); ++i )
    {
        llvm::json::Object trace;
        trace[ "result" ] = export_result( results[ i ] );

        llvm::json::Object out_hints;
        for ( std::size_t j = 0; j < memory_hints[ i ].size(); ++j )
        {
            auto val = memory_hints[ i ][ j ];
            out_hints[ str( val.id() ) ] = serialize_mem_hint( val );
        }
        trace[ "memory_hints" ] = std::move( out_hints );

        traces[hex(i)] = std::move( trace );
    }

    out[ "traces" ] = std::move( traces );
    return out;
}

void store_json(const std::string &path, llvm::json::Object obj)
{
    // Open output file
    std::error_code ec;
    llvm::raw_fd_ostream output(path, ec, llvm::sys::fs::OF_Text);
    circ::check(!ec) << "Error while opening output state JSON file: " << ec.message();

    output << llvm::formatv("{0:2}", llvm::json::Value(std::move(obj)));
}

template< typename Runner, typename CLI >
void run(const CLI &parsed_cli)
{
    auto circuit = load_circ(*parsed_cli.template get< circ::cli::run::IRIn >());

    auto json_trace = parsed_cli.template get< circ::cli::run::Traces >();
    circ::check(json_trace);

    auto trace = circ::run::trace::native::load_json(*json_trace);
    circ::check(trace.entries.size() >= 2) << trace.entries.size();

    auto ctl = [ & ]() -> std::string {
        auto maybe_ctl = parsed_cli.template get< circ::cli::run::Ctl >();
        if ( !maybe_ctl )
            return "derive";
        return *maybe_ctl;
    }();

    if ( ctl == "derive" )
    {
        circ::run::DefaultControl< circ::run::ExportMemory > ctrl;
        circ::run::test_trace(circuit.get(), trace, ctrl);

        auto as_json = serialize(ctrl);

        auto result_path = parsed_cli.template get< circ::cli::run::ExportDerived >();
        store_json(*result_path, std::move(as_json));
    } else if ( ctl == "verify" ) {
        std::vector< circ::run::parsed_mem_hints > memory_hints;
        auto collect = [ & ]( const auto &result_spawn_pairs )
        {
            for ( auto &[ status, spawn ] : result_spawn_pairs )
                if ( circ::run::accepted( status ) )
                {
                    memory_hints.push_back( spawn->get_derived_mem() );
                    return;
                }
            memory_hints.emplace_back();
        };

        auto results = circ::run::StatelessControl().test( circuit.get(), trace, collect );
        circ::log_dbg() << "[circuitous-run]:" << "Collected " << memory_hints.size()
                                               << "memory hints";
        auto as_json = serialize( results, memory_hints );
        auto result_path = parsed_cli.template get< circ::cli::run::ExportDerived >();
        store_json( *result_path, std::move( as_json ) );

    } else {
        circ::log_kill() << "uknown ctl";
    }

    if (parsed_cli.template present< circ::cli::run::Die >())
        circ::log_kill() << "FLAGS_die induced death.";
}

/** Trace conversion methods. **/

template< typename cli_t, circ::run::trace::loader_with_circuit_ctor loader_t >
auto produce_circuit( const cli_t &cli, loader_t loader )
{
    if ( auto in_file = cli.template get< circ::cli::run::IRIn >() )
        return load_circ( *in_file );

    return std::move( loader ).reconstruct();
}

auto parse_alien_trace( const auto &cli )
{
    circ::log_dbg() << "[run]:" << "Converting traces";
    auto trace_file = *cli.template get< circ::cli::run::Traces >();
    return circ::run::trace::alien_loader().parse_alien_trace( trace_file );
}

void parse_trace( const auto &cli )
{
    auto traces = parse_alien_trace( cli );

    auto dump = [&](auto &where)
    {
        where << traces.to_string();
    };

    if ( auto out = cli.template get< circ::cli::run::Output >() )
    {
        std::ofstream ofile( *out );
        circ::check( ofile );
        return dump( ofile );
    }

    return dump( std::cout );
}

void convert_trace( const auto &cli )
{
    auto out = *cli.template get< circ::cli::run::Output >();

    auto trace_file = *cli.template get< circ::cli::run::Traces >();
    auto loader = circ::run::trace::with_reconstructor();
    auto traces = loader.parse_alien_trace( trace_file );
    auto circuit = produce_circuit( cli, std::move( loader ) );

    circ::run::trace::trace_converter().convert_trace( traces, circuit.get() ).dump( out );
}

using run_modes = circ::tl::TL<
    circ::cli::run::Derive,
    circ::cli::run::Verify,
    circ::cli::run::ConvertTrace,
    circ::cli::run::ParseTrace
>;
using deprecated_options = circ::tl::TL<
    circ::cli::LogDir,
    circ::cli::LogToStderr
>;
using input_options = circ::tl::TL<
    circ::cli::run::IRIn,
    circ::cli::run::CircuitFromTrace
>;
using output_options = circ::tl::TL<
    circ::cli::run::ExportDerived,
    circ::cli::DotOut,
    circ::cli::run::Output
>;
using config_options = circ::tl::TL<
    circ::cli::run::Traces,
    circ::cli::run::Memory,
    circ::cli::run::Die,
    circ::cli::run::Ctl
>;
using other_options = circ::tl::TL<
    circ::cli::Help,
    circ::cli::Version,
    circ::cli::Dbg
>;

using cmd_opts_list = circ::tl::merge<
    run_modes,
    deprecated_options,
    input_options,
    output_options,
    config_options,
    other_options
>;

std::optional< circ::ParsedCmd > parse_and_validate(int argc, char *argv[])
{
    using namespace circ;
    static const auto yield_err = [&](const auto &msg)
    {
        std::cerr << msg << std::endl;
    };

    auto parsed = CmdParser< cmd_opts_list >::parse_argv(argc, argv);
    if (!parsed)
    {
        std::cerr << "Command line argumentes were not parsed cirrectly, see "
                  << "stderr for more details.";
        return {};
    }

    auto v = Validator(parsed);
    if (v.check(is_singleton< cli::Help >())
         .check(is_singleton< cli::Version >())
         .process_errors(yield_err))
    {
        return {};
    }

    if (v.check(are_exclusive< run_modes >()).process_errors(yield_err))
        return {};

    if (v.validate_leaves(cmd_opts_list()).process_errors(yield_err))
        return {};

    // TODO(bin:run): Should imply one of input_options.
    if (v.check(implies< cli::run::ConvertTrace, cli::run::Output >())
         .process_errors(yield_err))
    {
        return {};
    }

    if (v.check(are_exclusive< input_options >()).process_errors(yield_err))
    {
        return {};
    }

    return parsed;
}

int main(int argc, char *argv[])
{
    circ::add_sink< circ::severity::kill >(std::cerr);
    circ::add_sink< circ::severity::error >(std::cerr);
    circ::add_sink< circ::severity::warn >(std::cerr);
    circ::add_sink< circ::severity::info >(std::cout);

    auto maybe_cli = parse_and_validate(argc, argv);
    if (!maybe_cli)
    {
        std::cerr << circ::help_str(cmd_opts_list());
        return 1;
    }

    auto cli = std::move(*maybe_cli);

    if (cli.present< circ::cli::Help >())
    {
        std::cerr << circ::help_str(cmd_opts_list());
        return 0;
    }

    if (cli.present< circ::cli::Version >())
    {
        std::cerr << "TODO: Implement proper version message!";
        return 1;
    }

    if (cli.present< circ::cli::Dbg >())
        circ::add_sink< circ::severity::dbg >(std::cout);

    // REFACTOR(lukas): Old derive/verify are no longer reasonable.
    if (cli.present< circ::cli::run::Verify >())
        run< circ::run::Interpreter >(cli);
    else if (cli.present< circ::cli::run::Derive >())
    {
        circ::unreachable() << "--derive is currently broken, WIP.";
        run< circ::run::Interpreter >(cli);
    } else if (cli.present< circ::cli::run::ConvertTrace >()) {
        convert_trace(cli);
    } else if (cli.present< circ::cli::run::ParseTrace >()) {
        parse_trace(cli);
    } else {
        std::cerr << "[run]: Selected cmd args resulted in no operation being run.";
    }

    return 0;
}
