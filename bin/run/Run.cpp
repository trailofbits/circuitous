/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <circuitous/Support/CLIArgs.hpp>

#include <circuitous/Run/Inspect.hpp>
#include <circuitous/Run/Interpreter.hpp>
#include <circuitous/Run/Trace.hpp>

#include <circuitous/IR/Verify.hpp>
#include <circuitous/IR/IR.hpp>
#include <circuitous/Printers.hpp>

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

    struct Die : DefaultCmdOpt, Arity< 0 >
    {
        static inline const auto opt = CmdOpt("--die", false);
    };

    struct IRIn : DefaultCmdOpt, PathArg
    {
        static inline const auto opt = CmdOpt("--ir-in", true);
    };

} // namespace circ::cli::run

auto load_circ(const std::string &file)
{
    // Read input circuit file
    std::ifstream ir(file, std::ios::binary);
    circ::check(ir, []() { return std::strerror(errno); });

    // Deserialize circuit from binary IR file
    auto circuit = circ::Circuit::deserialize(ir);

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


template< typename I >
void export_derived(const I &acceptors, const std::string &export_derived_to)
{
    // Open output file
    std::error_code ec;
    llvm::raw_fd_ostream output(export_derived_to, ec, llvm::sys::fs::OF_Text);
    circ::check(!ec) << "Error while opening output state JSON file: " << ec.message();

    // Dump output register values to JSON
    llvm::json::Object output_obj;
    llvm::json::Object output_regs_obj;
    llvm::json::Object output_mem_hints;

    auto as_str = [](const auto &what) -> std::string {
        return std::to_string(what->getLimitedValue());
    };

    if (acceptors.size() != 0) {
        const auto &inspect = acceptors[ 0 ];
        for (auto [reg, val] : inspect->template get_derived<circ::OutputRegister>()) {
            circ::check(val);
            output_regs_obj[reg->reg_name] = std::to_string(val->getLimitedValue());
        }
        for (auto [_, val] : inspect->template get_derived<circ::OutputErrorFlag>()) {
            output_obj["ebit"] = (val == llvm::APInt(1, 1));
        }
        output_obj["timestamp"] = as_str(inspect->get_node_val(inspect->circuit->output_timestamp()));

        auto str = [](auto val) {
            return llvm::toString(val, 10, false);
        };

        for (const auto &val : inspect->get_derived_mem()) {
            llvm::json::Object mem_hint;
            mem_hint["used"]  = str(val.used());
            mem_hint["mode"]  = str(val.mode());
            mem_hint["id"]    = str(val.id());
            mem_hint["size"]  = str(val.size());
            mem_hint["addr"]  = str(val.addr());
            mem_hint["value"] = str(val.value());
            mem_hint["ts"]    = str(val.timestamp());
            output_mem_hints[str(val.id())] = std::move(mem_hint);
        }
    }

    // Serialize
    output_obj["regs"] = std::move(output_regs_obj);
    output_obj["result"] = acceptors.size() == 1;
    output_obj["mem_hints"] = std::move(output_mem_hints);
    output << llvm::json::Value(std::move(output_obj));
}

template< typename Runner, typename CLI >
void run(const CLI &parsed_cli)
{
    auto circuit = load_circ(*parsed_cli.template get< circ::cli::run::IRIn >());
    circ::run::NodeStateBuilder node_state{ circuit.get() };
    circ::run::MemoryBuilder memory{ circuit.get() };

    if (auto traces = parsed_cli.template get< circ::cli::run::Traces >())
    {
        auto trace = circ::run::trace::native::load_json(*traces);
        circ::check(trace.entries.size() >= 2) << trace.entries.size();

        auto &fst = *trace.entries.begin();
        auto &snd = *std::next(trace.entries.begin());

        auto step = circ::run::trace::native::make_step_trace(circuit.get(), fst, snd);
        node_state.set(step);

        for (const auto &[addr, val] : trace.initial_memory)
            memory.set(addr, val);
    }

    node_state.all< circ::Undefined >({});
    auto interpreter = Runner(circuit.get(), node_state.take(), memory.take());
    interpreter.template derive< circ::AdviceConstraint >();
    interpreter.template derive< circ::ReadConstraint >();
    interpreter.template derive< circ::WriteConstraint >();
    interpreter.template derive< circ::UnusedConstraint >();
    auto acceptors = interpreter.run_all();
    circ::check(acceptors.size() < 2)
        << "More than one acceptor, found: " << acceptors.size();

    if (parsed_cli.template present< circ::cli::run::Die >())
    {
        circ::log_kill() << "FLAGS_die induced death.";
    }

    if (auto export_derived_to = parsed_cli.template get< circ::cli::run::ExportDerived >())
        export_derived(acceptors, *export_derived_to);
}

using run_modes = circ::tl::TL<
    circ::cli::run::Derive,
    circ::cli::run::Verify
>;
using deprecated_options = circ::tl::TL<
    circ::cli::LogDir,
    circ::cli::LogToStderr
>;
using input_options = circ::tl::TL<
    circ::cli::run::IRIn
>;
using output_options = circ::tl::TL<
    circ::cli::run::ExportDerived,
    circ::cli::DotOut
>;
using config_options = circ::tl::TL<
    circ::cli::run::Traces,
    circ::cli::run::Memory,
    circ::cli::run::Die
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
    }

    return 0;
}
