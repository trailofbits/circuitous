/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#include <circuitous/Util/Logging.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <circuitous/Support/CLIArgs.hpp>

#include <circuitous/Run/Inspect.hpp>
#include <circuitous/Run/Interpreter.h>
#include <circuitous/Run/Trace.hpp>

#include <circuitous/IR/Verify.hpp>
#include <circuitous/IR/IR.h>
#include <circuitous/Printers.h>

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
    struct SingularCurrent : DefaultCmdOpt, PathArg
    {
        static inline const auto opt = CmdOpt("--singular-current", false);
        static std::string help()
        {
            std::stringstream ss;
            ss << "Path to current trace entry in singular form.\n";
            return ss.str();
        }
    };

    struct SingularNext : DefaultCmdOpt, PathArg
    {
        static inline const auto opt = CmdOpt("--singular-next", false);
        static std::string help()
        {
            std::stringstream ss;
            ss << "Path to next trace entry in a singular form.\n";
            ss << "Usefull with --singular-current to verify a trace transition.\n";
            return ss.str();
        }
    };

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

    const auto &[status, msg, warnings] = circ::VerifyCircuit(circuit.get());
    circ::check(status,
                [msg_ = msg](){ return "Loaded IR is not valid -- Aborting.\n" + msg_; });
    if (!warnings.empty())
        circ::log_info() << "Warnings produced while loading IR.\n"
                         << warnings;

    return circuit;
}

auto load_singular(const std::string &path) -> std::optional< circ::run::trace::Entry >
{
    if (path.empty())
        return {};

    using namespace circ::run::trace;
    return std::make_optional(get_entry(0ul, load_json(path)));
}

template< typename I >
void export_derived(I inspect, const std::string &export_derived_to)
{
    // Open output file
    std::error_code ec;
    llvm::raw_fd_ostream output(export_derived_to, ec, llvm::sys::fs::F_Text);
    circ::check(!ec) << "Error while opening output state JSON file: " << ec.message();

    // Dump output register values to JSON
    llvm::json::Object output_obj;
    llvm::json::Object output_regs_obj;
    llvm::json::Object output_mem_hints;

    auto as_str = [](const auto &what) -> std::string {
        return std::to_string(what->getLimitedValue());
    };

    if (inspect.focus()) {
        for (auto [reg, val] : inspect->template get_derived<circ::OutputRegister>()) {
            circ::check(val);
            output_regs_obj[reg->reg_name] = std::to_string(val->getLimitedValue());
        }
        for (auto [_, val] : inspect->template get_derived<circ::OutputErrorFlag>()) {
            output_obj["ebit"] = (val == llvm::APInt(1, 1));
        }
        output_obj["timestamp"] = as_str(inspect->get(inspect.circuit->output_timestamp()));

        auto str = [](auto val) {
            return val.toString(10, false);
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
    output_obj["result"] = inspect.g_result();
    output_obj["mem_hints"] = std::move(output_mem_hints);
    output << llvm::json::Value(std::move(output_obj));

    if (!inspect.lenses) {
        inspect.focus(0u);
    }
}

void print_dot() {

}

template< typename Runner, typename CLI >
void run(const CLI &parsed_cli)
{
    auto circuit = load_circ(*parsed_cli.template get< circ::cli::run::IRIn >());
    Runner run(circuit.get());

    if (auto singular_current = parsed_cli.template get< circ::cli::run::SingularCurrent >())
    {
        auto current_trace = load_singular(*singular_current);
        circ::check(current_trace);
        run.set_input_state(*current_trace);
        for (auto [addr, data] : current_trace->initial_memory)
            run.set_memory(addr, data);
    }

    if (auto singular_next = parsed_cli.template get< circ::cli::run::SingularNext >())
    {
        auto next_trace = load_singular(*singular_next);
        circ::check(next_trace);
        run.set_output_state(*next_trace);
    }

    if (auto memory = parsed_cli.template get< circ::cli::run::Memory >())
    {
        // TODO(lukas): Simplify.
        llvm::StringRef s_ref(*memory);
        auto [addr, val] = s_ref.split(',');
        run.set_memory(std::strtoull(addr.data(), nullptr, 16), val.str());
    }

    run.Run();

    if (parsed_cli.template present< circ::cli::run::Die >())
    {
        std::cout << run.dump_runners();
        circ::log_kill() << "FLAGS_die induced death.";
    }

    if (auto export_derived_to = parsed_cli.template get< circ::cli::run::ExportDerived >())
        export_derived(circ::run::Inspector<Runner>(&run), *export_derived_to);

    if (auto dot_out = parsed_cli.template get< circ::cli::DotOut >(); dot_out && run.acceptor)
    {
        std::unordered_map<circ::Operation *, std::string> values;
        for (auto &[op, val] : run.values()) {
            values[op] = (val) ? val->toString(16, false) : std::string("{ undef }");
        }
        std::ofstream os(*dot_out);
        circ::print_dot(os, circuit.get(), values);
    }
}

auto parse_cmd(int argc, char *argv[])
{
    using namespace circ::cli;
    using parser_t = circ::CmdParser<
        run::IRIn, LogDir, LogToStderr, DotOut,
        Dbg,
        //StateIn, StateOut, DotOut,
        run::SingularCurrent, run::SingularNext,
        run::ExportDerived,
        run::Traces, run::Memory,
        run::Verify, run::Derive,
        run::Die
    >;
    using run_mode = std::tuple< run::Verify, run::Derive >;
    return parser_t::parse_argv(argc, argv).validate();
}

int main(int argc, char *argv[])
{
    circ::add_sink< circ::severity::kill >(std::cerr);
    circ::add_sink< circ::severity::error >(std::cerr);
    circ::add_sink< circ::severity::warn >(std::cerr);
    circ::add_sink< circ::severity::info >(std::cout);

    auto cli = parse_cmd(argc, argv);

    if (cli.present< circ::cli::Dbg >())
        circ::add_sink< circ::severity::dbg >(std::cout);

    using run_modes_t = std::tuple< circ::cli::run::Derive, circ::cli::run::Verify >;
    if (!cli.exactly_one_present(run_modes_t{}))
    {
        std::cerr << "Must choose one of run or derive!\n" << std::endl;
        return 1;
    }
    if (cli.present< circ::cli::Dbg >())
        circ::add_sink< circ::severity::dbg >(std::cerr);

    if (cli.present< circ::cli::run::Verify >())
        run< circ::run::VQueueInterpreter >(cli);
    else if (cli.present< circ::cli::run::Derive >())
        run< circ::run::DQueueInterpreter >(cli);

    return 0;
}
