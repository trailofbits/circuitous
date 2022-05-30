/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Verify.hpp>
#include <circuitous/IR/SMT.hpp>
#include <circuitous/Printers.h>
#include <circuitous/Transforms.h>
#include <circuitous/IR/Cost.hpp>

#include <circuitous/Printers/Verilog.hpp>
#include <circuitous/Util/Logging.hpp>
#include <circuitous/Util/CmdParser.hpp>

#include <circuitous/Support/Ciff.hpp>
#include <circuitous/Support/CLIArgs.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <circuitous/Lifter/CircuitSmithy.hpp>

#include <circuitous/Disassembler/DisassemblerPrinter.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <gflags/gflags.h>
#include <glog/logging.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <remill/OS/OS.h>

#include <fstream>
#include <iostream>
#include <unordered_map>

// TODO(lukas): Clean this up once remill gets rid of gflags.
DEFINE_string(arch, "", "");
DEFINE_string(os, REMILL_OS, "");

DEFINE_string(ir_in, "", "Path to a file containing serialized IR.");
DEFINE_string(smt_in, "", "Path to the input smt2 file.");

DEFINE_string(dec_out, "", "Decoder shizzle.");
DEFINE_string(ir_out, "", "Path to the output IR file.");
DEFINE_string(dot_out, "", "Path to the output GraphViz DOT file.");
DEFINE_string(smt_out, "", "Path to the output smt2 file.");
DEFINE_string(json_out, "", "Path to the output JSON file.");
DEFINE_string(verilog_out, "", "Path to the output verilog file.");

DEFINE_string(bitblast_smt_out, "", "Path to the output smt2 file.");
DEFINE_bool(bitblast_stats, false, "Print smt bitblast statistics.");

DEFINE_string(bytes_in, "", "Hex representation of bytes to be lifted");
DEFINE_string(ciff_in, "", "Load input from circuitous-seed --dbg produced file");


DEFINE_string(patterns, "", "Equality saturation patterns.");
DEFINE_bool(eqsat, false, "Enable equality saturation based optimizations.");
DEFINE_bool(dbg, false, "Enable various debug dumps");
DEFINE_bool(quiet, false, "");

namespace cli = circ::cli;

namespace
{
    std::string_view as_string_view(std::vector< uint8_t > &buf)
    {
        return std::string_view( reinterpret_cast<char *>(buf.data()), buf.size());
    }

    using circuit_ptr_t = circ::Circuit::circuit_ptr_t;

    // optimize the circuit.
    template< typename Optimizer, typename CLI >
    circuit_ptr_t optimize(circuit_ptr_t &&circuit, const CLI &cli)
    {
        Optimizer opt;

        if (cli.template present<cli::EqSat>()) {
            auto &[name, pass] = opt.add_pass("eqsat");
            if (auto patterns = cli.template get<cli::Patterns>()) {
                auto eqpass =
                        std::dynamic_pointer_cast<circ::EqualitySaturationPass>(pass);
                eqpass->add_rules(circ::eqsat::parse_rules(patterns.value()));
            }
        }

        auto result = opt.run(std::move(circuit));
        circ::log_info() << "Optimizations done.";
        circ::log_info() << opt.report();
        return result;
    }

}  // namespace


using input_options = circ::tl::TL<
        cli::CiffIn,
        cli::IRIn,
        cli::SMTIn,
        cli::BytesIn
        >;
using output_options = circ::tl::TL<
        circ::cli::DecoderOut
        >;
using remill_config_options = circ::tl::TL<
        circ::cli::Arch,
        circ::cli::OS
        >;

using other_options = circ::tl::TL<
        circ::cli::Quiet,
        circ::cli::Dbg,
        circ::cli::Help,
        circ::cli::Version
        >;

using cmd_opts_list = circ::tl::merge<
        input_options,
        output_options,
        remill_config_options,
        other_options
        >;

circ::CircuitPtr get_input_circuit(auto &cli)
{
    auto make_circuit = [&](auto buf) {
        circ::log_info() << "Going to make circuit";
        circ::Ctx ctx{ *cli.template get< cli::OS >(), *cli.template get< cli::Arch >() };
        return circ::CircuitSmithy(std::move(ctx)).smelt(buf).forge();
    };

    if (auto bytes = cli.template get< cli::BytesIn >())
        return make_circuit(as_string_view(*bytes));

    if (auto ir_file = cli.template get< cli::IRIn >())
        return circ::Circuit::deserialize(*ir_file);
    if (auto smt_file = cli.template get< cli::SMTIn >())
        return circ::smt::deserialize(*smt_file);

    if (auto cif = cli.template get< cli::CiffIn >())
        return make_circuit(circ::CIFFReader().read(*cif).take_bytes());
    return {};
}

void store_outputs(const auto &cli, const circ::CircuitPtr &circuit)
{
    if (auto ir_out = cli.template get< cli::IROut >())
        circuit->serialize(*ir_out);

    if (auto json_out = cli.template get< cli::JsonOut >())
        circ::print_circuit(*json_out, circ::print_json, circuit.get());

    if (auto dot_out = cli.template get< cli::DotOut >())
        circ::print_circuit(*dot_out, circ::print_dot, circuit.get(),
                            std::unordered_map< circ::Operation *, std::string>{});

    if (auto verilog_out = cli.template get< cli::VerilogOut >())
        circ::print_circuit(*verilog_out, circ::print_verilog, "circuit", circuit.get());

    if (auto smt_out = cli.template get< cli::SMTOut >())
        circ::print_circuit(*smt_out, circ::print_smt, circuit.get());

    if (auto bitblast_smt = cli.template get< cli::BitBlastSmtOut >())
        circ::print_circuit(*bitblast_smt, circ::print_bitblasted_smt, circuit.get());
}

template< typename OptsList >
auto parse_and_validate_cli(int argc, char *argv[]) -> std::optional< circ::ParsedCmd >
{
    using namespace circ;

    static const auto yield_err = [&](const auto &msg)
    {
        std::cerr << msg << std::endl;
    };

    auto parsed = circ::CmdParser< OptsList >::parse_argv(argc, argv);
    if (!parsed)
    {
        std::cerr << "Command line arguments were not parsed correctly, see "
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

    if (v.check(one_of< input_options >())
    .process_errors(yield_err))
    {
        return {};
    }

    if (v.validate_leaves( OptsList{} ).process_errors(yield_err))
        return {};

    return parsed;
}

void reload_test(const circ::CircuitPtr &circuit)
{
    circuit->serialize("reload_test.circir");
    auto reload = circ::Circuit::deserialize("reload_test.circir");
    VerifyCircuit("Reload test starting ...\n", reload.get(), "Reload test successful.\n");
}

int main(int argc, char *argv[]) {
    auto maybe_parsed_cli = parse_and_validate_cli< cmd_opts_list >(argc, argv);
    if (!maybe_parsed_cli)
    {
        std::cerr << circ::help_str(cmd_opts_list());
        return 1;
    }

    auto parsed_cli = std::move(*maybe_parsed_cli);

    if (parsed_cli.template present< circ::cli::Help >())
    {
        std::cout << circ::help_str(cmd_opts_list());
        return 0;
    }

    if (parsed_cli.template present< circ::cli::Version >())
    {
        std::cerr << "TODO: Implement proper version message";
        return 1;
    }

    if (!parsed_cli.template present< circ::cli::Quiet >())
    {
        circ::add_sink< circ::severity::kill >(std::cerr);
        circ::add_sink< circ::severity::error >(std::cerr);
        circ::add_sink< circ::severity::warn >(std::cerr);
        circ::add_sink< circ::severity::info >(std::cout);
        circ::add_sink< circ::severity::trace >(std::cout);
        circ::Tracers::trace_all = true;
        if (parsed_cli.present< cli::Dbg >())
            circ::add_sink< circ::severity::dbg >(std::cout);

    }
    google::ParseCommandLineFlags(&argc, &argv, true);
    google::InitGoogleLogging(argv[0]);

    auto circuit = get_input_circuit(parsed_cli);
    if (!circuit)
    {
        std::cerr << "Not able to load circuit.\n";
        return 3;
    }

    VerifyCircuit("Verifying loaded circuit.", circuit.get(), "Circuit is valid.");

    if (parsed_cli.present< cli::Dbg >())
        circuit = optimize< circ::DebugOptimizer >(std::move(circuit), parsed_cli);
    else
        circuit = optimize< circ::DefaultOptimizer >(std::move(circuit), parsed_cli);


    if (parsed_cli.present< cli::Dbg >())
    {
        circ::log_dbg() << "Stats of final circuit:\n";
        circ::log_dbg() << circ::GetStats(circuit.get());
    }

    circ::log_info() << "Generating disassembler.";
    if (auto decoder_out = cli.template get< cli::DecoderOut >()){
        auto gen = circ::disassm::DisassemblerPrinter(circuit);
        gen.print_file();
    }
    
    return 0;
}
