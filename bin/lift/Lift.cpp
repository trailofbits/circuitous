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

#include <circuitous/Support/CLIArgs.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <gflags/gflags.h>
#include <glog/logging.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <remill/OS/OS.h>

#include <fstream>
#include <iostream>
#include <unordered_map>

DEFINE_string(arch, "", "");
DEFINE_string(os, REMILL_OS, "");

DEFINE_string(ir_in, "", "Path to a file containing serialized IR.");
DEFINE_string(smt_in, "", "Path to the input smt2 file.");

DEFINE_string(ir_out, "", "Path to the output IR file.");
DEFINE_string(dot_out, "", "Path to the output GraphViz DOT file.");
DEFINE_string(smt_out, "", "Path to the output smt2 file.");
DEFINE_string(json_out, "", "Path to the output JSON file.");
DEFINE_string(verilog_out, "", "Path to the output verilog file.");

DEFINE_string(bitblast_smt_out, "", "Path to the output smt2 file.");
DEFINE_bool(bitblast_stats, false, "Print smt bitblast statistics.");

DEFINE_string(bytes_in, "", "Hex representation of bytes to be lifted");
DEFINE_string(seed_dbg_in, "", "Load input from circuitous-seed --dbg produced file");


DEFINE_bool(eqsat, false, "Enable equality saturation based optimizations.");
DEFINE_bool(dbg, false, "Enable various debug dumps");

namespace opt = circ::opt;

namespace
{
    std::vector< std::string > load_seed_dbg(const std::string &fname) {
      std::vector< std::string > out;

      auto process = [](std::string line) {
        auto [bytes, _] = llvm::StringRef(line).split(' ');
        return bytes.str();
      };

      std::ifstream in(fname);
      for (std::string line; std::getline(in, line);)
        out.push_back(process(std::move(line)));
      return out;
    }

    void add_to_buffer(std::vector< uint8_t > &buf, const std::string &str)
    {
      CHECK(str.size() % 2 == 0);
      for (std::size_t i = 0; i < str.size(); i += 2) {
        std::string aux = {str[i], str[i + 1]};
        auto casted = static_cast< uint8_t >(std::strtoul(aux.data(), nullptr, 16));
        buf.push_back(casted);
      }
    }

    std::string_view as_string_view(std::vector< uint8_t > &buf)
    {
        return std::string_view( reinterpret_cast<char *>(buf.data()), buf.size());
    }

    using circuit_ptr_t = circ::Circuit::circuit_ptr_t;

    // optimize the circuit.
    template< typename Optimizer >
    circuit_ptr_t optimize(circuit_ptr_t &&circuit)
    {
        Optimizer opt;

        opt.add_pass("dummy-pass");

        auto result = opt.run(std::move(circuit));
        circ::log_info() << "Optimizations done.";
        circ::log_info() << opt.report();
        return result;
    }

}  // namespace

// TODO(lukas): This can be generalised using templated lambdas, but my local clang crashes
//              on them. Revisit with later version.
template< typename Parser, typename H, typename ... Ts >
uint64_t count_matched(const Parser &parser)
{
    uint64_t self = parser.template matched< H >();
    if constexpr (sizeof ... (Ts) == 0) return self;
    else return self + count_matched< Parser, Ts ... >(parser);
}

template< typename Parser, typename H, typename ... Ts >
std::string collect_status(const Parser &parser)
{
    std::string self = H::opt.primary + " -> ";
    if (parser.template matched< H >())
        self += "matched!\n";
    else
        self += "not matched!\n";
    if constexpr (sizeof ... (Ts) == 0) return self;
    else return self + collect_status< Parser, Ts ... >(parser);
}

template< typename Parser, typename ... Ts >
bool exactly_one_matched(const Parser &parser, std::tuple< Ts ... >)
{
    return count_matched< Parser, Ts ... >(parser) == 1;
}

template< typename Parser, typename ... Ts >
std::string status(const Parser &parser, std::tuple< Ts ... >)
{
    return collect_status< Parser, Ts ... >(parser);
}

circ::CircuitPtr get_input_circuit(const auto &cli)
{
    using in_opts = std::tuple< opt::SeedDbgIn, opt::IRIn, opt::SMTIn, opt::BytesIn >;
    if (!exactly_one_matched(cli, in_opts{}))
    {
        std::cerr << "Multiple options to produce circuit specified, do not know how to "
                  << "proceed!" << std::endl;
        std::cerr << status(cli, in_opts{});
        return {};
    }

    auto make_circuit = [&](auto buf) {
        circ::log_info() << "Going to make circuit";
        return circ::Circuit::make_circuit(*cli.template get< opt::Arch >(),
                                           *cli.template get< opt::OS >(),
                                           buf);
    };

    if (auto bytes = cli.template get< opt::BytesIn >())
        return make_circuit(as_string_view(*bytes));

    if (auto ir_file = cli.template get< opt::IRIn >())
        return circ::Circuit::deserialize(*ir_file);
    if (auto smt_file = cli.template get< opt::SMTIn >())
        return circ::smt::deserialize(*smt_file);

    if (auto seed_dbg_file = cli.template get< opt::SeedDbgIn >())
    {
        std::vector< uint8_t > buf;
        for (const auto &bytes : load_seed_dbg(*seed_dbg_file))
            add_to_buffer(buf, bytes);
        return make_circuit(as_string_view(buf));
    }
    return {};
}

void store_outputs(const auto &cli, const circ::CircuitPtr &circuit)
{
    if (auto ir_out = cli.template get< opt::IROut >())
        circuit->serialize(*ir_out);

    if (auto json_out = cli.template get< opt::JsonOut >())
        circ::print_circuit(*json_out, circ::print_json, circuit.get());

    if (auto dot_out = cli.template get< opt::DotOut >())
        circ::print_circuit(*dot_out, circ::print_dot, circuit.get(),
                            std::unordered_map< circ::Operation *, std::string>{});

    if (auto verilog_out = cli.template get< opt::VerilogOut >())
        circ::print_circuit(*verilog_out, circ::print_verilog, "circuit", circuit.get());

    if (auto smt_out = cli.template get< opt::SMTOut >())
        circ::print_circuit(*smt_out, circ::print_smt, circuit.get());

    if (auto bitblast_smt = cli.template get< opt::BitBlastSmtOut >())
        circ::print_circuit(*bitblast_smt, circ::print_bitblasted_smt, circuit.get());
}

template< typename Parser >
auto parse_and_validate_cli(int argc, char *argv[]) -> std::optional< Parser >
{
    auto parser = Parser{};
    parser.parse_argv(argc, argv);

    auto yield_err = [&](const auto &lopt, const auto &msg)
    {
        std::cerr << lopt << " validate() failed with: " << msg << std::endl;
    };

    if (!parser.validate(yield_err))
    {
        std::cerr << "Command line arguments were not validated correctly, see "
                  << "stderr for more details.";
        return {};
    }
    return { parser };
}

int main(int argc, char *argv[]) {
    using parser_t = circ::CmdOpts<
        opt::Arch, opt::OS, opt::Dbg,
        opt::IRIn, opt::SMTIn, opt::BytesIn, opt::SeedDbgIn,
        opt::SMTOut, opt::JsonOut, opt::BitBlastSmtOut, opt::VerilogOut,
        opt::IROut, opt::DotOut,
        opt::BitBlastStats,
        opt::LogToStderr >;

    auto parsed_cli = parse_and_validate_cli< parser_t >(argc, argv);
    if (!parsed_cli)
        return 1;

    circ::add_sink< circ::severity::kill >(std::cerr);
    circ::add_sink< circ::severity::error >(std::cerr);
    circ::add_sink< circ::severity::warn >(std::cerr);
    circ::add_sink< circ::severity::info >(std::cout);

    // NOTE(lukas): Support libraries still need to be initialized, since
    //              remill may be using/relying on them.
    google::ParseCommandLineFlags(&argc, &argv, true);
    google::InitGoogleLogging(argv[0]);

    std::cout << parsed_cli->dbg_str();
    auto circuit = get_input_circuit(*parsed_cli);
    if (!circuit)
    {
        std::cerr << "Not able to load circuit.\n";
        return 3;
    }

    VerifyCircuit("Verifying loaded circuit.", circuit.get(), "Circuit is valid.");

    if (parsed_cli->matched< opt::Dbg >())
        circuit = optimize< circ::DebugOptimizer >(std::move(circuit));
    else
        circuit = optimize< circ::DefaultOptimizer >(std::move(circuit));

    circ::log_info() << "Storing circuit.";
    gap::enforce(false) << "Controlled fail.";
    store_outputs(*parsed_cli, circuit);

    return 0;
}
