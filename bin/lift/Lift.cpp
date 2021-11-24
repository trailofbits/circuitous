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

#include <fstream>
#include <iostream>
#include <unordered_map>

DECLARE_string(arch);
DECLARE_string(os);

DEFINE_string(ir_in, "", "Path to a file containing serialized IR.");
DEFINE_string(smt_in, "", "Path to the input smt2 file.");

DEFINE_string(ir_out, "", "Path to the output IR file.");
DEFINE_string(dot_out, "", "Path to the output GraphViz DOT file.");
DEFINE_string(python_out, "", "TODO(luaks): Needs update");
DEFINE_string(smt_out, "", "Path to the output smt2 file.");
DEFINE_string(json_out, "", "Path to the output JSON file.");
DEFINE_string(verilog_out, "", "Path to the output verilog file.");

DEFINE_string(bitblast_smt_out, "", "Path to the output smt2 file.");
DEFINE_bool(bitblast_stats, false, "Print smt bitblast statistics.");

DEFINE_string(bytes_in, "", "Hex representation of bytes to be lifted");
DEFINE_string(seed_dbg_in, "", "Load input from circuitous-seed --dbg produced file");


DEFINE_bool(eqsat, false, "Enable equality saturation based optimizations.");
DEFINE_bool(dbg, false, "Enable various debug dumps");

namespace {

struct DefaultLog {

  // TOOD(lukas): Make it work.
  //static inline const constexpr char separator = ' ';

  template<typename ...Args>
  static void log(Args &&...args) {
    (LOG(INFO) << ... << args);
  }

  template<typename ...Args>
  static void fail(Args &&...args) {
    (LOG(ERROR) << ... << args);
  }

  static void kill() { LOG(FATAL) << "Aborted"; }

  template<typename ...Args>
  static void kill(Args &&...args) {
    if constexpr (sizeof...(Args) == 0) {
      return kill();
    } else {
      return (LOG(FATAL) << ... << std::forward<Args>(args));
    }
  }
};

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


std::unique_ptr<circ::Circuit> LoadCircuit() {
  auto make_circuit = [&](auto buf) {
    return circ::Circuit::make_circuit(FLAGS_arch, FLAGS_os, buf);
  };

  if (!FLAGS_bytes_in.empty()) {
    std::vector<uint8_t> buf;
    for (auto i = 0U; i < FLAGS_bytes_in.size(); i += 2) {
      std::string aux = {FLAGS_bytes_in[i], FLAGS_bytes_in[i + 1]};
      auto casted = static_cast<uint8_t>(std::strtoul(aux.data(), nullptr, 16));
      buf.push_back(casted);
    }
    auto as_sv = std::string_view( reinterpret_cast<char *>(buf.data()), buf.size());
    return make_circuit(as_sv);
  }

  if (!FLAGS_ir_in.empty()) {
    if (FLAGS_ir_in == "-") {
      FLAGS_ir_in = "/dev/stdin";
    }

    std::ifstream is(FLAGS_ir_in, std::ios::binary);
    if (!is.good()) {
      LOG(ERROR) << "Error while opening input IR file.";
      return {};
    }
    return circ::Circuit::Deserialize(is);
  }

  if (!FLAGS_smt_in.empty()) {
    if (FLAGS_ir_in == "-") {
      FLAGS_ir_in = "/dev/stdin";
    }

    return circ::smt::deserialize(FLAGS_smt_in);
  }


  if (!FLAGS_seed_dbg_in.empty()) {
    std::vector< uint8_t > buf;
    for (const auto &bytes : load_seed_dbg(FLAGS_seed_dbg_in))
      add_to_buffer(buf, bytes);
    return make_circuit(std::string_view( reinterpret_cast< char * >(buf.data()), buf.size()));
  }

  LOG(WARNING) << "Expected one of `--binary_in` or `--ir_in` or `--smt_in` or `--bytes_int`"
               << std::endl;
  return {};
}

using CircuitPtr = circ::CircuitPtr;

// Optimize the circuit.
template< typename Optimizer >
circ::CircuitPtr Optimize(CircuitPtr &&circuit)
{
  Optimizer opt;

  // Populate by default passes we want to always run
  if (FLAGS_eqsat) {
    opt.add_pass("eqsat");
  }

  // opt.add_pass("merge-advices");
  // opt.add_pass("dummy-pass");
  auto result = opt.run(std::move(circuit));
  LOG(INFO) << "Optimizations done.";
  LOG(INFO) << opt.report();
  return result;
}

}  // namespace


int main(int argc, char *argv[]) {
  google::ParseCommandLineFlags(&argc, &argv, true);
  google::InitGoogleLogging(argv[0]);

  auto circuit = LoadCircuit();
  if (!circuit) {
    std::cerr << "Failed to get circuit IR" << std::endl;
    return EXIT_FAILURE;
  }

  VerifyCircuit("Verifying loaded circuit.", circuit.get(), "Circuit is valid.");

  if (FLAGS_dbg) {
    LOG(INFO) << "Debug dumping before optimizations -- debug.* family.";
    VerifyCircuit("Verifying.", circuit.get());
    std::ofstream os("debug.json");
    circ::PrintJSON(os, circuit.get());
    os.flush();

    std::ofstream dos("debug.dot");
    circ::PrintDOT(dos, circuit.get());
    dos.flush();
    LOG(INFO) << "Debug dumping finished.";
  }


  LOG(INFO) << "Debug mode: " << FLAGS_dbg;

  if (FLAGS_dbg) {
    circuit = Optimize< circ::DebugOptimizer< DefaultLog > >(std::move(circuit));
  } else {
    circuit = Optimize< circ::DefaultOptimizer< DefaultLog > >(std::move(circuit));
  }


  if (!FLAGS_ir_out.empty()) {
    if (FLAGS_ir_out == "-") {
      FLAGS_ir_out = "/dev/stdout";
    }
    std::ofstream os(
        FLAGS_ir_out,
        std::ios::binary | std::ios::trunc);
    circuit->Serialize(os);
    if (FLAGS_dbg){
      LOG(INFO) << "Proceeding to reload test";
      std::ifstream is(FLAGS_ir_out, std::ios::binary);
      if (!is.good()) {
        LOG(FATAL) << "Error while re-checking.";
      }
      auto x = circ::Circuit::Deserialize(is);
      VerifyCircuit("Verifying loaded.", x.get(), "Reload test successful.");
      CHECK(x->ptr_size == circuit->ptr_size);
    }
  }

  if (!FLAGS_json_out.empty()) {
    if (FLAGS_json_out == "-") {
      FLAGS_json_out = "/dev/stderr";
    }
    LOG(INFO) << "Printing JSON";
    std::ofstream os(FLAGS_json_out);
    circ::PrintJSON(os, circuit.get());
    LOG(INFO) << "Done";
  }

  if (!FLAGS_dot_out.empty()) {
    if (FLAGS_dot_out == "-") {
      FLAGS_dot_out = "/dev/stderr";
    }

    LOG(INFO) << "Printing dot";
    std::ofstream os(FLAGS_dot_out);
    circ::PrintDOT(os, circuit.get());
    LOG(INFO) << "Done";
  }

  if (!FLAGS_python_out.empty()) {
    if (FLAGS_python_out == "-") {
      FLAGS_python_out = "/dev/stderr";
    }

    LOG(INFO) << "Printing python";
    std::ofstream os(FLAGS_python_out);
    circ::PrintPython(os, circuit.get());
    LOG(INFO) << "Done";
  }

  if (!FLAGS_verilog_out.empty()) {
    LOG(INFO) << "Printing smt";
    std::ofstream os(FLAGS_verilog_out);
    circ::print::verilog::print(os, "circuit", circuit.get());
    LOG(INFO) << "Done.";
  }

  if (!FLAGS_smt_out.empty()) {
    if (FLAGS_smt_out == "-") {
      FLAGS_smt_out = "/dev/stderr";
    }
    LOG(INFO) << "Printing smt";
    std::ofstream os(FLAGS_smt_out);
    circ::PrintSMT(os, circuit.get());
    LOG(INFO) << "Done.";
  }

  if (!FLAGS_bitblast_smt_out.empty()) {
    if (FLAGS_bitblast_smt_out == "-") {
      FLAGS_bitblast_smt_out = "/dev/stderr";
    }
    LOG(INFO) << "Printing bit-blasted smt";
    std::ofstream os(FLAGS_bitblast_smt_out);
    circ::PrintBitBlastSMT(os, circuit.get());
    LOG(INFO) << "Done.";
  }

  if (FLAGS_bitblast_stats) {
    std::cout << circ::get_stats(circuit.get()) << '\n';
  }

  return EXIT_SUCCESS;
}
