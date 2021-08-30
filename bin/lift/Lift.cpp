/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Verify.hpp>
#include <circuitous/IR/SMT.hpp>
#include <circuitous/Printers.h>
#include <circuitous/Transforms.h>
#include <circuitous/IR/Cost.hpp>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <gflags/gflags.h>
#include <glog/logging.h>
#pragma clang diagnostic pop

#include <fstream>
#include <iostream>
#include <unordered_map>

DECLARE_string(arch);
DECLARE_string(os);

DEFINE_string(ir_in, "", "Path to a file containing serialized IR.");
DEFINE_string(ir_out, "", "Path to the output IR file.");
DEFINE_string(dot_out, "", "Path to the output GraphViz DOT file.");
DEFINE_string(python_out, "", "TODO(luaks): Needs update");
DEFINE_string(smt_out, "", "Path to the output smt2 file.");
DEFINE_string(smt_in, "", "Path to the input smt2 file.");
DEFINE_string(bitblast_smt_out, "", "Path to the output smt2 file.");
DEFINE_string(json_out, "", "Path to the output JSON file.");
DEFINE_string(optimizations, "",
              "TODO(lukas): Not supported atm");
DEFINE_bool(append, false,
            "Append to output IR files, rather than overwriting.");

DEFINE_string(bytes_in, "", "Hex representation of bytes to be lifted");

DEFINE_bool(dbg, false, "Enable various debug dumps");

DEFINE_bool(bitblast_stats, false, "Print smt bitblast statistics.");

namespace {

static const std::hash<std::string> kStringHasher;

void TopologySpecificIRPrinter(circ::Circuit *circuit) {
  std::unordered_map<uint64_t, std::ofstream> streams;
  LOG(INFO) << "Veryfing before serialization";
  VerifyCircuit(circuit);
  LOG(INFO) << "Valid";
  circuit->Serialize([&](const std::string &topology) -> std::ostream & {
    const auto hash = kStringHasher(topology);
    auto it = streams.find(hash);
    if (it == streams.end()) {
      std::stringstream ss;
      for (auto c : FLAGS_ir_out) {
        if (c == '%') {
          ss << std::hex << hash << std::dec;
        } else {
          ss << c;
        }
      }

      std::ofstream os(
          ss.str(),
          std::ios::binary | (FLAGS_append ? std::ios::app : std::ios::trunc));
      auto added = false;
      std::tie(it, added) = streams.emplace(hash, std::move(os));
      CHECK(added);
    }

    return it->second;
  });
}

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

  LOG(WARNING) << "Expected one of `--binary_in` or `--ir_in` or `--smt_in` or `--bytes_int`" << std::endl;
  return {};
}

// Optimize the circuit.
template<typename Optimizer>
void Optimize(circ::Circuit *circuit) {
  Optimizer opt_manager;

  // Populate by default passes we want to always run
  //opt_manager.AddPass("dagify");
  opt_manager.AddPass("popcount2parity");
  opt_manager.AddPass("reducepopcount");
  // TODO(lukas): Broken fix.
  //opt_manager.AddPass("extractcommon");
  opt_manager.AddPass("eqsat");
  opt_manager.AddPass("depbreaker");

  opt_manager.Run(circuit);
  LOG(INFO) << "Optimizations done.";
  LOG(INFO) << opt_manager.Stats();
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
    Optimize<circ::DebugOptimizer<DefaultLog>>(circuit.get());
  } else {
    Optimize<circ::DefaultOptimizer<DefaultLog>>(circuit.get());
  }


  if (!FLAGS_ir_out.empty()) {
    if (FLAGS_ir_out == "-") {
      FLAGS_ir_out = "/dev/stdout";
    }

    // If the output IR file name has a `%` in it, then we'll use that as a
    // signal that we want to group the output files by topology, so that
    // later we can optimize within a cohort, then merge, then optimize.
    if (FLAGS_ir_out.find("%") != std::string::npos) {
      TopologySpecificIRPrinter(circuit.get());

    } else {
      std::ofstream os(
          FLAGS_ir_out,
          std::ios::binary | (FLAGS_append ? std::ios::app : std::ios::trunc));
      circuit->Serialize(os);
      if (FLAGS_dbg){
        LOG(INFO) << "Proceeding to reload test";
        std::ifstream is(FLAGS_ir_out, std::ios::binary);
        if (!is.good()) {
          LOG(FATAL) << "Error while re-checking.";
        }
        auto x = circ::Circuit::Deserialize(is);
        VerifyCircuit("Verifyinh loaded.", x.get(), "Reload test successful.");
        CHECK(x->ptr_size == circuit->ptr_size);
      }
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
