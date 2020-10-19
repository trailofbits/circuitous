/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/Printers.h>
#include <circuitous/Transforms.h>
#include <gflags/gflags.h>
#include <glog/logging.h>

#include <fstream>
#include <iostream>
#include <unordered_map>

DECLARE_string(arch);
DECLARE_string(os);
DEFINE_string(binary_in, "",
              "Path to a file containing only machine code instructions.");
DEFINE_string(ir_in, "", "Path to a file containing serialized IR.");
DEFINE_string(ir_out, "", "Path to the output IR file.");
DEFINE_string(dot_out, "", "Path to the output GraphViz DOT file.");
DEFINE_string(python_out, "", "Path to the output Python file.");
DEFINE_string(smt_out, "", "Path to the output SMT-LIB2 file.");
DEFINE_string(json_out, "", "Path to the output JSON file.");
DEFINE_string(optimizations, "popcount2parity,reducepopcount", "Comma-separated list of optimizations to run");
DEFINE_bool(append, false, "Append to output IR files, rather than overwriting.");

namespace {

static const std::hash<std::string> kStringHasher;

void TopologySpecificIRPrinter(circuitous::Circuit *circuit) {
  std::unordered_map<uint64_t, std::ofstream> streams;
  circuit->Serialize([&] (const std::string &topology) -> std::ostream & {
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

}  // namespace

int main(int argc, char *argv[]) {
  google::ParseCommandLineFlags(&argc, &argv, true);
  google::InitGoogleLogging(argv[0]);

  std::unique_ptr<circuitous::Circuit> circuit;

  if (!FLAGS_binary_in.empty()) {
    circuitous::Circuit::CreateFromInstructions(FLAGS_arch, FLAGS_os,
                                                FLAGS_binary_in)
        .swap(circuit);

  } else if (!FLAGS_ir_in.empty()) {
    if (FLAGS_ir_in == "-") {
      FLAGS_ir_in = "/dev/stdin";
    }

    std::ifstream is(FLAGS_ir_in, std::ios::binary);
    circuitous::Circuit::Deserialize(is).swap(circuit);

  } else {
    std::cerr << "Expected one of `--binary_in` or `--ir_in`" << std::endl;
    return EXIT_FAILURE;
  }

  if (!circuit) {
    std::cerr << "Failed to get circuit IR" << std::endl;
    return EXIT_FAILURE;
  }
  

  // Optimize the circuit.
  std::stringstream ss;
  ss << FLAGS_optimizations;
  for (std::string opt_name; std::getline(ss, opt_name, ','); ) {
    if (opt_name == "popcount2parity") {
      ConvertPopCountToParity(circuit.get());

    } else if (opt_name == "reducepopcount") {
      StrengthReducePopulationCount(circuit.get());

    } else if (opt_name == "extractcommon") {
      ExtractCommonTopologies(circuit.get());
    }
  }

  circuit->RemoveUnused();

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
    }
  }

  if (!FLAGS_dot_out.empty()) {
    if (FLAGS_dot_out == "-") {
      FLAGS_dot_out = "/dev/stderr";
    }

    std::ofstream os(FLAGS_dot_out);
    circuitous::PrintDOT(os, circuit.get());
  }

  if (!FLAGS_python_out.empty()) {
    if (FLAGS_python_out == "-") {
      FLAGS_python_out = "/dev/stderr";
    }

    std::ofstream os(FLAGS_python_out);
    circuitous::PrintPython(os, circuit.get());
  }

  if (!FLAGS_smt_out.empty()) {
    if (FLAGS_smt_out == "-") {
      FLAGS_smt_out = "/dev/stderr";
    }

    std::ofstream os(FLAGS_smt_out);
    circuitous::PrintSMT(os, circuit.get(), false);
  }

  if (!FLAGS_json_out.empty()) {
    if (FLAGS_json_out == "-") {
      FLAGS_json_out = "/dev/stderr";
    }

    std::ofstream os(FLAGS_json_out);
    circuitous::PrintJSON(os, circuit.get());
  }
  return EXIT_SUCCESS;
}
