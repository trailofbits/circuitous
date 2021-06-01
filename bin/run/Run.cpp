/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <gflags/gflags.h>
#include <glog/logging.h>

#include <llvm/Support/JSON.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
#pragma clang diagnostic pop

#include <circuitous/Run/Interpreter.h>
#include <circuitous/Run/Trace.hpp>

#include <circuitous/IR/Verify.hpp>
#include <circuitous/IR/IR.h>
#include <circuitous/Printers.h>

#include <fstream>
#include <iostream>
#include <unordered_map>

DEFINE_string(ir_in, "", "Path to a serialized circuitous IR file.");
DEFINE_string(json_in, "", "Path to an input state JSON file.");
DEFINE_string(json_out, "", "Path to an output state JSON file.");
DEFINE_string(dot_out, "", "Path to dump annotated dot file.");

DEFINE_string(singular_current, "", "Path to current entry in a singular form.");
DEFINE_string(singular_next, "", "Path to next entry in a singular form.");

DEFINE_string(export_derived, "", "Path to store derived values into.");

DEFINE_string(traces, "", "Path to traces");


DEFINE_bool(derive, false, "Derive mode");
DEFINE_bool(verify, false, "Verify mode");

DEFINE_bool(sim, false, "Interactive");

auto load_circ(const std::string &path) {
  // Read input circuit file
  std::ifstream ir(FLAGS_ir_in, std::ios::binary);
  if (!ir) {
    LOG(FATAL) << "Error while opening input IR file: " << std::strerror(errno);
  }

  // Deserialize circuit from binary IR file
  auto circuit = circuitous::Circuit::Deserialize(ir);

  const auto &[status, msg, warnings] = circuitous::VerifyCircuit(circuit.get());
  if (!status) {
    LOG(FATAL) << "Loaded IR is not valid -- Aborting.\n" << msg;
  }
  if (!warnings.empty()) {
    LOG(WARNING) << "Warnings produced while loading IR.";
    LOG(WARNING) << warnings;
  }

  return circuit;
}

auto load_singular(const std::string &path) -> std::optional<circuitous::run::trace::Entry> {
  if (path.empty()) {
    return {};
  }
  using namespace circuitous::run::trace;
  return std::make_optional(get_entry(0ul, load_json(path)));
}

template<typename Runner>
void run() {

  auto circuit = load_circ(FLAGS_ir_in);
  Runner run(circuit.get());

  if (auto current_trace = load_singular(FLAGS_singular_current)) {
    run.set_input_state(*current_trace);
  }
  if (auto next_trace = load_singular(FLAGS_singular_next)) {
    run.set_output_state(*next_trace);
  }
  auto result = run.Run();

  if (!FLAGS_export_derived.empty()) {
        // Open output file
    std::error_code ec;
    llvm::raw_fd_ostream output(FLAGS_export_derived, ec, llvm::sys::fs::F_Text);
    CHECK(!ec) << "Error while opening output state JSON file: " << ec.message();
    // Dump output register values to JSON

    llvm::json::Object output_obj;
    llvm::json::Object output_regs_obj;

    if (run.acceptor) {
      for (auto [reg, val] : run.acceptor->template get_derived<circuitous::OutputRegister>()) {
        CHECK(val);
        output_regs_obj[reg->reg_name] = std::to_string(val->getLimitedValue());
      }
      for (auto [_, val] : run.acceptor->template get_derived<circuitous::OutputErrorFlag>()) {
        output_obj["ebit"] = (val == llvm::APInt(1, 1));
      }
    }

    // Serialize
    output_obj["regs"] = std::move(output_regs_obj);
    output_obj["result"] = result;
    output << llvm::json::Value(std::move(output_obj));
  }

  //store_old_trace(FLAGS_json_out, run.get_output_state(), result);
  if (!FLAGS_dot_out.empty() && run.acceptor) {
    std::unordered_map<circuitous::Operation *, std::string> values;
    for (auto &[op, val] : run.values()) {
      values[op] = val.toString(16, false);
    }
    std::ofstream os(FLAGS_dot_out);
    circuitous::PrintDOT(os, circuit.get(), values);
  }
}


int main(int argc, char *argv[]) {
  std::stringstream usage;
  usage << std::endl
        << std::endl
        << "  " << argv[0] << " \\" << std::endl
        << "    --ir_in INPUT_IR_FILE \\" << std::endl
        << "    --json_in INPUT_JSON_FILE \\" << std::endl
        << "    --json_out OUTPUT_JSON_FILE \\" << std::endl
        << std::endl

        // Print the version and exit.
        << "    [--version]" << std::endl
        << std::endl;

  google::InitGoogleLogging(argv[0]);
  google::InstallFailureSignalHandler();
  google::SetUsageMessage(usage.str());
  google::ParseCommandLineFlags(&argc, &argv, true);

  LOG_IF(ERROR, FLAGS_ir_in.empty())
      << "Must specify path to an input serialized circuitous IR file.";

  if (FLAGS_ir_in.empty()) {
    std::cerr << google::ProgramUsage();
    return 1;
  }
  if (FLAGS_verify) {
    run<circuitous::run::VQueueInterpreter>();
  } else if (FLAGS_derive) {
    run<circuitous::run::DQueueInterpreter>();
  }

  return 0;
}