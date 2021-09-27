/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#include <circuitous/Util/Logging.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/Support/JSON.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <circuitous/Run/Inspect.hpp>
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
DEFINE_string(memory, "", "addr,hex_val");


DEFINE_bool(derive, false, "Derive mode");
DEFINE_bool(verify, false, "Verify mode");
DEFINE_bool(interactive, false, "Interactive mode");
DEFINE_string(inspect, "", "Inspect failing test case");

DEFINE_bool(sim, false, "Interactive");

DEFINE_bool(die, false, "DBG: Artificially kill program and dump state of all runners.");

auto load_circ(const std::string &path) {
  // Read input circuit file
  std::ifstream ir(FLAGS_ir_in, std::ios::binary);
  if (!ir) {
    LOG(FATAL) << "Error while opening input IR file: " << std::strerror(errno);
  }

  // Deserialize circuit from binary IR file
  auto circuit = circ::Circuit::Deserialize(ir);

  const auto &[status, msg, warnings] = circ::VerifyCircuit(circuit.get());
  if (!status) {
    LOG(FATAL) << "Loaded IR is not valid -- Aborting.\n" << msg;
  }
  if (!warnings.empty()) {
    LOG(WARNING) << "Warnings produced while loading IR.";
    LOG(WARNING) << warnings;
  }

  return circuit;
}

auto load_singular(const std::string &path) -> std::optional<circ::run::trace::Entry> {
  if (path.empty()) {
    return {};
  }
  using namespace circ::run::trace;
  return std::make_optional(get_entry(0ul, load_json(path)));
}

template<typename I>
void export_derived(I inspect) {
  // Open output file
  std::error_code ec;
  llvm::raw_fd_ostream output(FLAGS_export_derived, ec, llvm::sys::fs::F_Text);
  CHECK(!ec) << "Error while opening output state JSON file: " << ec.message();

  // Dump output register values to JSON
  llvm::json::Object output_obj;
  llvm::json::Object output_regs_obj;
  llvm::json::Object output_mem_hints;

  auto as_str = [](const auto &what) -> std::string {
    return std::to_string(what->getLimitedValue());
  };

  if (inspect.focus()) {
    for (auto [reg, val] : inspect->template get_derived<circ::OutputRegister>()) {
      CHECK(val);
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

template<typename Runner>
void run() {

  auto circuit = load_circ(FLAGS_ir_in);
  Runner run(circuit.get());

  if (auto current_trace = load_singular(FLAGS_singular_current)) {
    run.set_input_state(*current_trace);
    for (auto [addr, data] : current_trace->initial_memory) {
      run.set_memory(addr, data);
    }
  }
  if (auto next_trace = load_singular(FLAGS_singular_next)) {
    run.set_output_state(*next_trace);
  }

  if (!FLAGS_memory.empty()) {
    llvm::StringRef s_ref(FLAGS_memory);
    auto [addr, val] = s_ref.split(',');
    run.set_memory(std::strtoull(addr.data(), nullptr, 16), val.str());
  }

  run.Run();
  if (FLAGS_die)
    LOG(FATAL) << run.dump_runners();

  if (!FLAGS_export_derived.empty()) {
    export_derived(circ::run::Inspector<Runner>(&run));
  }

  if (!FLAGS_dot_out.empty() && run.acceptor) {
    std::unordered_map<circ::Operation *, std::string> values;
    for (auto &[op, val] : run.values()) {
      values[op] = (val) ? val->toString(16, false) : std::string("{ undef }");
    }
    std::ofstream os(FLAGS_dot_out);
    circ::PrintDOT(os, circuit.get(), values);
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
    run<circ::run::VQueueInterpreter>();
  } else if (FLAGS_derive) {
    run<circ::run::DQueueInterpreter>();
  }

  return 0;
}
