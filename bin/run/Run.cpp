/*
 * Copyright (c) 2020 Trail of Bits, Inc.
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

#include "Interpreter.h"

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

namespace {

template <typename T>
inline static std::string GetErrorString(llvm::ErrorOr<T> &val) {
  return val.getError().message();
}

inline static std::string GetErrorString(llvm::Error &val) {
  std::string err;
  llvm::raw_string_ostream os(err);
  llvm::handleAllErrors(std::move(val),
                        [&os](llvm::ErrorInfoBase &eib) { eib.log(os); });
  os.flush();
  return err;
}

template <typename T>
inline static std::string GetErrorString(llvm::Expected<T> &val) {
  auto err = val.takeError();
  return GetErrorString(err);
}

}  // namespace

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

  if (FLAGS_ir_in.empty() || FLAGS_json_in.empty()) {
    std::cerr << google::ProgramUsage();
    return EXIT_FAILURE;
  }
  // Read input circuit file
  std::ifstream ir(FLAGS_ir_in, std::ios::binary);
  if (!ir) {
    LOG(ERROR) << "Error while opening input IR file: " << std::strerror(errno);
    return EXIT_FAILURE;
  }

  // Read input state JSON file
  auto maybe_buff{llvm::MemoryBuffer::getFile(FLAGS_json_in)};
  if (!maybe_buff) {
    LOG(ERROR) << "Error while opening input state JSON file: "
               << GetErrorString(maybe_buff);
    return EXIT_FAILURE;
  }

  // Parse JSON
  auto maybe_json{llvm::json::parse(maybe_buff.get()->getBuffer())};
  if (!maybe_json) {
    LOG(ERROR) << "Error while parsing state JSON file: "
               << GetErrorString(maybe_json);
    return EXIT_FAILURE;
  }

  // Get top level JSON object
  auto input_obj{maybe_json.get().getAsObject()};
  CHECK(input_obj) << "Invalid input state JSON object";

  // Get input instruction bits from JSON value
  auto inst{input_obj->getString("instruction_bits")};
  CHECK(inst) << "Invalid instruction bits JSON value";

  // Get error bit from JSON
  auto ebit{ input_obj->getBoolean("ebit") };
  CHECK(ebit) << "Invalid ebit JSON value";

  // Get input register values from JSON values
  auto input_regs_obj{input_obj->getObject("input_regs")};
  CHECK(input_regs_obj) << "Invalid input registers JSON object";

  // Deserialize circuit from binary IR file
  auto circuit{circuitous::Circuit::Deserialize(ir)};

  const auto &[status, msg, warnings] = circuitous::VerifyCircuit(circuit.get());
  if (!status) {
    LOG(FATAL) << "Loaded IR is not valid -- Aborting.\n" << msg;
  }
  if (!warnings.empty()) {
    LOG(WARNING) << "Warnings produced while loading IR.";
    LOG(WARNING) << warnings;
  }

  circuitous::QueueInterpreter run(circuit.get());
  // Initialize instruction bits
  run.SetInstructionBitsValue((*inst).str());
  run.SetInputEbit(*ebit);
  // Initialize input register state
  for (auto [obj_key, obj_val] : *input_regs_obj) {
    auto reg_name{obj_key.str()};
    auto raw_reg_val{obj_val.getAsString()};
    CHECK(raw_reg_val) << "Invalid JSON value for register " << reg_name;
    uint64_t reg_val = std::strtoull(raw_reg_val->data(), nullptr, 10);
    run.SetInputRegisterValue(reg_name, reg_val);
  }
  // Run circuit
  auto result = run.Run();
  if (result) {
    LOG(INFO) << "Success!";
  } else {
    LOG(INFO) << "Fail!";
  }
  // Print JSON output
  if (!FLAGS_json_out.empty()) {
    // Open output file
    std::error_code ec;
    llvm::raw_fd_ostream output(FLAGS_json_out, ec, llvm::sys::fs::F_Text);
    CHECK(!ec) << "Error while opening output state JSON file: "
               << ec.message();
    // Dump output register values to JSON
    llvm::json::Object output_regs_obj;
    for (auto reg : circuit->Attr<circuitous::OutputRegister>()) {
      auto key{reg->reg_name};
      if (auto val = run.GetOutputRegisterValue(key)) {
        output_regs_obj[key] = std::to_string(*val);
      }
    }
    // Serialize
    llvm::json::Object output_obj;
    output_obj["output_regs"] = std::move(output_regs_obj);
    output_obj["result"] = result;
    output_obj["inst_bytes"] = *inst;
    if (auto ebit = run.GetOutputErrorFlagValue()) {
      output_obj["ebit"] = *ebit;
    } else {
      LOG(FATAL) << "Output ebit is undefined!";
    }
    output << llvm::json::Value(std::move(output_obj));
  }

  if (!FLAGS_dot_out.empty()) {
    std::unordered_map<circuitous::Operation *, std::string> values;
    for (auto &[op, val] : run.values()) {
      values[op] = val.toString(16, false);
    }
    std::ofstream os(FLAGS_dot_out);
    circuitous::PrintDOT(os, circuit.get(), values);
  }

  return EXIT_SUCCESS;
}