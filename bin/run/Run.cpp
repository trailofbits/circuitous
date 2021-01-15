/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <gflags/gflags.h>
#include <glog/logging.h>
#pragma clang diagnostic pop

#include <circuitous/IR/IR.h>

#include <fstream>
#include <iostream>

#include "Interpreter.h"

DEFINE_string(input, "", "Path to a serialized circuitous IR file.");

int main(int argc, char *argv[]) {
  std::stringstream usage;
  usage << std::endl
        << std::endl
        << "  " << argv[0] << " \\" << std::endl
        << "    --input INPUT_IR_FILE \\" << std::endl
        << std::endl

        // Print the version and exit.
        << "    [--version]" << std::endl
        << std::endl;

  google::InitGoogleLogging(argv[0]);
  google::InstallFailureSignalHandler();
  google::SetUsageMessage(usage.str());
  google::ParseCommandLineFlags(&argc, &argv, true);

  LOG_IF(ERROR, FLAGS_input.empty())
      << "Must specify path to an input serialized circuitous IR file.";

  if (FLAGS_input.empty()) {
    std::cerr << google::ProgramUsage();
    return EXIT_FAILURE;
  }

  std::ifstream is(FLAGS_input, std::ios::binary);  
  if (!is.good()) {
    LOG(ERROR) << "Error while opening input IR file.";
    return EXIT_FAILURE;
  }
  
  auto circuit{circuitous::Circuit::Deserialize(is)};

  circuitous::Interpreter run(circuit.get());

  run.SetInstructionBitsValue(0x89d8ULL);
  run.SetInputRegisterValue("RAX", 0ULL);
  run.SetInputRegisterValue("RBX", 1ULL);
  run.SetInputRegisterValue("RCX", 2ULL);
  run.SetInputRegisterValue("RDX", 3ULL);
  run.SetInputRegisterValue("RSI", 4ULL);
  run.SetInputRegisterValue("RDI", 5ULL);
  run.SetInputRegisterValue("RSP", 6ULL);
  run.SetInputRegisterValue("RBP", 7ULL);
  
  run.Run();

  DLOG(INFO) << "OUTPUT_RAX: " << run.GetOutputRegisterValue("RAX");

  return EXIT_SUCCESS;
}