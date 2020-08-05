/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <gflags/gflags.h>
#include <glog/logging.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/MemoryBuffer.h>
#include <remill/Arch/Arch.h>
#include <remill/BC/Compat/Error.h>
#include <remill/BC/Util.h>

#include <fstream>
#include <iostream>

#include "CircuitBuilder.h"

DECLARE_string(arch);
DECLARE_string(os);
DEFINE_string(binary_in, "",
              "Path to a file containing only machine code instructions.");
DEFINE_string(ir_in, "", "Path to a file containing serialized IR.");
DEFINE_string(ir_out, "", "Path to the output IR file.");
DEFINE_string(dot_out, "", "Path to the output GraphViz DOT file.");
DEFINE_string(python_out, "", "Path to the output Python file.");

namespace circuitous {

void PrintDOT(std::ostream &os, Circuit *circuit);
void PrintPython(std::ostream &os, Circuit *circuit);

}  // namespace circuitous

int main(int argc, char *argv[]) {
  google::ParseCommandLineFlags(&argc, &argv, true);
  google::InitGoogleLogging(argv[0]);

  std::unique_ptr<circuitous::Circuit> circuit;

  if (!FLAGS_binary_in.empty()) {
    auto maybe_buff = llvm::MemoryBuffer::getFile(FLAGS_binary_in, -1, false);
    if (remill::IsError(maybe_buff)) {
      std::cerr << remill::GetErrorString(maybe_buff) << std::endl;
      return EXIT_FAILURE;
    }

    const auto buff = remill::GetReference(maybe_buff)->getBuffer();

    circuitous::CircuitBuilder builder([](llvm::LLVMContext &context) {
      return remill::Arch::GetTargetArch(context);
    });

    builder.Build(buff).swap(circuit);

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

  if (!FLAGS_ir_out.empty()) {
    if (FLAGS_ir_out == "-") {
      FLAGS_ir_out = "/dev/stdout";
    }

    std::ofstream os(FLAGS_ir_out, std::ios::binary | std::ios::trunc);
    circuit->Serialize(os);
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

  return EXIT_SUCCESS;
}
