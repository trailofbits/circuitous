/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <glog/logging.h>
#include <gflags/gflags.h>

#include <iostream>
#include <fstream>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/MemoryBuffer.h>

#include <remill/Arch/Arch.h>
#include <remill/BC/Compat/Error.h>
#include <remill/BC/Util.h>

#include "CircuitBuilder.h"

DECLARE_string(arch);
DECLARE_string(os);
DEFINE_string(binary_in, "", "Path to a file containing only machine code instructions.");
DEFINE_string(dot_out, "", "Path to the output GraphViz DOT file.");
DEFINE_string(python_out, "", "Path to the output Python file.");

namespace circuitous {
namespace {

static const char * const kBeginDOTNode = "[label=<<TABLE cellpadding=\"0\" cellspacing=\"0\" border=\"1\"><TR>";
static const char * const kEndDOTNode = "</TR></TABLE>>];\n";

class DOTPrinter : public UniqueVisitor<DOTPrinter> {
 public:
  explicit DOTPrinter(std::ostream &os_)
      : os(os_) {}

  void PrintOperands(Operation *op) {
    if (!op->operands.Empty()) {
      os << "</TR><TR>";
      for (auto sub_op : op->operands) {
        const auto sub_id = reinterpret_cast<uintptr_t>(sub_op);
        os << "<TD port=\"s" << sub_id << "\"> &nbsp; </TD>";
      }
    }
    os << kEndDOTNode;
    const auto id = reinterpret_cast<uintptr_t>(op);
    for (auto sub_op : op->operands) {
      const auto sub_id = reinterpret_cast<uintptr_t>(sub_op);
      os << 'o' << id << ":s" << sub_id << " -> o" << sub_id << ":id;\n";
    }
  }

  void PrintNodeName(Operation *op) {
    const auto id = reinterpret_cast<uintptr_t>(op);
    os << "o" << id << " " << kBeginDOTNode << "<TD port=\"id\"";
    if (!op->operands.Empty()) {
      os << " colspan=\"" << op->operands.Size()<< "\"";
    }
    os << ">" << op->Name() << "</TD>";
  }

  void VisitOperation(Operation *op) {
    op->Traverse(*this);
    PrintNodeName(op);
    PrintOperands(op);
  }

  void VisitCircuit(Circuit *op) {
    os << "digraph {\n"
       << "node [shape=plain];\n";
    op->Traverse(*this);
    PrintNodeName(op);
    PrintOperands(op);
    os << "}\n";
  }

 private:
  std::ostream &os;
};

}  // namespace
}  // namespace circuitous

int main(int argc, char *argv[]) {
  google::ParseCommandLineFlags(&argc, &argv, true);
  google::InitGoogleLogging(argv[0]);


  auto maybe_buff = llvm::MemoryBuffer::getFile(FLAGS_binary_in, -1, false);
  if (remill::IsError(maybe_buff)) {
    std::cerr << remill::GetErrorString(maybe_buff) << std::endl;
    return EXIT_FAILURE;
  }

  const auto buff = remill::GetReference(maybe_buff)->getBuffer();

  circuitous::CircuitBuilder builder([] (llvm::LLVMContext &context) {
    return remill::Arch::GetTargetArch(context);
  });

  auto circuit = builder.Build(buff);

  if (!FLAGS_dot_out.empty()) {
    if (FLAGS_dot_out == "-") {
      FLAGS_dot_out = "/dev/stderr";
    }

    std::ofstream os(FLAGS_dot_out);
    circuitous::DOTPrinter dot_os(os);
    dot_os.Visit(circuit.get());
  }

  if (!FLAGS_python_out.empty()) {
    if (FLAGS_python_out == "-") {
      FLAGS_python_out = "/dev/stderr";
    }

    std::ofstream os(FLAGS_python_out);
    os << "def circuit():\n"
       << "  v" << std::hex << reinterpret_cast<uintptr_t>(circuit.get()) << " = []\n";

    circuit->ForEachOperation([&] (circuitous::Operation *op) {
      os << "  v" << reinterpret_cast<uintptr_t>(op) << " = []\n";
    });

    auto do_op = [&] (circuitous::Operation *op) {
      const auto id = reinterpret_cast<uintptr_t>(op);
      os << "  v" << id << ".append(\"" << op->Name() << ")\n"
         << "  v" << id << ".append(" << static_cast<unsigned>(op->op_code) << ")\n"
         << "  v" << id << ".append(" << op->size << ")\n";

      if (auto llvm_op = dynamic_cast<circuitous::LLVMOperation *>(op); llvm_op) {
        os << "  v" << id << ".append(" << llvm_op->llvm_op_code << ")\n"
           << "  v" << id << ".append(" << llvm_op->llvm_predicate << ")\n";
      }

      for (auto sub_op : op->operands) {
        os << "  v" << id << ".append(v"
           << reinterpret_cast<uintptr_t>(sub_op) << ")\n";
      }
    };

    circuit->ForEachOperation(do_op);
    do_op(circuit.get());

    os << "  return v" << std::hex << reinterpret_cast<uintptr_t>(circuit.get())
       << "\n\n" << std::dec;
  }

//  for (auto &block : circuit_func) {
//    for (auto &inst : block) {
//      if (auto cmp_inst = llvm::dyn_cast<llvm::CmpInst>(&inst);
//          cmp_inst &&
//          (cmp_inst->getPredicate() == llvm::CmpInst::ICMP_EQ ||
//           cmp_inst->getPredicate() == llvm::CmpInst::FCMP_OEQ) &&
//          llvm::isa<llvm::Argument>(cmp_inst->getOperand(0)) &&
//          llvm::isa<llvm::Argument>(cmp_inst->getOperand(1))) {
//
//      }
//    }
//  }

//  circuit_func->print(llvm::outs());


  return EXIT_SUCCESS;
}
