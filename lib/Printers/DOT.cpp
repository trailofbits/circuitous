/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>

#include <ostream>

namespace circuitous {
namespace {

static const char *const kBeginDOTNode =
    "[label=<<TABLE cellpadding=\"0\" cellspacing=\"0\" border=\"1\"><TR>";
static const char *const kEndDOTNode = "</TR></TABLE>>];\n";

class DOTPrinter : public UniqueVisitor<DOTPrinter> {
 public:
  explicit DOTPrinter(std::ostream &os_) : os(os_) {}

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
      os << " colspan=\"" << op->operands.Size() << "\"";
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

void PrintDOT(std::ostream &os, Circuit *circuit) {
  circuitous::DOTPrinter dot_os(os);
  dot_os.Visit(circuit);
}

}  // namespace circuitous
