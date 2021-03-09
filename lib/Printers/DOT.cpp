/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>

#include <ostream>
#include <unordered_map>

namespace circuitous {
namespace {

static const char *const kBeginDOTNode =
    "[label=<<TABLE cellpadding=\"0\" cellspacing=\"0\" border=\"1\"><TR>";
static const char *const kEndDOTNode = "</TR></TABLE>>];\n";

class DOTPrinter : public UniqueVisitor<DOTPrinter> {
  using value_map_t = std::unordered_map<Operation *, std::string>;
 public:
  explicit DOTPrinter(std::ostream &os_, const value_map_t &vals)
    : os(os_), node_values(vals) {}

  void PrintOperands(Operation *op) {
    if (!op->operands.Empty()) {
      os << "</TR><TR>";
      for (auto sub_op : op->operands) {
        os << "<TD port=\"s";
        os << reinterpret_cast<uintptr_t>(sub_op);
        os << "\"> &nbsp; </TD>";
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
    os << ">" << op->Name();
    if (node_values.count(op)) {
      os << " = " << node_values.find(op)->second;
    }
    os << "</TD>";
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
  const value_map_t &node_values;
};

}  // namespace

void PrintDOT(std::ostream &os, Circuit *circuit,
              const std::unordered_map<Operation *, std::string> &node_values) {
  circuitous::DOTPrinter dot_os(os, node_values);
  dot_os.Visit(circuit);
}

}  // namespace circuitous
