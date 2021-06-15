/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Circuit.hpp>

#include <ostream>
#include <unordered_map>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <glog/logging.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#pragma clang diagnostic pop

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
        os << sub_op->id();
        os << "\"> &nbsp; </TD>";
      }
    }
    os << kEndDOTNode;
    for (auto sub_op : op->operands) {
      os << 'o' << op->id() << ":s" << sub_op->id() << " -> o" << sub_op->id() << ":id;\n";
    }
  }

  void PrintNodeName(Operation *op) {
    os << "o" << op->id() << " " << kBeginDOTNode << "<TD port=\"id\"";
    if (!op->operands.Empty()) {
      os << " colspan=\"" << op->operands.Size() << "\"";
    }
    os << ">" << op->Name();
    if (node_values.count(op)) {
      os << " = " << node_values.find(op)->second;
    }
    os << "</TD>";
  }

  void Visit(Operation *op) {
    op->Traverse(*this);
    PrintNodeName(op);
    PrintOperands(op);
  }

  void Visit(Circuit *op) {
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

namespace dot {

struct Printer : UniqueVisitor<Printer> {
  using value_map_t = std::unordered_map<Operation *, std::string>;
  explicit Printer(std::ostream &os_, const value_map_t &vals)
    : os(os_), node_values(vals) {}


  std::string Operand(Operation *of, std::size_t i) {
    return NodeID(of) + ':' + NodeID(of) + std::to_string(i);
  }

  void Edge(Operation *from, Operation *to, std::size_t i) {
    os << Operand(from, i)
       << " -> "
       << NodeID(to)
       << ";\n";
  }

  std::string NodeID(Operation *op) {
    return "v" + std::to_string(op->id()) + "v";
  }

  std::string AsID(const std::string &what) {
    return "<" + what + ">";
  }

  void Node(Operation *op) {

    if (auto lop = dynamic_cast<Add *>(op)) {
      os << NodeID(op) << " [fillcolor=red;style=filled;label = " << '"'
        << "{ " << AsID(NodeID(op)) << " " << op->Name();
    } else {
      os << NodeID(op) << " [label = " << '"'
        << "{ " << AsID(NodeID(op)) << " " << op->Name();
    }
    if (node_values.count(op)) {
      os << " " << node_values.find(op)->second << " ";
    }

    if (op->operands.size() == 0) {
      os << " }" << '"' << "];\n";
      return;
    }

    os << "| {";
    for (std::size_t i = 0; i < op->operands.size(); ++i) {
      os << AsID(NodeID(op) + std::to_string(i));
      if (i != op->operands.size() - 1) {
        os << " | ";
      }
    }
    os << " }}" << '"' << "];\n";
  }

  void Init() {
    os << "digraph {" << std::endl;
    os << "node [shape=record];";
  }

  void Visit(Operation *op) {
    op->Traverse(*this);
    Node(op);
    for (std::size_t i = 0; i < op->operands.size(); ++i) {
      Edge(op, op->operands[i], i);
    }
  }

  void Visit(Circuit *op) {
    Init();
    op->Traverse(*this);
    Node(op);
    for (std::size_t i = 0; i < op->operands.size(); ++i) {
      Edge(op, op->operands[i], i);
    }
    os << "}";
  }

  std::ostream &os;
  const value_map_t &node_values;
};

} // namespace dot

void PrintDOT(std::ostream &os, Circuit *circuit,
              const std::unordered_map<Operation *, std::string> &node_values) {
  circuitous::dot::Printer dot_os(os, node_values);
  dot_os.Visit(circuit);
}

}  // namespace circuitous
