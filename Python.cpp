/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <ostream>

#include "IR.h"

namespace circuitous {

void PrintPython(std::ostream &os, Circuit *circuit) {
  os << "def circuit():\n"
     << "  operations = {}\n"
     << "  result = 'v" << std::hex << reinterpret_cast<uintptr_t>(circuit)
     << "'\n"
     << "  operations[result] = []\n";

  circuit->ForEachOperation([&](circuitous::Operation *op) {
    os << "  operations['v" << reinterpret_cast<uintptr_t>(op) << "'] = []\n";
  });

  auto do_op = [&](circuitous::Operation *op) {
    const auto id = reinterpret_cast<uintptr_t>(op);
    os << "  operations['v" << std::hex << id << "'].append(\"" << op->Name()
       << "\")\n"
       << "  operations['v" << std::hex << id << "'].append(" << std::dec
       << static_cast<unsigned>(op->op_code) << ")\n"
       << "  operations['v" << std::hex << id << "'].append(" << std::dec
       << op->size << ")\n";

    if (auto llvm_op = dynamic_cast<circuitous::LLVMOperation *>(op); llvm_op) {
      os << "  operations['v" << std::hex << id << "'].append(" << std::dec
         << llvm_op->llvm_op_code << ")\n"
         << "  operations['v" << std::hex << id << "'].append(" << std::dec
         << llvm_op->llvm_predicate << ")\n";

    } else if (auto extract_op = dynamic_cast<circuitous::Extract *>(op);
               extract_op) {
      os << "  operations['v" << std::hex << id << "'].append(" << std::dec
         << extract_op->high_bit_exc << ")\n"
         << "  operations['v" << std::hex << id << "'].append(" << std::dec
         << extract_op->low_bit_inc << ")\n";
    }

    for (auto sub_op : op->operands) {
      os << "  operations['v" << std::hex << id << "'].append('v"
         << reinterpret_cast<uintptr_t>(sub_op) << std::dec << "')\n";
    }
  };

  circuit->ForEachOperation(do_op);
  do_op(circuit);

  //os << "  return v" << std::hex << reinterpret_cast<uintptr_t>(circuit.get())
  os << "  return operations"
     << "\n\n"
     << std::dec;
}

}  // namespace circuitous
