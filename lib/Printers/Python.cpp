/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Circuit.hpp>

#include <ostream>

namespace circ {

void PrintPython(std::ostream &os, Circuit *circuit) {
  os << "def circuit():\n"
     << "  operations = {}\n"
     << "  result = 'v" << std::hex << reinterpret_cast<uintptr_t>(circuit)
     << "'\n"
     << "  operations[result] = []\n";

  circuit->ForEachOperation([&](circ::Operation *op) {
    os << "  operations['v" << op->id() << "'] = []\n";
  });

  auto do_op = [&](circ::Operation *op) {
    const auto id = op->id();
    os << "  operations['v" << std::hex << id << "'].append(\"" << op->Name()
       << "\")\n"
       << "  operations['v" << std::hex << id << "'].append(" << std::dec
       << static_cast<unsigned>(op->op_code) << ")\n"
       << "  operations['v" << std::hex << id << "'].append(" << std::dec
       << op->size << ")\n";

   if (auto extract_op = dynamic_cast<circ::Extract *>(op);
               extract_op) {
      os << "  operations['v" << std::hex << id << "'].append(" << std::dec
         << extract_op->high_bit_exc << ")\n"
         << "  operations['v" << std::hex << id << "'].append(" << std::dec
         << extract_op->low_bit_inc << ")\n";
    }

    for (auto sub_op : op->operands) {
      os << "  operations['v" << std::hex << id << "'].append('v"
         << sub_op->id() << std::dec << "')\n";
    }
  };

  circuit->ForEachOperation(do_op);
  do_op(circuit);

  //os << "  return v" << std::hex << reinterpret_cast<uintptr_t>(circuit.get())
  os << "  return operations"
     << "\n\n"
     << std::dec;
}

}  // namespace circ
