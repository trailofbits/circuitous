/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Circuit.hpp>

#include <ostream>
#include <iostream>

namespace circ {

void print_json(std::ostream &os, Circuit *circuit) {
  auto id = [](Operation *op) -> std::string {
    CHECK(op);
    std::stringstream ss;
    ss << "v" << std::hex << op->id() << "v";
    return ss.str();
  };

  os << "[\"" << id(circuit) << "\",\n";
  os << "{\n";
  auto sep = "";
  auto do_op = [&](circ::Operation *op) {
    CHECK(op);
    os << sep;
    os << "\"" << id(op) << "\":{";
    os << "\"op_name\":\"" << op->Name() << "\",";
    os << "\"op_size\":" << std::dec << op->size << ",";
    os << "\"op_code\":" << std::dec << static_cast<unsigned>(op->op_code)
       << ",";

    if (auto extract_op = dynamic_cast<circ::Extract *>(op)) {
      os << "\"node_type\":{\"ExtractOp\":{";
      os << "\"high\":" << std::dec << extract_op->high_bit_exc;
      os << ",\"low\":" << std::dec << extract_op->low_bit_inc << "}},";
    } else {
      os << "\"node_type\": \"Other\",";
    }

    os << "\"operands\":[";
    sep = "";
    for (auto sub_op : op->operands) {
      os << sep;
      os << "\"" << id(sub_op) << "\"";
      sep = ",";
    }
    os << "]}";
    sep = ",\n";
  };

  circuit->ForEachOperation(do_op);
  do_op(circuit);

  os << "\n}]"
     << "\n"
     << std::dec;
}

}  // namespace circ
