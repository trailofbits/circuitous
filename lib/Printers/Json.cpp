/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>

#include <ostream>

namespace circuitous {

void PrintJSON(std::ostream &os, Circuit *circuit) {
  os << "{\n";
  bool first = true;
  auto do_op = [&](circuitous::Operation *op) {
    if (!first) {
      os << ",\n";
    } else {first = false;}
    os << "\"v" << std::hex << reinterpret_cast<uintptr_t>(op) << "\":{";
    os << "\"op_name\":\"" << op->Name() << "\",";
    os << "\"op_size\":" << std::dec << op->size << ",";
    os << "\"op_code\":" << std::dec << static_cast<unsigned>(op->op_code) << ",";

    if (auto llvm_op = dynamic_cast<circuitous::LLVMOperation *>(op); llvm_op) {
      os << "\"node_type\":{\"LLVMOp\":{";
      os << "\"llvm_opcode\":" << std::dec << llvm_op->llvm_op_code;
      os << ",\"llvm_pred\":" << std::dec << llvm_op->llvm_predicate << "}},";

    } else if (auto extract_op = dynamic_cast<circuitous::Extract *>(op);
               extract_op) {
      os << "\"node_type\":{\"ExtractOp\":{";
      os << "\"high\":" << std::dec << extract_op->high_bit_exc;
      os << ",\"low\":" << std::dec << extract_op->low_bit_inc << "}},";
    } else {
      os << "\"node_type\": \"Other\",";
    }

    os << "\"operands\":[";
    bool first2 = true;
    for (auto sub_op : op->operands) {
      if (!first2) {
        os << ",";
      } else {first2 = false;}
      os << "\"v" << std::hex << reinterpret_cast<uintptr_t>(sub_op) << "\"";
    }
    os << "]}";
  };

  circuit->ForEachOperation(do_op);
  do_op(circuit);

  os << "\n}"
     << "\n"
     << std::dec;
}

}  // namespace circuitous
