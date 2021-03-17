/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/Printers.h>

#include <ostream>

namespace circuitous {

void PrintTopology(std::ostream &os, Operation *op, unsigned max_depth,
                   std::function<bool(Operation *)> accept) {
  if (!max_depth || !accept(op)) {
    os << 'V' << op->size;  // Treat this as a hole.
    return;
  }

  max_depth -= 1u;
  switch (op->op_code) {
    case Operation::kInputRegister : {
      if (auto x = dynamic_cast<InputRegister *>(op); x->reg_name == "RIP") {
        os << "I_RIP"; break;
      }
    }
    case Operation::kConstant:
    case Operation::kUndefined:
    case Operation::kOutputRegister:
    case Operation::kInputInstructionBits:
    case Operation::kInputImmediate:
    case Operation::kHint:
      os << 'V' << op->size;
      break;
    case Operation::kLLVMOperation:
      os << "(L" << op->size;
      for (auto sub_op : op->operands) {
        os << ' ';
        PrintTopology(os, sub_op, max_depth, accept);
      }
      os << ')';
      break;
    case Operation::kExtract:
      os << "(E" << op->size << ' ';
      PrintTopology(os, op->operands[0], max_depth, accept);
      os << ')';
      break;
    case Operation::kConcat:
      os << "(C";
      for (auto sub_op : op->operands) {
        os << ' ';
        PrintTopology(os, sub_op, max_depth, accept);
      }
      os << ')';
      break;
    case Operation::kPopulationCount:
    case Operation::kParity:
    case Operation::kCountLeadingZeroes:
    case Operation::kCountTrailingZeroes:
      os << "(F" << op->size << ' ';
      PrintTopology(os, op->operands[0], max_depth, accept);
      os << ')';
      break;
    case Operation::kRegisterCondition:
    case Operation::kPreservedCondition:
    case Operation::kCopyCondition:
    case Operation::kDecodeCondition:
      os << "(=";
      for (auto sub_op : op->operands) {
        os << ' ';
        PrintTopology(os, sub_op, max_depth, accept);
      }
      os << ')';
      break;
    case Operation::kVerifyInstruction: {
      os << "(&";
      std::vector<std::string> topos;
      topos.reserve(op->operands.Size());
      for (auto sub_op : op->operands) {
        std::stringstream ss;
        PrintTopology(ss, sub_op, max_depth, accept);
        topos.emplace_back(ss.str());
      }

      std::sort(topos.begin(), topos.end());

      for (const auto &topo : topos) {
        os << ' ' << topo;
      }
      os << ')';
      break;
    }
    case Operation::kOnlyOneCondition:
      os << "(^";
      for (auto sub_op : op->operands) {
        os << ' ';
        PrintTopology(os, sub_op, max_depth, accept);
      }
      os << ')';
      break;
    case Operation::kCircuit:
      return PrintTopology(os, op->operands[0], max_depth + 1u, accept);
  }
}

}  // namespace circuitous
