/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Circuit.hpp>
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
    case InputRegister::kind : {
      if (auto x = dynamic_cast<InputRegister *>(op); x->reg_name == "RIP") {
        os << "I_RIP"; break;
      }
    }
    case Constant::kind:
    case Undefined::kind:
    case OutputRegister::kind:
    case InputInstructionBits::kind:
    case InputImmediate::kind:
    case Hint::kind:
      os << 'V' << op->size;
      break;
    case Extract::kind:
      os << "(E" << op->size << ' ';
      PrintTopology(os, op->operands[0], max_depth, accept);
      os << ')';
      break;
    case Concat::kind:
      os << "(C";
      for (auto sub_op : op->operands) {
        os << ' ';
        PrintTopology(os, sub_op, max_depth, accept);
      }
      os << ')';
      break;
    case PopulationCount::kind:
    case Parity::kind:
    case CountLeadingZeroes::kind:
    case CountTrailingZeroes::kind:
      os << "(F" << op->size << ' ';
      PrintTopology(os, op->operands[0], max_depth, accept);
      os << ')';
      break;
    case RegisterCondition::kind:
    case PreservedCondition::kind:
    case CopyCondition::kind:
    case DecodeCondition::kind:
      os << "(=";
      for (auto sub_op : op->operands) {
        os << ' ';
        PrintTopology(os, sub_op, max_depth, accept);
      }
      os << ')';
      break;
    case VerifyInstruction::kind: {
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
    case OnlyOneCondition::kind:
      os << "(^";
      for (auto sub_op : op->operands) {
        os << ' ';
        PrintTopology(os, sub_op, max_depth, accept);
      }
      os << ')';
      break;
    case Circuit::kind:
      return PrintTopology(os, op->operands[0], max_depth + 1u, accept);
  }
}

}  // namespace circuitous
