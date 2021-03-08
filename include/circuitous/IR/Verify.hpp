/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <cstdint>
#include <sstream>

#include <circuitous/IR/IR.h>


namespace circuitous {

// Really simple structural verifier
struct Verifier {
  std::stringstream ss;
  bool status = true;

  std::string Report() { return ss.str(); }

  bool LogError(Operation *op) {
    ss << op->Name() << " has " << op->operands.Size() << " which is invalid.\n";
    return false;
  }

  bool Undef(Operation *op) {
    ss << op->Name() << " is not supported.\n";
    return false;
  }

  template<typename Fn>
  bool Verify(uint64_t count, Operation *op, Fn fn) {
    if (!fn(count, op->operands.Size())) {
      return LogError(op);
    }
    return true;
  }

  bool Exactly(uint64_t count, Operation *op) {
    return Verify(count, op, [](auto ex, auto ac){ return  ex == ac; });
  }
  bool Not(uint64_t count, Operation *op) {
    return Verify(count, op, [](auto ex, auto ac){ return  ex != ac; });
  }
  bool MoreThan(uint64_t count, Operation *op) {
    return Verify(count, op, [](auto ex, auto ac){ return  ex <= ac; });
  }


  bool VerifySelf(Operation *op) {
    switch (op->op_code) {
      case Operation::kConstant:
      case Operation::kUndefined:
      case Operation::kInputRegister:
      case Operation::kOutputRegister:
      case Operation::kInputInstructionBits:
      case Operation::kHint:
        return Exactly(0, op);
      case Operation::kInputImmediate:
      case Operation::kNot:
      case Operation::kPopulationCount:
      case Operation::kParity:
      case Operation::kCountLeadingZeroes:
      case Operation::kCountTrailingZeroes:
      case Operation::kExtract:
        return Exactly(1, op);
      case Operation::kRegisterCondition:
      case Operation::kPreservedCondition:
      case Operation::kCopyCondition:
      case Operation::kDecodeCondition:
        return Exactly(2, op);
      case Operation::kConcat:
        return MoreThan(2, op);
      // TODO(lukas): LLVMOperation can actually have a variety of arities, but
      //              that will be quite complicated to check here.
      case Operation::kLLVMOperation:
      case Operation::kVerifyInstruction:
      case Operation::kOnlyOneCondition:
      case Operation::kCircuit:
        return Not(0, op);
      case Operation::kReadMemoryCondition:
        return Undef(op);
      default:
        LOG(FATAL) << "Cannot verify " << op->Name();
        return false;
    }
  }

  bool Verify(Operation *op) {
    status &= VerifySelf(op);
    for (auto o : op->operands) {
      status &= Verify(o);
    }
    return status;
  }

};

// Check if circuit has some really basic structural integrity, and return
// a `( result, error messages )`.
static inline std::pair<bool, std::string> VerifyCircuit(Circuit *circuit) {
  Verifier verifier;
  verifier.Verify(circuit);
  return {verifier.status, verifier.Report()};
}

 } // namespave circuitous
