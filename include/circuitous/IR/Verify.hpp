/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <cstdint>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

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


  bool VerifyArity(Operation *op) {
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
      case Operation::kHintCondition:
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
    status &= VerifyArity(op);
    for (auto o : op->operands) {
      status &= Verify(o);
    }
    return status;
  }

  void VerifyHints(Circuit *circuit) {
    status &= VerifyHintChecks(circuit);
    status &= VerifyHintUsers(circuit);
  }

  // We try to check if there is not a HINT with more than one HINT_CHECK
  // in the same context, since that is an error.
  bool VerifyHintChecks(Circuit *circuit) {
    bool out = true;
    std::unordered_map<Operation *, std::unordered_set<VerifyInstruction *>> hint_to_ctxs;
    std::unordered_map<Operation *, VerifyInstruction *> ctx;
    for (auto verif : circuit->verifications) {
      for (auto op : verif->operands) {
        if (op->op_code == Operation::kHintCondition) {
          ctx[op] = verif;

          bool found_hint = false;
          for (auto hint : op->operands) {
            if (hint->op_code == Operation::kHint) {
              if (found_hint) {
                ss << "HINT_CHECK has at least two direct HINT operands!\n";
                status = false;
              }
              found_hint = true;
              if (hint_to_ctxs[hint].count(verif)) {
                ss << "Hint is under two hint checks in the same context!\n";
                status = false;
              }
            }
          }
          if (!found_hint) {
            ss << "HINT_CHECK does not have HINT as direct operand!\n";
            status = false;
          }
        }
      }
    }
    return out;
  }

  using hint_users_t = std::unordered_map<Operation *, std::unordered_set<Operation *>>;
  void CollectHintUsers(Operation *op, hint_users_t &collected ) {
    for (auto child : op->operands) {
      if (child->op_code == Operation::kHint) {
        collected[child].insert(op);
      } else {
        CollectHintUsers(child, collected);
      }
    }
  }

  bool VerifyHintUsers(Operation *circuit) {
    hint_users_t collected;
    CollectHintUsers(circuit, collected);

    bool out = true;
    for (auto &[hint, users] : collected) {
      if (users.size() <= 1) {
        ss << "HINT has only one user. High possibility of error!\n";
        out = false;
      }
    }
    return out;
  }

};

// Check if circuit has some really basic structural integrity, and return
// a `( result, error messages )`.
static inline std::pair<bool, std::string> VerifyCircuit(Circuit *circuit) {
  Verifier verifier;
  verifier.Verify(circuit);
  verifier.VerifyHints(circuit);
  return {verifier.status, verifier.Report()};
}

 } // namespave circuitous
