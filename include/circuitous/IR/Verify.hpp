/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <cstdint>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Shapes.hpp>

namespace circuitous {

// Really simple structural verifier
struct Verifier {
  std::stringstream ss;
  std::stringstream _warnings;

  bool status = true;

  std::string Report() { return ss.str(); }

  bool LogError(Operation *op) {
    ss << op->Name() << " has " << op->operands.Size() << " operands which is invalid.\n";
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
    CHECK(op);
    switch (op->op_code) {
      case Operation::kConstant:
      case Operation::kUndefined:
      case Operation::kInputRegister:
      case Operation::kOutputRegister:
      case Operation::kInputInstructionBits:
      case Operation::kHint:
      case Operation::kInputErrorFlag:
      case Operation::kOutputErrorFlag:
        return Exactly(0, op);
      case Operation::kInputImmediate:
      case Operation::kNot:
      case Operation::kPopulationCount:
      case Operation::kParity:
      case Operation::kCountLeadingZeroes:
      case Operation::kCountTrailingZeroes:
      case Operation::kExtract:
      case Operation::kMemoryRead:
        return Exactly(1, op);
      case Operation::kRegisterCondition:
      case Operation::kPreservedCondition:
      case Operation::kCopyCondition:
      case Operation::kDecodeCondition:
      case Operation::kHintCondition:
      case Operation::kMemoryWrite:
        return Exactly(2, op);
      case Operation::kSelect:
        return Exactly((1 << op->operands[0]->size) + 1, op);
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
    CHECK(op);
    status &= VerifyArity(op);
    for (auto o : op->operands) {
      status &= Verify(o);
    }
    return status;
  }

  void VerifyHints(Circuit *circuit) {
    status &= VerifyHintChecks(circuit);
    status &= VerifyHintUsers(circuit);
    status &= VerifyHintCtxs(circuit);
  }

  bool VerifyHintCtxs(Circuit *circuit) {
    CtxCollector collector;
    collector.Run(circuit);

    bool out = true;
    for (auto hint_check : circuit->Attr<HintCondition>()) {
      if (collector.op_to_ctxs[hint_check].size() != 1) {
        _warnings << "HINT_CHECK is member of multiple contexts.\n";
        out &= true;
      }
    }
    return out;
  }

  // We try to check if there is not a HINT with more than one HINT_CHECK
  // in the same context, since that is an error.
  bool VerifyHintChecks(Circuit *circuit) {
    bool out = true;
    std::unordered_map<Operation *, std::unordered_set<VerifyInstruction *>> hint_to_ctxs;
    std::unordered_map<Operation *, VerifyInstruction *> ctx;
    for (auto verif : circuit->Attr<VerifyInstruction>()) {
      for (auto op : verif->operands) {
        if (op->op_code == Operation::kHintCondition) {
          ctx[op] = verif;

          bool found_hint = false;
          for (auto hint : op->operands) {
            if (hint->op_code == Operation::kHint) {
              if (found_hint) {
                _warnings << "HINT_CHECK has at least two direct HINT operands!\n";
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


  void VerifyIDs(Circuit *circuit) {
    std::unordered_set<Operation *> seen;
    std::unordered_map<uint64_t, uint64_t> ids;
    CollectIDs(circuit, seen, ids);
    for (auto &[id, count] : ids) {
      if (count != 1) {
        status &= false;
        ss << "ID: " << id << " is present " << count << " times.\n";
      }
    }
  }

  void CollectIDs(Operation *op,
                  std::unordered_set<Operation *> &seen,
                  std::unordered_map<uint64_t, uint64_t> &ids) {
    if (seen.count(op)) {
      return;
    }
    seen.insert(op);
    if (ids.count(op->id())) {
      ids[op->id()] += 1;
    } else {
      ids[op->id()] = 1;
    }
    for (auto o : op->operands) {
      CollectIDs(o, seen, ids);
    }
  }

};

// Check if circuit has some really basic structural integrity, and return
// a `( result, error messages )`.
static inline std::tuple<bool, std::string, std::string> VerifyCircuit(Circuit *circuit) {
  Verifier verifier;
  verifier.Verify(circuit);
  verifier.VerifyHints(circuit);
  verifier.VerifyIDs(circuit);
  return {verifier.status, verifier.Report(), verifier._warnings.str() };
}

template<bool PrintWarnings=false>
static inline void VerifyCircuit(const std::string &prefix,
                                 Circuit *circuit,
                                 const std::string &suffix="Done.") {
  LOG(INFO) << prefix;
  const auto &[status, msg, warnings] = VerifyCircuit(circuit);
  if (!status) {
    LOG(ERROR) << warnings;
    LOG(ERROR) << msg;
    LOG(FATAL) << "Circuit is invalid";
  }
  if (PrintWarnings) {
    LOG(WARNING) << warnings;;
  }
  LOG(INFO) << suffix;
}

} // namespave circuitous
