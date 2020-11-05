/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/Printers.h>
#include <circuitous/Transforms.h>
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Instruction.h>
#pragma clang diagnostic pop

#include <sstream>
#include <unordered_map>

namespace circuitous {
namespace {

using VerifyList = std::vector<VerifyInstruction *>;
using OpUseList = std::vector<std::pair<Operation *, const VerifyList *>>;

static void SortOpUseList(OpUseList &op_uses) {
  std::sort(op_uses.begin(), op_uses.end(),
            [](std::pair<Operation *, const VerifyList *> a,
               std::pair<Operation *, const VerifyList *> b) {
              return a.first < b.first;
            });
}

// Merge leaf nodes (input registers, constants, or hints).
static Operation *
MergeLeaves(Circuit *circuit, const OpUseList &op_uses,
            std::unordered_map<Operation *, Operation *> &seen) {

  const auto first_op = op_uses[0].first;
  auto found_unequal = false;
  for (auto [op, verifications] : op_uses) {
    (void) verifications;
    if (!first_op->Equals(op)) {
      found_unequal = true;
      break;
    }
  }

  // All of the operations in `seen` are structurally identical; there's nothing
  // to do.
  if (!found_unequal) {
    return first_op;
  }

  // NOTE(pag): We don't cache these, because otherwise if one operation
  //            does something like `add a, a` then it might not be mergable
  //            with another one that does `add b, c`.
  auto hint = circuit->hints.Create(first_op->size);

  Operation *last_op = nullptr;
  Operation *last_check = nullptr;
  for (auto [op, verifications] : op_uses) {
    CHECK(!verifications->empty());

    Operation *check = nullptr;
    if (last_op == op) {
      check = last_check;
    } else {
      check = circuit->hint_conds.Create();
      check->operands.AddUse(op);
      check->operands.AddUse(hint);
      last_check = check;
      last_op = op;
    }

    // Wire it in so that the verification that transitively needs to use
    // `hint` also checks that `hint` has the expected value.
    for (VerifyInstruction *verify : (*verifications)) {
      verify->operands.AddUse(check);
    }
  }

  return hint;
}

// The values of `op_uses[N].first->op_code` don't all match, so we want to
// generate a kind of "ALU" node that selects which operation to run based off
// of a hint.
//
// The degenerate case is that some of these values are registers and some are
// consts, in which case we can handle all of them with hints.
static Operation *
MergeWithALU(Circuit *circuit, const OpUseList &op_uses,
             std::unordered_map<Operation *, Operation *> &seen) {

  // If all of the the operations in `op_uses` are leaf nodes then we can treat
  // them uniformly with `MergeLeaves`.
  bool all_leaves = true;
  for (auto [op, verifications] : op_uses) {
    (void) verifications;
    switch (op->op_code) {
      case Operation::kInputRegister:
      case Operation::kHint:
      case Operation::kConstant: break;
      default: all_leaves = false; break;
    }
  }

  if (all_leaves) {
    return MergeLeaves(circuit, op_uses, seen);
  }

  return nullptr;
}

static Operation *Merge(Circuit *circuit, const OpUseList &op_uses,
                        std::unordered_map<Operation *, Operation *> &seen) {

  // Figure out if we need to generate an ALU, or if we're going to hit some
  // cases that we definitely can't merge.
  const auto first_op = op_uses[0].first;
  const auto op_size = first_op->size;
  const auto op_code = first_op->op_code;
  const auto op_num_ops = first_op->operands.Size();

  // First, go through and sanity check that we can actually do any kind
  // of merging.
  for (auto [op, verifications] : op_uses) {
    (void) verifications;
    if (op->size != op_size || op->operands.Size() != op_num_ops) {
      return nullptr;
    }
  }

  // Sanity checks have passed, lets look for non-uniformity. If not all
  // operations are uniform then we might need to invent an ALU-like node
  // that can choose what operator to invoke,
  for (auto [op, verifications] : op_uses) {
    (void) verifications;
    if (op->op_code != op_code) {
      return MergeWithALU(circuit, op_uses, seen);
    }
  }

  // We've already merged these.
  //
  // TODO(pag): Think about if this caching is actually correct.
  auto prev_op_it = seen.find(first_op);
  if (prev_op_it != seen.end()) {
    return prev_op_it->second;
  }

  // All of the operations are uniform. If we're dealing with a leaf node
  // then we'll go and merge them using a different approach.
  if (op_code == Operation::kInputRegister || op_code == Operation::kHint ||
      op_code == Operation::kConstant) {
    return MergeLeaves(circuit, op_uses, seen);
  }

  CHECK_LT(0u, op_num_ops);

  Operation *merged_op = nullptr;

  switch (op_code) {
#define CASE(cls, field) \
  case Operation::k##cls: \
    merged_op = first_op->CloneWithoutOperands(circuit); \
    break;

    FOR_EACH_OPERATION(CASE)
#undef CASE

    default: return nullptr;
  }

  // Go cache that we've processed these nodes before we go and actually
  // merge the child nodes.
  for (const auto &[op, verifications] : op_uses) {
    seen.emplace(op, merged_op);
  }

  OpUseList next_op_uses;
  for (auto i = 0u; i < op_num_ops; ++i) {
    next_op_uses.clear();
    for (auto [op, verifications] : op_uses) {
      next_op_uses.emplace_back(op->operands[i], verifications);
    }

    SortOpUseList(next_op_uses);
    auto sub_op = Merge(circuit, next_op_uses, seen);
    if (!sub_op) {
      return nullptr;
    }

    merged_op->operands.AddUse(sub_op);
  }

  return merged_op;
}

// The rough "shape" of all expressions here are the same, just that some of
// the operations may differ, thus requiring the invention of an ALU node, or
// where a leaf (e.g. a register uses) is different, thus requiring the
// invention of a HINT value.
static bool
ExtractCommonTopologies(Circuit *circuit, const OpUseList &op_uses,
                        std::unordered_map<Operation *, Operation *> &seen) {

  if (op_uses.size() <= 1u) {
    return false;
  }

  if (auto new_op = Merge(circuit, op_uses, seen); new_op) {
    for (const auto &[op, verifications] : op_uses) {
      op->ReplaceAllUsesWith(new_op);
    }
    return true;
  }

  return false;
}

}  // namespace

// Look for common topological structures and extract them so that they are
// shared by multiple different expressions.
bool ExtractCommonTopologies(Circuit *circuit) {

  circuit->transitions.RemoveUnused();

  std::unordered_map<Operation *, VerifyList> values_by_inst;

  // Go find all output values that we might want to compare against each-other.
  //
  // NOTE(pag): We need to keep things "context-sensitive" with respect to the
  //            `VerifyInstruction` so that we can later add hint conditions
  //            into them.
  for (auto verification : circuit->verifications) {
    for (auto op : verification->operands) {
      if (dynamic_cast<RegisterCondition *>(op) ||
          dynamic_cast<HintCondition *>(op)) {
        const auto value =
            op->operands[RegisterCondition::kDynamicRegisterValue];

        // NOTE(pag): Here we exclude "trivial" transitions, i.e. where the
        //            transition value is a single thing and not actually
        //            and expression. These should be handled by another
        //            transformer somewhere.
        if (!dynamic_cast<Hint *>(value) && !dynamic_cast<Undefined *>(value) &&
            !dynamic_cast<InputRegister *>(value) &&
            !dynamic_cast<Constant *>(value)) {
          values_by_inst[value].push_back(verification);
        }
      }
    }
  }

  // Keep only unique ones.
  for (auto &[val, verifications] : values_by_inst) {
    (void) val;
    std::sort(verifications.begin(), verifications.end());
    auto it = std::unique(verifications.begin(), verifications.end());
    verifications.erase(it, verifications.end());
  }

  // Bucket them by topology.
  std::unordered_map<std::string, OpUseList> val_buckets;
  for (const auto &[val, verifications] : values_by_inst) {
    std::stringstream ss;
    PrintTopology(
        ss, val, ~0u, +[](Operation *op) { return true; });
    val_buckets[ss.str()].emplace_back(val, &verifications);
  }

  // Process each bucket.
  bool changed = false;
  std::unordered_map<Operation *, Operation *> seen;
  for (auto &[topology, op_uses] : val_buckets) {
    SortOpUseList(op_uses);
    if (ExtractCommonTopologies(circuit, op_uses, seen)) {
      changed = true;
    }
  }

  if (changed) {
    circuit->RemoveUnused();
  }

  return changed;
}

}  // namespace circuitous
