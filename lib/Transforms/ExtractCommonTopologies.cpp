/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Shapes.hpp>

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

struct ECT {
  struct IDHash {
    std::size_t operator()(const Operation *op) const { return op->id(); }
  };

  // We actually do care about ordering being deterministic here, for
  // debugging reasons.
  using Ctxs = std::unordered_set<VerifyInstruction *, IDHash>;
  using VerifyList = std::unordered_set<VerifyInstruction *, IDHash>;
  using OpUseList = std::vector<std::pair<Operation *, const VerifyList *>>;

  std::unordered_map<Operation *, Operation *> seen;

  // Merge leaf nodes (input registers, constants, or hints).
  Operation *MergeLeaves(Circuit *circuit, const OpUseList &op_uses) {

    for (auto &in : op_uses) {
      LOG(INFO) << in.first->Name();
    }

    const auto first_op = op_uses[0].first;
    auto found_unequal = [&]() {
      for (auto [op, _] : op_uses) {
        if (!first_op->Equals(op)) {
          return true;
        }
      }
      return false;
    }();

    // All of the operations in `seen` are structurally identical; there's nothing
    // to do.
    if (!found_unequal) {
      return first_op;
    }

    // NOTE(pag): We don't cache these, because otherwise if one operation
    //            does something like `add a, a` then it might not be mergable
    //            with another one that does `add b, c`.
    auto hint = circuit->Create<Hint>(first_op->size);

    // Keep track of which context already enforce some value of this Hint;
    // this check is *probably* redundant, since we do some validy
    // assesment before starting the `Merge` but it does not hurt.
    std::unordered_set<Operation *> saturated_ctxs;

    for (auto [op, verifications] : op_uses) {
      CHECK(!verifications->empty());

      auto check = circuit->Create<HintCondition>();
      check->AddUse(op);
      check->AddUse(hint);


      // Wire it in so that the verification that transitively needs to use
      // `hint` also checks that `hint` has the expected value.
      for (VerifyInstruction *verify : *verifications) {
        CHECK(saturated_ctxs.count(verify));
        saturated_ctxs.insert(verify);
        verify->AddUse(check);
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
  Operation *
  MergeWithALU(Circuit *circuit, const OpUseList &op_uses) {

    // If all of the the operations in `op_uses` are leaf nodes then we can treat
    // them uniformly with `MergeLeaves`.
    bool all_leaves = true;
    for (auto [op, verifications] : op_uses) {
      (void) verifications;
      switch (op->op_code) {
        case Operation::kInputRegister:
        case Operation::kInputImmediate:
        case Operation::kHint:
        case Operation::kConstant: break;
        default: all_leaves = false; break;
      }
    }

    if (all_leaves) {
      return MergeLeaves(circuit, op_uses);
    }

    LOG(WARNING) << "TODO(lukas): We would like to merge via ALU.";
    return nullptr;
  }

  Operation *Merge(Circuit *circuit, const OpUseList &op_uses) {

    // Figure out if we need to generate an ALU, or if we're going to hit some
    // cases that we definitely can't merge.
    const auto first_op = op_uses[0].first;
    const auto op_size = first_op->size;
    const auto op_code = first_op->op_code;
    const auto op_num_ops = first_op->operands.Size();

    // First, go through and sanity check that we can actually do any kind
    // of merging.
    for (auto [op, _] : op_uses) {
      if (op->size != op_size || op->operands.Size() != op_num_ops) {
        return nullptr;
      }
    }

    // Sanity checks have passed, lets look for non-uniformity. If not all
    // operations are uniform then we might need to invent an ALU-like node
    // that can choose what operator to invoke,
    for (auto [op, _] : op_uses) {
      if (op->op_code != op_code) {
        return MergeWithALU(circuit, op_uses);
      }
    }

    // We've already merged these.
    //
    // TODO(pag): Think about if this caching is actually correct.
    auto prev_op_it = seen.find(first_op);
    if (prev_op_it != seen.end()) {
      LOG(INFO) << "WE HIT DUBIOUS CACHE";
      //return prev_op_it->second;
    }

    LOG(INFO) << "JUST CHECKING THAT I AM PROGRESSING";
    // All of the operations are uniform. If we're dealing with a leaf node
    // then we'll go and merge them using a different approach.
    if (IsOneOf<InputRegister, Hint, Constant, InputImmediate>(first_op)) {
      LOG(INFO) << "GOING TO MERGE LEAVES";
      return MergeLeaves(circuit, op_uses);
    }

    CHECK_LT(0u, op_num_ops);
    Operation *merged_op = first_op->CloneWithoutOperands(circuit);

    // Go cache that we've processed these nodes before we go and actually
    // merge the child nodes.
    for (const auto &[op, verifications] : op_uses) {
      seen.emplace(op, merged_op);
    }

    // Now we are going to try and merge operands - they should be identical
    // after all and that means mergeable.
    for (auto i = 0u; i < op_num_ops; ++i) {
      OpUseList next_op_uses;
      // Take all i-ith operands
      for (auto [op, verifications] : op_uses) {
        next_op_uses.emplace_back(op->operands[i], verifications);
      }

      auto sub_op = Merge(circuit, next_op_uses);
      if (!sub_op) {
        return nullptr;
      }

      // We sucessfully created something new in their place, so let's use it
      merged_op->AddUse(sub_op);
    }

    return merged_op;
  }

  // The rough "shape" of all expressions here are the same, just that some of
  // the operations may differ, thus requiring the invention of an ALU node, or
  // where a leaf (e.g. a register uses) is different, thus requiring the
  // invention of a HINT value.
  bool ExtractCommonTopologies(Circuit *circuit, const OpUseList &op_uses) {
    auto new_op = Merge(circuit, op_uses);
    // We were not able to do any merges
    if (!new_op) {
      return false;
    }

    // Replace old Operations
    for (const auto &[op, verifications] : op_uses) {
      op->ReplaceAllUsesWith(new_op);
    }
    return true;
  }

  std::string Hash(Operation *op) {
    std::stringstream ss;
    PrintTopology(ss, op, ~0u, [](auto){ return true; } );
    return ss.str();
  }

  bool ExtractCommonTopologies(Circuit *circuit) {
    // Go find all output values that we might want to compare against each-other.
    //
    // NOTE(pag): We need to keep things "context-sensitive" with respect to the
    //            `VerifyInstruction` so that we can later add hint conditions
    //            into them.
    std::unordered_map<Operation *, Ctxs> val_to_ctxs;
    for (auto verification : circuit->Attr<VerifyInstruction>()) {
      for (auto op : verification->operands) {
        if (IsOneOf<RegisterCondition, HintCondition>(op)) {
          auto value = op->operands[RegisterCondition::kDynamicRegisterValue];

          // NOTE(pag): Here we exclude "trivial" transitions, i.e. where the
          //            transition value is a single thing and not actually
          //            and expression. These should be handled by another
          //            transformer somewhere.
          if (!IsOneOf<Hint, Undefined, InputRegister, Constant, InputImmediate>(op)) {
            val_to_ctxs[value].insert(verification);
          }
        }
      }
    }

    // Bucket them by topology.
    std::unordered_map<std::string, OpUseList> val_buckets;
    for (const auto &[val, verifications] : val_to_ctxs) {
      val_buckets[Hash(val)].emplace_back(val, &verifications);
    }

    auto have_unique_ctxs = [](auto &items) {
      uint32_t expected_size = 0;
      std::unordered_set<VerifyInstruction *> all_ctx;
      for (auto [_, verifications] : items) {
        for (auto v : *verifications) {
          all_ctx.emplace(v);
        }
        expected_size += verifications->size();
      }
      return all_ctx.size() == expected_size;
    };

    // Process each bucket.
    bool changed = false;
    for (auto &[topology, op_uses] : val_buckets) {
      // There must be 2+ trees with the same hash, so we have something
      // to extract.
      if (op_uses.size() > 1) {
        LOG(INFO) << topology;
      } else {
        continue;
      }

      // This is a preventive measure, because you cannot extract 2 operations
      // (even thought they can share the same hash) from the same context --
      // you would end up with 2 HINT_CHECKS of the same HINT in one context
      // and that is not correct.
      if (!have_unique_ctxs(op_uses)) {
        LOG(INFO) << "Skipping topology " << topology << " as some operations"
                  << "share ctxs.";
        continue;
      }

      changed |= ExtractCommonTopologies(circuit, op_uses);
    }

    if (changed) {
      circuit->RemoveUnused();
    }

    return changed;
  }
};

}  // namespace

// Look for common topological structures and extract them so that they are
// shared by multiple different expressions.
bool ExtractCommonTopologies(Circuit *circuit) {
  return ECT().ExtractCommonTopologies(circuit);
}

}  // namespace circuitous
