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
namespace impl {

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
        CHECK(!saturated_ctxs.count(verify));
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
        case InputRegister::kind:
        case InputImmediate::kind:
        case Hint::kind:
        case Constant::kind: break;
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
    if (IsOneOf<InputRegister, Hint, Constant, InputImmediate, InputInstructionBits>(first_op)) {
      LOG(INFO) << "GOING TO MERGE LEAVES";
      return MergeLeaves(circuit, op_uses);
    }

    CHECK_LT(0u, op_num_ops);
    Operation *merged_op = circuit->Fork(first_op);

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
          auto value = op->operands[RegisterCondition::kDynamic];

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

struct DependencyBreaker {
  using collector_t = Collector<collect::Ctxs, collect::Hashes>;
  collector_t info;

  template<typename C>
  void Status(std::size_t total, const C& ctxs) {
    LOG(INFO) << "Ctx requirements:";
    for (auto &[ctx, ops] : ctxs) {
      LOG(INFO) << " " << ctx->id() << " " << ops.size();
    }
    LOG(INFO) << "Total op count " << total;
  }

  struct AllocationMap {
    std::unordered_map<Operation *, std::vector<Operation *>> table;
    uint64_t current_size = 0;

    AllocationMap(uint64_t expected_size) : current_size(expected_size) {
      LOG(INFO) << "Creating table with " << expected_size;
    }

    void Allocate(Operation *inst, const std::unordered_set<Operation *> &ctxs) {
      std::map<uint64_t, uint64_t> holes;
      for (auto ctx : ctxs) {
        if (!table.count(ctx)) {
          AddRow(ctx);
        }
        auto &row = table[ctx];
        for (std::size_t i = 0; i < row.size(); ++i) {
          if (!row[i]) {
            auto [it, _] = holes.emplace(i, 0);
            ++(it->second);
          }
        }
      }
      std::optional<uint64_t> trg;
      LOG(INFO) << inst->id() << " searching for a allocation spot " << ctxs.size();
      for (auto &[idx, count]: holes) {
        LOG(INFO) << idx << " : " << count;
        if (count == ctxs.size()) {

          trg = idx;
          break;
        }
      }

      if (!trg) {
        Enlarge();
        trg = current_size - 1;
      }

      for (auto ctx : ctxs) {
        table[ctx][*trg] = inst;
      }
    }


    void AddRow(Operation *op) {
      table[op] = std::vector<Operation *>(current_size, nullptr);
    }

    void Enlarge() {
      ++current_size;
      for (auto &[_, vec] : table) {
        vec.resize(vec.size() + 1);
      }
    }

    std::string Results() {
      std::stringstream ss;
      ss << std::endl;
      for (auto &[ctx, row] : table) {
        ss << ctx->id() << " : ";
        for (auto i : row) {
          if (i) {
            ss << i->id() << "  ";
          } else {
            ss << " __ ";
          }
          ss << " | ";
        }
        ss << std::endl;
      }
      return ss.str();
    }
  };

  void Extract(Circuit *circuit, uint32_t llvm_op) {
    using operations_t = std::unordered_set<LLVMOperation *>;

    std::unordered_map<Operation *, operations_t> ctx_requirements;
    std::size_t total = 0;
    std::size_t required = 0;
    std::unordered_set<LLVMOperation *> candidates;
    for (LLVMOperation *op : circuit->Attr<LLVMOperation>()) {
      if (op->llvm_op_code != llvm_op) {
        continue;
      }
      ++total;
      candidates.insert(op);
      for (auto ctx : info.op_to_ctxs[op]) {
        auto [it, _] = ctx_requirements.try_emplace(ctx, operations_t{});
        it->second.insert(op);
        required = std::max(it->second.size(), required);
      }
    }
    Status(total, ctx_requirements);

    AllocationMap allocations(required);
    for (auto inst : candidates) {
      allocations.Allocate(inst, info.op_to_ctxs[inst]);
    }
    LOG(INFO) << allocations.Results();

    std::vector<Operation *> models;
    for (uint64_t i = 0; i < allocations.current_size; ++i) {
      auto orig = *candidates.begin();
      auto model = circuit->Fork(orig);
      auto lhs = circuit->Create<Hint>(orig->operands[0]->size);
      auto rhs = circuit->Create<Hint>(orig->operands[1]->size);
      model->AddUse(lhs);
      model->AddUse(rhs);
      models.push_back(model);
    }
    LOG(INFO) << "Created models";

    auto bind_hints = [&](auto ctx, auto model, auto op) {
      for (std::size_t i = 0; i < op->operands.size(); ++i) {
        auto condition = circuit->Create<HintCondition>();
        condition->AddUse(op->operands[i]);
        condition->AddUse(model->operands[i]);
        ctx->AddUse(condition);
      }
    };

    LOG(INFO) << "Going to bind them";
    std::unordered_set<Operation *> transfered;
    for (auto &[ctx, row] : allocations.table) {
      LOG(INFO) << "Handling: " << ctx->id();
      for (std::size_t i = 0; i < row.size(); ++i) {
        CHECK(row.size() == allocations.current_size);
        if (!row[i]) {
          continue;
        }
        LOG(INFO) << " " << row[i]->id();
        if (!transfered.count(row[i])) {
          transfered.insert(row[i]);
          row[i]->ReplaceAllUsesWith(models[i]);
        }
        LOG(INFO) << "Binding: " << models[i]->id() << " " << row[i]->id();
        bind_hints(ctx, models[i], row[i]);
      }
    }
  }



  void Run(Circuit *circuit) {
    // Collect context information together with hashes
    info.Run(circuit);
    Extract(circuit, llvm::BinaryOperator::Add);
  }

};


// If we have hints: H, I and X( H, A ) and HINT_CHECK( H, J ), HINT_CHECK( J, B )
// we can simplify the first check to HINT_CHECK( H, B )
// This can occure if we run several optimizations after each other.
struct HintCheckReduce {
  // Given chain of hint checks **in the same context**
  // `HINT_CHECK( H0, H1 ), HINT_CHECK( H1, H2 ), ... HINT_CHECK( Hn, A )`
  // return `A`.
  Operation *Traverse(Operation *hint_check, Operation *ctx) {
      auto dyn_val = hint_check->operands[HintCondition::kDynamic];
      if (!Is<Hint>(dyn_val)) {
        return dyn_val;
      }

      for (auto user : dyn_val->users) {
        if (Is<HintCondition>(user) || GetContext(user) == ctx) {
          return Traverse(user, ctx);
        }
      }
      return nullptr;
    };


  Operation *HintCheckEnd(Operation *hint) {
    for (auto user : hint->users) {
      LOG(INFO) << print::FullNames().Hash(user);
      if (user->op_code == HintCondition::kind &&
          Is<Hint>(user->operands[HintCondition::kDynamic])) {
        CHECK(user->users.size() == 1);
        if (auto dyn_val = Traverse(user, user->users[0])) {
          LOG(INFO) << "Actually setting something";
          user->ReplaceUse(dyn_val, HintCondition::kDynamic);
        }
      }
    }
    return nullptr;
  }

  void Run(Circuit *circuit) {
    for (auto x : circuit->Attr<Hint>()) {
      HintCheckEnd(x);
    }
  }
};

}  // namespace impl

// Look for common topological structures and extract them so that they are
// shared by multiple different expressions.
bool ExtractCommonTopologies(Circuit *circuit) {
  return impl::ECT().ExtractCommonTopologies(circuit);
}

bool DependencyBreaker(Circuit *circuit) {
  impl::DependencyBreaker().Run(circuit);
  return true;
}

}  // namespace circuitous
