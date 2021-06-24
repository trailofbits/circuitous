/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/Transforms.h>

#include <set>
#include <unordered_map>

namespace circ {

using inst_set_t = std::unordered_set<Operation *>;
using decode_bucket_t = std::vector<inst_set_t>;

// We are given a bucket of decode instructions that share the constant fragments,
// now we want to figure out which are actually identical (with respect to entire bucket).
void ProcessBucket(const decode_bucket_t &bucket) {
  std::unordered_map<std::string, decode_bucket_t> sorted;

  for (auto decodes : bucket) {
    sorted[print::FullNames().Hash(decodes)].push_back(decodes);
  }

  for (auto &[_, equivalence] : sorted) {
    if (equivalence.size() > 1) {
      LOG(FATAL)
        << "We got something to merge, this is unexpected as llvm opts typically "
        << "take care of it for us";
    }
  }
}

bool MergeByBytes(Circuit *circuit) {
  // Check if contexts are not overlaping
  auto ctxs = std::move(CtxCollector().Run(circuit).op_to_ctxs);
  for (auto &[op, ctx] : ctxs) {
    if (!IsLeafNode(op)) {
      LOG(WARNING) << op->Name() << " " << op->id() << " shares more contexts";
      return true;
    }
  }

  std::unordered_map<std::string, decode_bucket_t> ordered_by_fragments;

  // For each top-level instruction find all decode inst bits and group
  // them by their constant fragments.
  // E.g `decode(C_21, add(RAX, C_22))` and `decode(C_22, sub(RAX, C_21))`
  // would be grouped together.
  // NOTE(lukas): Watch out, constants can repeat themselves.
  for (auto verif : circuit->Attr<VerifyInstruction>()) {
    std::unordered_set<Operation *> decodes;
    for (auto decode : verif->operands) {
      if (decode->op_code == DecodeCondition::kind) {
        decodes.insert(decode);
      }
    }

    // Fetch all constants and return their bits.
    auto extract = [](auto c) { return c->bits; };
    auto extracted = SubtreeCollector<Constant>().Run(decodes).Apply(extract);

    // Concat bits together, so we have a reasonable hash.
    auto concated = [&]() {
      std::stringstream ss;
      for (auto x : extracted) {
        ss << x;
      }
      return ss.str();
    }();

    // Finally, add the decoded fragments to correct buckets.
    ordered_by_fragments[concated].push_back(decodes);
  }

  // Now process buckets, including their potentional merge.
  for (auto&[_, ops] : ordered_by_fragments) {
    ProcessBucket(ops);
  }

  return false;
}

template<typename Hasher>
struct DAGifier : UniqueVisitor<DAGifier<Hasher>> {
  using hash_t = std::string;

  // The "location" that represents the hash -- each
  // value with the same one should be replaced by this.
  std::unordered_map<hash_t, Operation *> locations;
  // TODO(lukas): Cache, so the hashing does not get too expensive.
  std::unordered_map<Operation *, hash_t> hashes;

  std::unordered_set<Operation *> seen;
  std::deque<Operation *> todo;

  bool Update(hash_t key, Operation *op) {
    hashes[op] = key;
    const auto &[it, status] = locations.try_emplace(key, op);
    if (!status) {
      op->ReplaceAllUsesWith(it->second);
      // TODO(lukas): Remove op
    }
    return status;
  }

  bool Update(Operation *op) {
    return Update(Hasher().Hash(op), op);
  }

  void Push(Operation *op) {
    for (auto user : op->users) {
      if (seen.count(user)) {
        continue;
      }
      seen.insert(user);
      todo.push_back(user);
    }
  }

  Operation *Pop() {
    auto x = todo.front();
    todo.pop_front();
    return x;
  }

  void Process(Operation *op) {
    if (Update(op)) {
      Push(op);
    }
  }

  template<typename Head, typename ... Tail>
  void LocateLeaves(Circuit *circuit) {
    for (auto x : circuit->Attr<Head>()) {
      Process(x);
    }
    if constexpr (sizeof ... (Tail) != 0)
      return LocateLeaves<Tail ...>(circuit);
  }

  void LocateLeaves(Circuit *circuit) {
    return LocateLeaves<
      InputRegister,
      OutputRegister,
      Constant,
      Advice,
      InputInstructionBits
     >(circuit);
  }

  void Run(Circuit *circuit) {
    LocateLeaves(circuit);
    while (!todo.empty()) {
      Process(Pop());
    }
  }

};

bool DAGify(Circuit *circuit) {
  DAGifier<print::FullNames>().Run(circuit);
  circuit->RemoveUnused();
  return true;
}

} // namespace circ