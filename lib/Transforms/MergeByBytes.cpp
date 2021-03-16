/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/Transforms.h>
#include <circuitous/IR/Shapes.hpp>

#include <set>
#include <unordered_map>

namespace circuitous {

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
  auto ctxs = CtxCollector().Run(circuit).Get();
  for (auto &[op, ctx] : ctxs) {
    LOG(INFO) << "CHECKING " << op->Name() << " " << op->id();
    if (!IsLeaf(op)) {
      //CHECK(ctx.size() == 1) << op->Name() << " " << op->id() << " " << ctx.size();
      LOG(WARNING) << op->Name() << " " << op->id() << " shares more contexts";
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
      if (decode->op_code == Operation::kDecodeCondition) {
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

} // namespace circuitous