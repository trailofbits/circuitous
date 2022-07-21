/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.hpp>
#include <circuitous/Transforms.hpp>

#include <circuitous/Support/Check.hpp>

#include <algorithm>
#include <numeric>
#include <unordered_map>
#include <unordered_set>

namespace circ {

// Merge all of the hint inputs into a single "wide" input hint that is of
// sufficient size to support all verifiers. In place of the individual hints,
// the verifiers pull out slices of wide hint value with an EXTRACT.
bool MergeAdvices(Circuit *circuit) {

  // Create a mapping of instructions to the hints that they use.
  std::unordered_map<VerifyInstruction *, std::vector<Advice *>> used_hints;
  for (auto inst_check : circuit->attr<VerifyInstruction>()) {
    auto &inst_hints = used_hints[inst_check];

    std::function<void(Operation *)> visit;
    visit = [&] (Operation *op) {
      if (auto hint = dynamic_cast<Advice *>(op); hint) {
        inst_hints.push_back(hint);
      } else {
        for (auto sub_op : op->operands()) {
          visit(sub_op);
        }
      }
    };

    visit(inst_check);
  }

  unsigned max_size = 0;

  // Sort and unique the hints by size. We want biggest ones first. Also figure
  // out the maximum hint size needed.
  for (auto &[inst_check, hints] : used_hints) {
    std::sort(hints.begin(), hints.end());
    auto it = std::unique(hints.begin(), hints.end());
    hints.erase(it, hints.end());

    std::stable_sort(hints.begin(), hints.end(),
                     [] (Advice *a, Advice *b) {
                       return a->size > b->size;
                     });

    auto inst_needed_size = 0u;
    for (auto hint : hints) {
      inst_needed_size += hint->size;
    }

    max_size = std::max(max_size, inst_needed_size);
  }

  if (!max_size) {
    return false;
  }

  // If the same hint is used by the two instructions then we want to have them
  // use the same extraction.
  std::unordered_map<Advice *, Extract *> prev_extract;
  std::vector<bool> used_bits;

  // Can be `0` since all other Advices will be removed.
  const auto wide_hint = circuit->create<Advice>(max_size, 0u);
  for (auto &[inst_check, hints] : used_hints) {

    used_bits.clear();
    used_bits.resize(max_size);

    // Go re-use any prior used extracts, and mark the bits of the hint that
    // we've used.
    for (auto hint : hints) {
      if (auto extract = prev_extract[hint]; extract) {
        hint->replace_all_uses_with(extract);
        for (auto i = extract->low_bit_inc; i < extract->high_bit_exc; ++i) {
          used_bits[i] = true;
        }
      }
    }

    // Now go process the previously unseen hint values, and allocate them to
    // ranges of the bits within the wide hint.
    for (auto hint : hints) {
      auto &extract = prev_extract[hint];
      if (extract) {
        continue;
      }

      for (auto i = 0u; i < max_size;) {
        auto failed = 0u;

        for (auto j = 0u; j < hint->size; ++j) {
          if ((i + j) >= max_size || used_bits[i + j]) {
            failed = i + j + 1u;
            break;
          }
        }

        // We've found an allocation.
        if (!failed) {
          extract = circuit->create<Extract>(i, i + hint->size);
          extract->add_operand(wide_hint);
          hint->replace_all_uses_with(extract);

          for (auto j = 0u; j < hint->size; ++j, ++i) {
            used_bits[i] = true;
          }
          break;

        } else {
          i = failed;
        }
      }

      check(extract != nullptr)
          << "Failed to allocate " << hint->size << "-bit hint into "
          << max_size << "-bit wide hint";
    }
  }

  return true;
}

}  // namespace circ
