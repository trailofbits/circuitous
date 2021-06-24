/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/Transforms.h>
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Instruction.h>
#pragma clang diagnostic pop

namespace circ {

// Look for uses of the population count instruction that look like they are
// actually computing the parity of some bits, and then replace that computation
// with a parity node.
bool ConvertPopCountToParity(Circuit *circuit) { return circuit; }
  /**
  const auto num_pop_counts = circuit->Attr<PopulationCount>().Size();

  // Look for `popcount & 1`, which is testing is the result of doing a
  // population count is even or odd, i.e. the parity of the value.
  auto process_masked_pop_count = [=](PopulationCount *pop_count,
                                      LLVMOperation *and_inst,
                                      Operation *mask) {
    auto const_val = dynamic_cast<Constant *>(mask);
    if (!const_val || const_val->bits[0] != '1') {
      return;
    }

    auto mask_pop_count = 0;
    for (auto bit : const_val->bits) {
      mask_pop_count += bit - '0';
    }

    if (1 < mask_pop_count) {
      return;
    }

    Parity *new_parity = circuit->Create<Parity>();
    LLVMOperation *new_zext = circuit->Create<LLVMOperation>(
        llvm::Instruction::ZExt, LLVMOperation::kInvalidLLVMPredicate,
        and_inst->size);

    and_inst->ReplaceAllUsesWith(new_zext);

    new_zext->AddUse(new_parity);
    new_parity->AddUse(pop_count->operands[0]);
  };

  // Go find uses of the popcount IR node, and then find uses that mask the
  // result with an `AND`.
  circuit->RemoveUnused<PopulationCount>();
  for (auto i = 0u; i < num_pop_counts; ++i) {
    const auto inst = circuit->Attr<PopulationCount>()[i];
    inst->ForEachUse<LLVMOperation>([=](LLVMOperation *user) {
      if (user->llvm_op_code != llvm::Instruction::And) {
        return;
      }
      if (user->operands[0] == inst) {
        process_masked_pop_count(inst, user, user->operands[1]);
      } else if (user->operands[1] == inst) {
        process_masked_pop_count(inst, user, user->operands[0]);
      }
    });
  }

  return 0u < circuit->RemoveUnused<LLVMOperation>();
}
**/

}  // namespace circ
