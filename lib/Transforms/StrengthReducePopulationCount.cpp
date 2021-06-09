/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/Transforms.h>
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Instruction.h>
#pragma clang diagnostic pop

namespace circuitous {

// Look for uses of population count that operates on a zero-extended value, and
// change it to operate on the original value.
bool StrengthReducePopulationCount(Circuit *circuit) {
  /**
  circuit->RemoveUnused<PopulationCount>();
  const auto num_pop_counts = circuit->Attr<PopulationCount>().Size();
  for (auto i = 0u; i < num_pop_counts; ++i) {
    const auto inst = circuit->Attr<PopulationCount>()[i];
    const auto val = inst->operands[0];

    auto llvm_ir = dynamic_cast<LLVMOperation *>(val);
    if (!llvm_ir || llvm_ir->llvm_op_code != llvm::Instruction::ZExt) {
      continue;
    }

    const auto real_val = val->operands[0];
    auto new_popcount = circuit->Create<PopulationCount>(real_val->size);
    auto new_zext = circuit->Create<LLVMOperation>(
        llvm::Instruction::ZExt, LLVMOperation::kInvalidLLVMPredicate,
        inst->size);

    inst->ReplaceAllUsesWith(new_zext);
    new_zext->AddUse(new_popcount);
    new_popcount->AddUse(real_val);
  }

  return 0u < circuit->RemoveUnused<PopulationCount>();
  **/
  return circuit;
}

}  // namespace circuitous
