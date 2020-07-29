/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include "IR.h"

#include <glog/logging.h>

#include <sstream>
#include <unordered_map>

#include <llvm/ADT/SmallString.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

#include <remill/Arch/Arch.h>
#include <remill/BC/Util.h>

namespace circuitous {
namespace {

enum : unsigned {
  kMaxNumBytesRead = 16u
};

}  // namespace

Operation::~Operation(void) {}
Operation::Operation(unsigned op_code_, unsigned size_)
    : User(this),
      Def<Operation>(this),
      op_code(op_code_),
      size(size_),
      operands(this) {}

Operation::Operation(unsigned op_code_, unsigned size_, Operation *eq_class_)
    : User(this),
      Def<Operation>(this),
      op_code(op_code_),
      size(size_),
      operands(this),
      eq_class(eq_class_->CreateWeakUse(this)) {}


const unsigned LLVMOperation::kInvalidLLVMPredicate = \
    static_cast<unsigned>(llvm::CmpInst::BAD_ICMP_PREDICATE);

namespace {

static unsigned SizeOfValue(llvm::Instruction *inst) {
  auto module = inst->getParent()->getParent()->getParent();
  const auto &dl = module->getDataLayout();
  return static_cast<unsigned>(dl.getTypeSizeInBits(inst->getType()));
}

static unsigned Predicate(llvm::Instruction *inst) {
  if (auto cmp = llvm::dyn_cast<llvm::CmpInst>(inst); cmp) {
    return cmp->getPredicate();
  } else {
    return LLVMOperation::kInvalidLLVMPredicate;
  }
}

}  // namespace

LLVMOperation::LLVMOperation(unsigned llvm_opcode_, unsigned llvm_predicate_,
                             unsigned size_)
    : Operation(Operation::kLLVMOperation, size_),
      llvm_op_code(llvm_opcode_),
      llvm_predicate(llvm_predicate_) {}

LLVMOperation::LLVMOperation(unsigned llvm_opcode_, unsigned llvm_predicate_,
                             unsigned size_, Operation *eq_class_)
    : Operation(Operation::kLLVMOperation, size_, eq_class_),
      llvm_op_code(llvm_opcode_),
      llvm_predicate(llvm_predicate_) {}

LLVMOperation::LLVMOperation(llvm::Instruction *inst)
    : LLVMOperation(inst->getOpcode(), Predicate(inst), SizeOfValue(inst)) {}

LLVMOperation::LLVMOperation(llvm::Instruction *inst, Operation *eq_class_)
    : LLVMOperation(inst->getOpcode(), Predicate(inst),
                    SizeOfValue(inst), eq_class_) {}

#define COMMON_METHODS(cls) \
    cls::~cls(void) {} \

#define STREAM_NAME(cls, ...) \
    std::string cls::Name(void) const { \
      std::stringstream ss; \
      ss << __VA_ARGS__; \
      return ss.str(); \
    }

#define RETURN_NAME(cls, ...) \
    std::string cls::Name(void) const { \
      return __VA_ARGS__; \
    }

COMMON_METHODS(LLVMOperation)
std::string LLVMOperation::Name(void) const {
  std::stringstream ss;
  ss << "LLVM_" << llvm::Instruction::getOpcodeName(llvm_op_code);
  if (llvm_predicate != LLVMOperation::kInvalidLLVMPredicate) {
    const auto pred = static_cast<llvm::CmpInst::Predicate>(llvm_predicate);
    ss << '_' << llvm::CmpInst::getPredicateName(pred).str();
  }
  ss << '_' << size;
  return ss.str();
}

COMMON_METHODS(EquivalenceClass)
STREAM_NAME(EquivalenceClass, "EQ_CLASS_" << size);

COMMON_METHODS(Constant)
STREAM_NAME(Constant, "CONST_" << size << "_" << bits)

BitOperation::~BitOperation(void) {}

COMMON_METHODS(Extract)
STREAM_NAME(Extract, "EXTRACT_" << high_bit_exc << "_" << low_bit_inc)

COMMON_METHODS(Concat)
RETURN_NAME(Concat, "CONCAT")

COMMON_METHODS(PopulationCount)
STREAM_NAME(PopulationCount, "POPULATION_COUNT_" << operands[0]->size)

COMMON_METHODS(CountLeadingZeroes)
STREAM_NAME(CountLeadingZeroes, "COUNT_LEADING_ZEROES_" << operands[0]->size)

COMMON_METHODS(CountTrailingZeroes)
STREAM_NAME(CountTrailingZeroes, "COUNT_TRAILING_ZEROES_" << operands[0]->size)

COMMON_METHODS(ZeroFillLeft)
STREAM_NAME(ZeroFillLeft, "ZERO_FILL_LEFT_" << (size - operands[0]->size))

COMMON_METHODS(ZeroFillRight)
STREAM_NAME(ZeroFillRight, "ZERO_FILL_RIGHT_" << (size - operands[0]->size))

COMMON_METHODS(SignExtend)
STREAM_NAME(SignExtend, "SIGN_EXTEND_" << operands[0]->size << "_" << size)

Condition::~Condition(void) {}

COMMON_METHODS(ReadMemoryCondition)
STREAM_NAME(ReadMemoryCondition, "CHECK_MEM_READ_ADDR_" << operands[0]->size)

COMMON_METHODS(InputRegister)
STREAM_NAME(InputRegister, "INPUT_REGISTER_" << reg_name << "_" << size)

COMMON_METHODS(OutputRegister)
STREAM_NAME(OutputRegister, "OUTPUT_REGISTER_" << reg_name << "_" << size)

COMMON_METHODS(RegisterCondition)
STREAM_NAME(RegisterCondition, "OUTPUT_REGISTER_CHECK_" << dynamic_cast<OutputRegister *>(operands[1])->reg_name << "_" << operands[1]->size)

COMMON_METHODS(PreservedCondition)
STREAM_NAME(PreservedCondition, "PRESERED_REGISTER_CHECK_" << dynamic_cast<OutputRegister *>(operands[1])->reg_name << "_" << operands[1]->size)

COMMON_METHODS(CopyCondition)
STREAM_NAME(CopyCondition, "COPIED_REGISTER_CHECK_" << dynamic_cast<OutputRegister *>(operands[1])->reg_name << "_" << operands[1]->size)

COMMON_METHODS(Hint)
STREAM_NAME(Hint, "HINT_" << size)

COMMON_METHODS(InputInstructionBits)
STREAM_NAME(InputInstructionBits, "INSTRUCTION_BITS_" << size)

COMMON_METHODS(DecodeCondition)
STREAM_NAME(DecodeCondition, "INSTRUCTION_BITS_CHECK_" << operands[0]->size)

COMMON_METHODS(VerifyInstruction)
STREAM_NAME(VerifyInstruction, "ALL_OF_" << operands.Size())

COMMON_METHODS(OnlyOneCondition)
STREAM_NAME(OnlyOneCondition, "ONE_OF_" << operands.Size())

Circuit::~Circuit(void) {
  operands.ClearWithoutErasure();
#define CLEAR_FIELD(type, field) \
    for (auto op : field) { \
      op->operands.ClearWithoutErasure(); \
    }

  FOR_EACH_OPERATION(CLEAR_FIELD)
#undef CLEAR_FIELD
}

RETURN_NAME(Circuit, "RESULT")

#define INIT_FIELD(type, field) \
    , field(this)

Circuit::Circuit(void)
    : Condition(Operation::kCircuit)
      FOR_EACH_OPERATION(INIT_FIELD) {}

#undef INIT_FIELD

namespace {

// Apply a callback `cb(llvm::CallInst *)` to each call of `callee` in the
// function `caller`.
template <typename T>
static void ForEachCallTo(llvm::Function *caller, llvm::Function *callee, T cb) {
  if (!callee) {
    return;
  }

  std::vector<llvm::CallInst *> verify_calls;
  for (auto user : callee->users()) {
    if (auto call_inst = llvm::dyn_cast<llvm::CallInst>(user);
        call_inst && call_inst->getParent()->getParent() == caller) {
      verify_calls.push_back(call_inst);
    }
  }

  for (auto call_inst : verify_calls) {
    cb(call_inst);
  }
}

// Keeps track of instruction dependencies.
template <typename T>
class BottomUpDependencyVisitor {
 public:
  void VisitArgument(llvm::Function *, llvm::Use &, llvm::Argument *) {}
  void VisitFunctionCall(llvm::Function *, llvm::Use &, llvm::CallInst *) {}
  void VisitBinaryOperator(llvm::Function *, llvm::Use &, llvm::Instruction *) {}
  void VisitUnaryOperator(llvm::Function *, llvm::Use &, llvm::Instruction *) {}
  void VisitConstantInt(llvm::Function *, llvm::Use &, llvm::ConstantInt *) {}
  void VisitConstantFP(llvm::Function *, llvm::Use &, llvm::ConstantFP *) {}
  void Visit(llvm::Function *context, llvm::Use &use_);
};

// Analyze how `use_` is produced.
template <typename T>
void BottomUpDependencyVisitor<T>::Visit(llvm::Function *context, llvm::Use &use) {
  auto self = static_cast<T *>(this);

  const auto val = use.get();

  // Bottom out at an argument; it should be an input register.
  if (auto arg_val = llvm::dyn_cast<llvm::Argument>(val); arg_val) {
    self->VisitArgument(context, use, arg_val);

  // Instruction; follow the dependency chain.
  } else if (auto inst_val = llvm::dyn_cast<llvm::Instruction>(val);
             inst_val) {
    if (auto call_val = llvm::dyn_cast<llvm::CallInst>(inst_val); call_val) {
      for (auto &op_use: call_val->arg_operands()) {
        self->Visit(context, op_use);
      }

      self->VisitFunctionCall(context, use, call_val);

    } else {
      for (auto &op_use: inst_val->operands()) {
        self->Visit(context, op_use);
      }

      if (llvm::isa<llvm::BinaryOperator>(inst_val) ||
          llvm::isa<llvm::CmpInst>(inst_val)) {
        self->VisitBinaryOperator(context, use, inst_val);

      } else if (llvm::isa<llvm::UnaryInstruction>(inst_val)) {
        self->VisitUnaryOperator(context, use, inst_val);

      } else {
        LOG(FATAL)
            << "Unexpected value during visit: "
            << remill::LLVMThingToString(inst_val);
      }
    }

  // Bottom out at a constant, ignore for now.
  } else if (auto const_val = llvm::dyn_cast<llvm::Constant>(val);
             const_val) {
    if (auto ce = llvm::dyn_cast<llvm::ConstantExpr>(const_val); ce) {
      auto ce_inst = ce->getAsInstruction();
      auto &entry_block = context->getEntryBlock();
      ce_inst->insertBefore(&*entry_block.getFirstInsertionPt());
      ce->replaceAllUsesWith(ce_inst);
      CHECK_EQ(use.get(), ce_inst);
      self->Visit(context, use);  // Revisit.

    } else if (auto ci = llvm::dyn_cast<llvm::ConstantInt>(val); ci) {
      self->VisitConstantInt(context, use, ci);

    } else if (auto cf = llvm::dyn_cast<llvm::ConstantFP>(val); cf) {
      self->VisitConstantFP(context, use, cf);

    } else {
      LOG(FATAL)
          << "Unexpected constant encountered during dependency visitor: "
          << remill::LLVMThingToString(val);
    }
  } else {
    LOG(FATAL)
        << "Unexpected value encountered during dependency visitor: "
        << remill::LLVMThingToString(val);
  }
}

//static std::pair<llvm::Value *, llvm::ConstantInt *> GetConstantOp(
//    llvm::Value *lhs, llvm::Value *rhs) {
//  if (auto rhs_ci = llvm::dyn_cast<llvm::ConstantInt>(rhs);
//      rhs_ci) {
//    return {lhs, rhs_ci};
//  } else if (auto lhs_ci = llvm::dyn_cast<llvm::ConstantInt>(lhs);
//      rhs_ci) {
//    return {rhs, lhs_ci};
//  } else {
//    return {nullptr, nullptr};
//  }
//}

class IRImporter : public BottomUpDependencyVisitor<IRImporter> {
 public:
  explicit IRImporter(const remill::Arch *arch_, const llvm::DataLayout &dl_,
                      Circuit *impl_)
      : arch(arch_),
        dl(dl_),
        impl(impl_) {}

  void VisitArgument(llvm::Function *, llvm::Use &, llvm::Argument *val) {
    CHECK(val_to_op.count(val));
  }

  // Create an `size`-bit memory read.
  Operation *CreateMemoryRead(llvm::CallInst *read_call, unsigned size) {
    auto &read = val_to_mem_cond[read_call];
    if (read) {
      verifier->operands.AddUse(read);
      return read->operands[ReadMemoryCondition::kHintedValue];
    }

    Hint *expected_addr = impl->hints.Create(arch->address_size);
    Hint *bytes[kMaxNumBytesRead] = {};
    for (auto i = 0u, j = 0u; i < size; i += 8u, j += 1u) {
      bytes[j] = impl->hints.Create(8u);
    }

    Operation *value_read = nullptr;

    const auto num_bytes = size / 8u;
    if (1 == num_bytes) {
      value_read = bytes[0u];

    } else {
      value_read = impl->concats.Create(size);
      if (arch->MemoryAccessIsLittleEndian()) {
        for (auto i = 0u, j = num_bytes; i < num_bytes; ++i) {
          value_read->operands.AddUse(bytes[--j]);
        }
      } else {
        for (auto i = 0u; i < num_bytes; ++i) {
          value_read->operands.AddUse(bytes[i]);
        }
      }
    }

    read = impl->memory_reads.Create();
    read->operands.AddUse(val_to_op[read_call->getArgOperand(1u)]);
    read->operands.AddUse(expected_addr);
    read->operands.AddUse(value_read);

    verifier->operands.AddUse(read);

    for (auto byte : bytes) {
      if (byte) {
        byte->weak_conditions.AddUse(read);
      }
    }

    return value_read;
  }

  void VisitFunctionCall(llvm::Function *, llvm::Use &, llvm::CallInst *val) {
    auto &op = val_to_op[val];
    if (op) {
      return;
    }

    const auto res_size = static_cast<unsigned>(
        dl.getTypeSizeInBits(val->getType()));

    const auto func = val->getCalledFunction();
    LOG_IF(FATAL, !func)
        << "Cannot find called function used in call: "
        << remill::LLVMThingToString(val);

    switch (func->getIntrinsicID()) {
      case llvm::Intrinsic::not_intrinsic:
        break;
      case llvm::Intrinsic::ctpop: {
        op = impl->popcounts.Create(res_size);
        op->operands.AddUse(val_to_op[val->getArgOperand(0u)]);
        return;
      }
      case llvm::Intrinsic::ctlz: {
        op = impl->clzs.Create(res_size);
        op->operands.AddUse(val_to_op[val->getArgOperand(0u)]);
        return;
      }
      case llvm::Intrinsic::cttz: {
        op = impl->ctzs.Create(res_size);
        op->operands.AddUse(val_to_op[val->getArgOperand(0u)]);
        return;
      }
      default:
        LOG(FATAL)
            << "Unsupported intrinsic call: " << remill::LLVMThingToString(val);
        return;
    }

    auto name = func->getName();
    if (name.startswith("__remill_read_memory_")) {
      unsigned size = 0;
      if (name.endswith("_8")) {
        size = 8u;
      } else if (name.endswith("_16")) {
        size = 16u;
      } else if (name.endswith("_32")) {
        size = 32u;
      } else if (name.endswith("_64")) {
        size = 64u;
      } else if (name.endswith("_f32")) {
        size = 32u;
      } else if (name.endswith("_f64")) {
        size = 64u;
      } else if (name.endswith("_f80")) {
        size = 80u;
      } else if (name.endswith("_f128")) {
        size = 128u;
      } else {
        LOG(FATAL)
            << "Unsupported memory read intrinsic: "
            << remill::LLVMThingToString(val);
      }

      op = CreateMemoryRead(val, size);

    } else if (name.startswith("__remill_write_memory_")) {
      LOG(FATAL)
          << "Memory write intrinsics not yet supported";
    } else {
      LOG(FATAL)
          << "Unsupported function: " << remill::LLVMThingToString(val);
    }
  }

  void VisitBinaryOperator(llvm::Function *, llvm::Use &,
                           llvm::Instruction *val) {
    auto &op = val_to_op[val];
    if (op) {
      return;
    }

    const auto lhs_val = val->getOperand(0u);
    const auto rhs_val = val->getOperand(1u);
    auto lhs_op = val_to_op[lhs_val];
    auto rhs_op = val_to_op[rhs_val];
    CHECK_NOTNULL(lhs_op);
    CHECK_NOTNULL(rhs_op);

    op = impl->llvm_insts.Create(llvm::dyn_cast<llvm::Instruction>(val));

    op->operands.AddUse(lhs_op);
    op->operands.AddUse(rhs_op);
  }

  void VisitUnaryOperator(llvm::Function *, llvm::Use &,
                          llvm::Instruction *val) {
    auto &op = val_to_op[val];
    if (op) {
      return;
    }

    const auto op_val = val->getOperand(0u);
    auto op0 = val_to_op[op_val];
    CHECK_NOTNULL(op0);

    op = impl->llvm_insts.Create(val);
    op->operands.AddUse(op0);
  }

  void VisitAPInt(llvm::Constant *val, llvm::APInt ap_val) {
    auto &op = val_to_op[val];
    if (op) {
      return;
    }

    const auto num_bits = dl.getTypeSizeInBits(val->getType());

    bits.clear();
    ap_val.reverseBits().toStringUnsigned(bits, 2);
    while (bits.size() < num_bits) {
      bits.push_back('0');
    }
    std::reverse(bits.begin(), bits.end());
    auto bits_str = bits.str().str();
    auto &bits_op = bits_to_constants[bits_str];
    if (bits_op) {
      op = bits_op;
      return;
    }

    CHECK_EQ(num_bits, bits_str.size());

    bits_op = impl->constants.Create(
        std::move(bits_str), static_cast<unsigned>(num_bits));
    op = bits_op;
  }

  void VisitConstantInt(llvm::Function *, llvm::Use &, llvm::ConstantInt *val) {
    VisitAPInt(val, val->getValue());
  }

  void VisitConstantFP(llvm::Function *, llvm::Use &, llvm::ConstantFP *val) {
    VisitAPInt(val, val->getValueAPF().bitcastToAPInt());
  }

  const remill::Arch * const arch;
  const llvm::DataLayout &dl;
  Circuit * const impl;
  VerifyInstruction *verifier{nullptr};

  llvm::SmallString<128> bits;
  std::unordered_map<llvm::Value *, Operation *> val_to_op;
  std::unordered_map<llvm::CallInst *, ReadMemoryCondition *> val_to_mem_cond;
  std::unordered_map<std::string, Constant *> bits_to_constants;

 private:
  IRImporter(void) = delete;
};

}  // namespace

std::unique_ptr<Circuit> Circuit::Create(const remill::Arch *arch,
                                       llvm::Function *circuit_func) {
  const auto module = circuit_func->getParent();
  const auto &dl = module->getDataLayout();

  std::unique_ptr<Circuit> impl(new Circuit);

  auto num_inst_parts = 0u;
  auto num_input_regs = 0u;
  auto num_output_regs = 0u;

  IRImporter importer(arch, dl, impl.get());

  auto num_inst_bits = 0u;
  for (auto &arg : circuit_func->args()) {
    if (arg.hasName() && !arg.getName().empty()) {
      break;
    }
    num_inst_bits += static_cast<unsigned>(
        dl.getTypeSizeInBits(arg.getType()));
  }

  const auto inst_bits = impl->inst_bits.Create(num_inst_bits);
  for (auto &arg : circuit_func->args()) {
    const auto arg_size = static_cast<unsigned>(
        dl.getTypeSizeInBits(arg.getType()));
    Operation *op = nullptr;
    if (arg.hasName() && !arg.getName().empty()) {
      CHECK(num_inst_parts);

      // Expected output register.
      if (arg.getName().endswith("_next")) {
        op = impl->output_regs.Create(
            arg_size,
            arg.getName().substr(0u, arg.getName().size() - 5u).str());
        ++num_output_regs;

      // Input register.
      } else {
        op = impl->input_regs.Create(arg_size, arg.getName().str());
        ++num_input_regs;
      }

    // Extract from the instruction bits.
    } else {
      CHECK(!num_input_regs);
      CHECK(!num_output_regs);

      op = impl->extracts.Create(num_inst_bits - arg_size, num_inst_bits);
      op->operands.AddUse(inst_bits);

      ++num_inst_parts;
      num_inst_bits -= arg_size;
    }
    importer.val_to_op.emplace(&arg, op);
  }

  CHECK_LT(0u, num_inst_parts);
  CHECK_LT(0u, num_input_regs);
  CHECK_EQ(num_input_regs, num_output_regs);

  std::unordered_set<Operation *> seen;

  const auto verify_isnt_func = module->getFunction("__circuitous_verify_inst");
  auto all_verifications = impl->xor_all.Create();
  impl->operands.AddUse(all_verifications);

  ForEachCallTo(circuit_func, verify_isnt_func, [&] (llvm::CallInst *verify_isnt_call) {

    const auto verify_inst = impl->verifications.Create();
    importer.verifier = verify_inst;
    all_verifications->operands.AddUse(verify_inst);

    seen.clear();

    for (auto &arg_use : verify_isnt_call->arg_operands()) {
      const auto val = arg_use.get();
      auto &op = importer.val_to_op[val];
      if (op) {
        if (seen.count(op)) {
          continue;
        }
        seen.insert(op);
        verify_inst->operands.AddUse(op);
        continue;
      }

      const auto arg_cmp = llvm::dyn_cast<llvm::CallInst>(arg_use.get());
      CHECK_NOTNULL(arg_cmp);
      CHECK(arg_cmp->getCalledFunction()->getName().startswith("__circuitous_icmp_eq_"));

      const auto proposed_val = arg_cmp->getArgOperand(0u);
      const auto expected_val = arg_cmp->getArgOperand(1u);

      // Usually this means expected val is some instruction bits.
      if (llvm::isa<llvm::Constant>(expected_val)) {
        importer.Visit(circuit_func, arg_cmp->getArgOperandUse(1u));
      }

      auto &lhs_op = importer.val_to_op[proposed_val];
      const auto rhs_op = importer.val_to_op[expected_val];

      CHECK_NOTNULL(rhs_op);

      if (const auto output_reg = dynamic_cast<OutputRegister *>(rhs_op);
          output_reg) {

        // Proposed valid
        if (lhs_op) {
          if (const auto input_reg = dynamic_cast<InputRegister *>(lhs_op);
              input_reg) {
            if (input_reg->reg_name == output_reg->reg_name) {
              op = impl->preserved_regs.Create();
            } else {
              op = impl->copied_regs.Create();
            }
          } else {
            op = impl->transitions.Create();
          }

        // Proposed value of this register is dynamically computed.
        } else {
          importer.Visit(circuit_func, arg_cmp->getArgOperandUse(0u));
          CHECK_NOTNULL(lhs_op);
          op = impl->transitions.Create();
        }

      } else if (const auto output_bits = dynamic_cast<Extract *>(rhs_op);
                 output_bits) {

        CHECK_EQ(output_bits->operands[0]->op_code,
                 Operation::kInputInstructionBits);

        if (!lhs_op) {
          importer.Visit(circuit_func, arg_cmp->getArgOperandUse(0u));
          CHECK_NOTNULL(lhs_op);
        }

        op = impl->decode_conditions.Create();

      } else {
        LOG(FATAL)
            << "Unexpected argument: " << remill::LLVMThingToString(expected_val);
      }

      op->operands.AddUse(lhs_op);
      op->operands.AddUse(rhs_op);
      verify_inst->operands.AddUse(op);
      seen.insert(op);
    }
  });

  return impl;
}

}  // namespace circuitous
