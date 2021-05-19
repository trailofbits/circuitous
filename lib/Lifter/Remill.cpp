/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Lifter.hpp>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <glog/logging.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#pragma clang diagnostic pop
#include <remill/Arch/Arch.h>
#include <remill/Arch/Name.h>
#include <remill/BC/Compat/Error.h>
#include <remill/BC/Util.h>
#include <remill/OS/OS.h>

#include "CircuitBuilder.h"

namespace circuitous {
namespace {

auto CallArgs(llvm::CallInst *call) {
  std::vector<llvm::Value *> out;
  for (uint32_t i = 0; i < call->getNumArgOperands(); ++i) {
    // NOTE(lukas): Check if we do not include the called fn by accident.
    CHECK(!llvm::isa<llvm::Function>(call->getArgOperand(i)));
    out.push_back(call->getArgOperand(i));
  }
  return out;
}

// Keeps track of instruction dependencies.
template <typename T>
class BottomUpDependencyVisitor {
 public:
  void VisitArgument(llvm::Function *, llvm::Argument *) {}
  void VisitFreeze(llvm::Function *, llvm::FreezeInst *) {}
  void VisitFunctionCall(llvm::Function *, llvm::CallInst *) {}
  void VisitBinaryOperator(llvm::Function *, llvm::Instruction *) {}
  void VisitSelect(llvm::Function *, llvm::Instruction *);
  void VisitUnaryOperator(llvm::Function *, llvm::Instruction *) {}
  void VisitUndefined(llvm::Function *, llvm::UndefValue *) {}
  void VisitConstantInt(llvm::Function *, llvm::ConstantInt *) {}
  void VisitConstantFP(llvm::Function *, llvm::ConstantFP *) {}
  void Visit(llvm::Function *context, llvm::Use &use_) {
    return Visit(context, use_.get());
  }
  void Visit(llvm::Function *context, llvm::Value *val);
};

// Analyze how `use_` is produced.
template <typename T>
void BottomUpDependencyVisitor<T>::Visit(llvm::Function *context,
                                         llvm::Value *val) {
  auto self = static_cast<T *>(this);

  //const auto val = use.get();

  // Bottom out at an argument; it should be an input register.
  if (auto arg_val = llvm::dyn_cast<llvm::Argument>(val); arg_val) {
    self->VisitArgument(context, arg_val);

  // Instruction; follow the dependency chain.
  } else if (auto inst_val = llvm::dyn_cast<llvm::Instruction>(val); inst_val) {
    if (auto call_val = llvm::dyn_cast<llvm::CallInst>(inst_val); call_val) {
      for (auto &op_use : call_val->arg_operands()) {
        self->Visit(context, op_use);
      }

      self->VisitFunctionCall(context, call_val);

    } else {
      for (auto &op_use : inst_val->operands()) {
        self->Visit(context, op_use);
      }

      if (llvm::isa<llvm::BinaryOperator>(inst_val) ||
          llvm::isa<llvm::CmpInst>(inst_val)) {
        self->VisitBinaryOperator(context, inst_val);

      } else if (llvm::isa<llvm::UnaryInstruction>(inst_val)) {
        self->VisitUnaryOperator(context, inst_val);

      } else if (llvm::isa<llvm::SelectInst>(inst_val)) {
        self->VisitSelect(context, inst_val);

      } else if (auto freeze = llvm::dyn_cast<llvm::FreezeInst>(inst_val)) {
        self->VisitFreeze(context, freeze);
      } else {
        LOG(FATAL) << "Unexpected value during visit: "
                   << remill::LLVMThingToString(inst_val);
      }
    }

  // Bottom out at a constant, ignore for now.
  } else if (auto const_val = llvm::dyn_cast<llvm::Constant>(val); const_val) {
    if (auto undef = llvm::dyn_cast<llvm::UndefValue>(const_val); undef) {
      self->VisitUndefined(context, undef);

    } else if (auto ce = llvm::dyn_cast<llvm::ConstantExpr>(const_val); ce) {
      auto ce_inst = ce->getAsInstruction();
      auto &entry_block = context->getEntryBlock();
      ce_inst->insertBefore(&*entry_block.getFirstInsertionPt());
      ce->replaceAllUsesWith(ce_inst);
      CHECK_EQ(val, ce_inst);
      self->Visit(context, val);  // Revisit.

    } else if (auto ci = llvm::dyn_cast<llvm::ConstantInt>(val); ci) {
      self->VisitConstantInt(context, ci);

    } else if (auto cf = llvm::dyn_cast<llvm::ConstantFP>(val); cf) {
      self->VisitConstantFP(context, cf);

    } else {
      LOG(FATAL)
          << "Unexpected constant encountered during dependency visitor: "
          << remill::LLVMThingToString(val);
    }
  } else {
    LOG(FATAL) << "Unexpected value encountered during dependency visitor: "
               << remill::LLVMThingToString(val);
  }
}

class IRImporter : public BottomUpDependencyVisitor<IRImporter> {
 public:
  explicit IRImporter(const remill::Arch *arch_, const llvm::DataLayout &dl_,
                      Circuit *impl_)
      : arch(arch_),
        dl(dl_),
        impl(impl_) {}

  void VisitArgument(llvm::Function *, llvm::Argument *val) {
    CHECK(val_to_op.count(val))
        << remill::LLVMThingToString(val) << " not present IRImporter.";
  }

  void VisitFreeze(llvm::Function *fn, llvm::FreezeInst *freeze) {
    auto arg = freeze->getOperand(0u);
    Visit(fn, arg);
    val_to_op[freeze] = val_to_op[arg];
  }

  // Create an `size`-bit memory read.
  Operation *CreateMemoryRead(llvm::CallInst *read_call, unsigned size) {
    LOG(FATAL) << "TODO(lukas): Pending refactor.";
    auto &read = val_to_mem_cond[read_call];
    if (read) {
      verifier->AddUse(read);
      return read->operands[ReadMemoryCondition::kHintedValue];
    }

    Hint *expected_addr = impl->Create<Hint>(arch->address_size);
    Hint *bytes[kMaxNumBytesRead] = {};
    for (auto i = 0u, j = 0u; i < size; i += 8u, j += 1u) {
      bytes[j] = impl->Create<Hint>(8u);
    }

    Operation *value_read = nullptr;

    const auto num_bytes = size / 8u;
    if (1 == num_bytes) {
      value_read = bytes[0u];

    } else {
      value_read = impl->Create<Concat>(size);
      if (arch->MemoryAccessIsLittleEndian()) {
        for (auto i = 0u, j = num_bytes; i < num_bytes; ++i) {
          value_read->AddUse(bytes[--j]);
        }
      } else {
        for (auto i = 0u; i < num_bytes; ++i) {
          value_read->AddUse(bytes[i]);
        }
      }
    }

    read = impl->Create<ReadMemoryCondition>();
    read->AddUse(val_to_op[read_call->getArgOperand(1u)]);
    read->AddUse(expected_addr);
    read->AddUse(value_read);

    verifier->AddUse(read);

    LOG(ERROR) << "TODO: Should set weak conditions when reading memory.";
    return value_read;
  }

  static unsigned SizeFromSuffix(llvm::StringRef name) {
    if (name.endswith("_8")) {
      return 8u;
    } else if (name.endswith("_16")) {
      return 16u;
    } else if (name.endswith("_32")) {
      return 32u;
    } else if (name.endswith("_64")) {
      return 64u;
    } else if (name.endswith("_f32")) {
      return 32u;
    } else if (name.endswith("_f64")) {
      return 64u;
    } else if (name.endswith("_f80")) {
      return 80u;
    } else if (name.endswith("_f128")) {
      return 128u;
    } else {
      LOG(FATAL) << "Unsupported memory read intrinsic: " << name.str();
      return 0u;
    }
  }

  Operation *VisitExtractIntrinsic(llvm::Function *fn) {
    // TODO(lukas): Refactor into separate method and check in a better way
    //              that includes `_avx` variants.
    auto triple = llvm::Triple(fn->getParent()->getTargetTriple());
    CHECK(remill::GetArchName(triple) == remill::kArchAMD64);

    const auto &[from, size] = intrinsics::Extract::ParseArgs(fn);

    CHECK(impl->Attr<InputInstructionBits>().Size() == 1);
    const auto &inst_bytes = *impl->Attr<InputInstructionBits>().begin();

    // We split extract to sepratate bytes. This is so we can reorder them,
    // which can be handy if the extracted data are in a different order
    // (endiannity for example).
    const unsigned step = 8;
    std::deque<Operation *> partials;
    auto generate_fragments = [&](uint32_t from, uint32_t to) {
      std::deque<Operation *> partials;
      while (true) {
        uint32_t y = std::min(from + (step - from % step), to);
        auto op = impl->Create<Extract>(from, y);
        op->AddUse(inst_bytes);
        partials.push_front(op);
        if (y == to) {
          return partials;
        }
        from = y;
      }
    };
    partials =  generate_fragments(static_cast<uint32_t>(from),
                                   static_cast<uint32_t>(from + size));

    if (partials.size() == 1) {
      return partials.front();
    }

    // x86 immediates are encoded using little-endian however instruction bytes
    // will be encoded differently:
    // ba 12 00 00 00 - mov 12, %rdx
    // If we do extract(32, 0) we end up with `12000000` as number, but we would
    // expect `00000012` therefore we must reorder them and then concat.
    auto full = impl->Create<Concat>(static_cast<unsigned>(size));
    for (auto x : partials) {
      full->AddUse(x);
    }
    return full;
  }

  Operation *VisitInputImmediate(llvm::CallInst *call, llvm::Function *fn) {
    auto [size] = intrinsics::InputImmediate::ParseArgs<uint32_t>(fn);
    return VisitGenericIntrinsic<InputImmediate>(call, fn, size);
  }

  Operation *VisitExtractRawIntrinsic(llvm::CallInst *call, llvm::Function *fn) {
    // TODO(lukas): Refactor into separate method and check in a better way
    //              that includes `_avx` variants.
    auto triple = llvm::Triple(fn->getParent()->getTargetTriple());
    CHECK(remill::GetArchName(triple) == remill::kArchAMD64);

    CHECK(impl->Attr<InputInstructionBits>().Size() == 1);
    const auto &inst_bytes = *impl->Attr<InputInstructionBits>().begin();

    const auto &[from, size] = intrinsics::ExtractRaw::ParseArgs(fn);
    auto op = impl->Create<Extract>(
        static_cast<unsigned>(from),
        static_cast<unsigned>(from + size));

    auto args = CallArgs(call);
    if (!args.empty()) {
      Visit(call->getParent()->getParent(), args[0]);
      op->AddUse(val_to_op[args[0]]);
    } else {
      op->AddUse(inst_bytes);
    }
    return op;
  }

  template<typename O, typename ... Args>
  Operation *VisitGenericIntrinsic(
      llvm::CallInst *call, llvm::Function *fn, Args &&... args) {
    auto out = impl->Create<O>(std::forward<Args>(args)...);
    for (auto arg : CallArgs(call)) {
      if (!val_to_op.count(arg)) {
        Visit(call->getParent()->getParent(), arg);
      }
      auto op = val_to_op[arg];
      out->AddUse(op);
    }
    return out;
  }

  Operation *VisitOutputCheckIntrinsic(llvm::CallInst *call, llvm::Function *fn) {
    return VisitGenericIntrinsic<RegisterCondition>(call, fn);
  }

  Operation *VisitXor(llvm::CallInst *call, llvm::Function *fn) {
    return VisitGenericIntrinsic<OnlyOneCondition>(call, fn);
  }

  Operation *VisitBitCompare(llvm::CallInst *call, llvm::Function *fn) {
    return VisitGenericIntrinsic<DecodeCondition>(call, fn);
  }

  Operation *VisitVerifyInst(llvm::CallInst *call, llvm::Function *fn) {
    return VisitGenericIntrinsic<VerifyInstruction>(call, fn);
  }

  Operation *VisitHintCheckIntrinsics(llvm::CallInst *call, llvm::Function *fn) {
    return VisitGenericIntrinsic<HintCondition>(call, fn);
  }

  Operation *VisitHintIntrinsic(llvm::CallInst *call, llvm::Function *fn) {
    auto [size] = intrinsics::Hint::ParseArgs<uint32_t>(fn);
    return VisitGenericIntrinsic<Hint>(call, fn, size);
  }

  Operation *VisitSelectIntrinsic(llvm::CallInst *call, llvm::Function *fn) {
    auto [bits, size] = intrinsics::Select::ParseArgs<uint32_t>(fn);
    return VisitGenericIntrinsic<Select>(call, fn, bits, size);
  }

  Operation *VisitConcat(llvm::CallInst *call, llvm::Function *fn) {
    auto args = CallArgs(call);
    if (args.size() == 1) {
      Visit(call->getParent()->getParent(), args[0]);
      return val_to_op[args[0]];
    }

    auto [size] = intrinsics::Concat::ParseArgs<uint32_t>(fn);
    return VisitGenericIntrinsic<Concat>(call, fn, size);
  }

  template<typename T>
  Operation *LowerLLVMIntrinsic(llvm::CallInst *call, llvm::Function *fn) {
    auto res_size = static_cast<uint32_t>(dl.getTypeSizeInBits(call->getType()));
    auto arg_0 = call->getArgOperand(0u);
    if (!val_to_op.count(arg_0)) {
      Visit(call->getParent()->getParent(), arg_0);
    }
    if (val_to_op[arg_0]->op_code == Operation::kUndefined) {
      return impl->Create<Undefined>(res_size);
    }
    return VisitGenericIntrinsic<T>(call, fn, res_size);
  }

  Operation *VisitLLVMIntrinsic(llvm::CallInst *call, llvm::Function *fn) {
    switch (fn->getIntrinsicID()) {
      case llvm::Intrinsic::ctpop : return LowerLLVMIntrinsic<PopulationCount>(call, fn);
      case llvm::Intrinsic::ctlz  : return LowerLLVMIntrinsic<CountLeadingZeroes>(call, fn);
      case llvm::Intrinsic::cttz  : return LowerLLVMIntrinsic<CountTrailingZeroes>(call, fn);
      default:
        LOG(FATAL) << "Unsupported intrinsic call: "
                   << remill::LLVMThingToString(call);
        return nullptr;
    }
  }

  Operation *VisitIntrinsic(llvm::CallInst *call, llvm::Function *fn) {
    auto name = fn->getName();
    if (name.startswith("__remill_read_memory_")) {
      return CreateMemoryRead(call, SizeFromSuffix(name));
    }
    if (name.startswith("__remill_undefined_")) {
      return impl->Create<Undefined>(SizeFromSuffix(name));
    }
    if (name.startswith("__remill_write_memory_")) {
      LOG(FATAL) << "Memory write intrinsics not yet supported";
    }
    if (intrinsics::Extract::IsIntrinsic(fn)) {
      return VisitExtractIntrinsic(fn);
    }
    if (intrinsics::ExtractRaw::IsIntrinsic(fn)) {
      return VisitExtractRawIntrinsic(call, fn);
    }
    if (intrinsics::InputImmediate::IsIntrinsic(fn)) {
      return VisitInputImmediate(call, fn);
    }
    if (intrinsics::Xor::IsIntrinsic(fn)) {
      return VisitXor(call, fn);
    }
    if (intrinsics::Concat::IsIntrinsic(fn)) {
      return VisitConcat(call, fn);
    }
    if (intrinsics::Select::IsIntrinsic(fn)) {
      return VisitSelectIntrinsic(call, fn);
    }
    if (intrinsics::OutputCheck::IsIntrinsic(fn)) {
      return VisitOutputCheckIntrinsic(call, fn);
    }
    if (intrinsics::BitCompare::IsIntrinsic(fn)) {
      return VisitBitCompare(call, fn);
    }
    if (intrinsics::VerifyInst::IsIntrinsic(fn)) {
      return VisitVerifyInst(call, fn);
    }
    if (intrinsics::Hint::IsIntrinsic(fn)) {
      return VisitHintIntrinsic(call, fn);
    }
    if (intrinsics::HintCheck::IsIntrinsic(fn)) {
      return VisitHintCheckIntrinsics(call, fn);
    }
    LOG(FATAL) << "Unsupported function: " << remill::LLVMThingToString(call);
  }

  void VisitFunctionCall(llvm::Function *, llvm::CallInst *val) {
    if (val_to_op.count(val)) {
      return;
    }
    const auto func = val->getCalledFunction();
    LOG_IF(FATAL, !func) << "Cannot find called function used in call: "
                         << remill::LLVMThingToString(val);

    if (func->getIntrinsicID() != llvm::Intrinsic::not_intrinsic) {
      val_to_op[val] = VisitLLVMIntrinsic(val, func);
    } else {
      val_to_op[val] = VisitIntrinsic(val, func);
    }
  }

  void VisitSelect(llvm::Function *func, llvm::Instruction *val) {
    auto &op = val_to_op[val];
    if (op) {
      return;
    }

    const auto sel = llvm::dyn_cast<llvm::SelectInst>(val);
    const auto cond_val = sel->getCondition();
    const auto true_val = sel->getTrueValue();
    const auto false_val = sel->getFalseValue();

    auto cond_op = val_to_op[cond_val];
    CHECK_NOTNULL(cond_op);

    const auto num_bits =
        static_cast<unsigned>(dl.getTypeSizeInBits(val->getType()));

    // The condition is undefined, that means we aren't selecting either value,
    // unfortunately :-(
    if (cond_op->op_code == Operation::kUndefined) {
      op = impl->Create<Undefined>(static_cast<unsigned>(num_bits));

    // Condition is defined.
    } else {
      auto true_op = val_to_op[true_val];
      auto false_op = val_to_op[false_val];
      CHECK_NOTNULL(true_val);
      CHECK_NOTNULL(false_op);
      CHECK_EQ(num_bits, true_op->size);
      CHECK_EQ(num_bits, false_op->size);

      // Both selected values are undefined, thus the result is undefined.
      if (true_op->op_code == Operation::kUndefined &&
          false_op->op_code == Operation::kUndefined) {
        op = true_op;
        return;
      }

      op = impl->Create<LLVMOperation>(val);
      op->AddUse(cond_op);

      // True side is undefined; convert it into a defined value that is not
      // the same as the False side.
      if (true_op->op_code == Operation::kUndefined) {
        auto not_false_op = impl->Create<Not>(num_bits);
        not_false_op->AddUse(false_op);

        op->AddUse(not_false_op);
        op->AddUse(false_op);

      // False side is undefined; convert it into a defined value that is not
      // the same as the True side.
      } else if (false_op->op_code == Operation::kUndefined) {
        auto not_true_op = impl->Create<Not>(num_bits);
        not_true_op->AddUse(true_op);

        op->AddUse(true_op);
        op->AddUse(not_true_op);

      // Neither is undefined, yay!
      } else {
        op->AddUse(true_op);
        op->AddUse(false_op);
      }
    }
  }

  void VisitBinaryOperator(llvm::Function *func,
                           llvm::Instruction *val) {
    auto &op = val_to_op[val];
    if (op) {
      return;
    }

    const auto lhs_val = val->getOperand(0u);
    const auto rhs_val = val->getOperand(1u);
    Visit(func, lhs_val);
    Visit(func, rhs_val);
    auto lhs_op = val_to_op[lhs_val];
    auto rhs_op = val_to_op[rhs_val];
    CHECK_NOTNULL(lhs_op);
    CHECK_NOTNULL(rhs_op);

    // Fold undefined values.
    if (lhs_op->op_code == Operation::kUndefined ||
        rhs_op->op_code == Operation::kUndefined) {
      const auto num_bits = dl.getTypeSizeInBits(val->getType());
      op = impl->Create<Undefined>(static_cast<unsigned>(num_bits));

    } else {
      op = impl->Create<LLVMOperation>(val);
      op->AddUse(lhs_op);
      op->AddUse(rhs_op);
    }
  }

  void VisitUnaryOperator(llvm::Function *func,
                          llvm::Instruction *val) {
    auto &op = val_to_op[val];
    if (op) {
      return;
    }

    const auto op_val = val->getOperand(0u);
    auto op0 = val_to_op[op_val];
    CHECK_NOTNULL(op0);

    if (op0->op_code == Operation::kUndefined) {
      const auto num_bits = dl.getTypeSizeInBits(val->getType());
      op = impl->Create<Undefined>(static_cast<unsigned>(num_bits));
    } else {
      op = impl->Create<LLVMOperation>(val);
      op->AddUse(op0);
    }
  }

  void VisitAPInt(llvm::Constant *val, llvm::APInt ap_val) {
    if (val_to_op.count(val)) {
      return;
    }

    const auto num_bits = dl.getTypeSizeInBits(val->getType());
    llvm::SmallString<128> bits;

    bits.reserve(num_bits);
    ap_val.toStringUnsigned(bits, 2);
    while (bits.size() < num_bits) {
      bits.insert(bits.begin(), '0');
    }
    std::reverse(bits.begin(), bits.end());
    auto bits_str = bits.str().str();

    auto &bits_op = bits_to_constants[bits_str];
    if (!bits_op) {
      CHECK_EQ(num_bits, bits_str.size());
      bits_op = impl->Create<Constant>(std::move(bits_str),
                                      static_cast<unsigned>(num_bits));
    }
    val_to_op[val] = bits_op;
  }

  void VisitUndefined(llvm::Function *, llvm::UndefValue *val) {
    if (val_to_op.count(val)) {
      return;
    }

    auto num_bits = static_cast<uint32_t>(dl.getTypeSizeInBits(val->getType()));
    Emplace<Undefined>(val, num_bits);
  }

  void VisitConstantInt(llvm::Function *, llvm::ConstantInt *val) {
    VisitAPInt(val, val->getValue());
  }

  void VisitConstantFP(llvm::Function *, llvm::ConstantFP *val) {
    VisitAPInt(val, val->getValueAPF().bitcastToAPInt());
  }

  Operation *Fetch(llvm::Function *fn, llvm::Value *val) {
    if (!val_to_op.count(val)) {
      Visit(fn, val);
    }
    return val_to_op[val];
  }

  template<typename T, typename ...Args>
  Operation* Emplace(llvm::Value *key, Args &&... args) {
    auto [it, _] = val_to_op.emplace(key, impl->Create<T>(std::forward<Args>(args)...));
    return it->second;
  }

  template<bool allow_failure=false>
  Operation *get(llvm::Value *key) const {
    auto it = val_to_op.find(key);
    if constexpr (!allow_failure) {
      CHECK(it != val_to_op.end() && it->second);
    }
    return (it != val_to_op.end()) ? it->second : nullptr;
  }

  bool contains(llvm::Value *key) const { return val_to_op.count(key); }

  const remill::Arch *arch;
  const llvm::DataLayout &dl;
  Circuit *impl;
  VerifyInstruction *verifier{nullptr};

  llvm::SmallString<128> bits;
  std::unordered_map<llvm::Value *, Operation *> val_to_op;
  std::unordered_map<llvm::CallInst *, ReadMemoryCondition *> val_to_mem_cond;
  std::unordered_map<std::string, Constant *> bits_to_constants;

 private:
  IRImporter(void) = delete;
};

}  // namespace

auto Circuit::make_circuit(
    const std::string &arch_name, const std::string &os_name,
    const std::string &file_name, const Optimizations &opts)
-> circuit_ptr_t
{
  auto maybe_buff = llvm::MemoryBuffer::getFile(file_name, -1, false);
  if (remill::IsError(maybe_buff)) {
    LOG(ERROR) << remill::GetErrorString(maybe_buff) << std::endl;
    return nullptr;
  }

  const auto buff = remill::GetReference(maybe_buff)->getBuffer();
  return make_circuit(arch_name, os_name, buff, opts);
}

auto Circuit::make_circuit(
    const std::string &arch_name, const std::string &os_name,
    std::string_view bytes, const Optimizations &opts)
-> circuit_ptr_t
{
  return make_circuit(arch_name, os_name,
                      llvm::StringRef{bytes.data(),
                                      bytes.size()},
                      opts);
}

auto Circuit::make_circuit(
    const std::string &arch_name, const std::string &os_name,
    const llvm::StringRef &buff, const Optimizations &opts)
-> circuit_ptr_t
{

  circuitous::Ctx ctx{ os_name, arch_name };
  circuitous::CircuitBuilder builder(ctx);
  builder.reduce_imms = opts.reduce_imms;

  const auto arch = builder.ctx.arch();
  const auto circuit_func = builder.Build(buff);
  LOG(INFO) << "FINAL";
  circuit_func->print(llvm::errs());
  llvm::errs().flush();
  LOG(INFO) << "DONE";

  const auto module = circuit_func->getParent();
  const auto &dl = module->getDataLayout();

  auto impl = std::make_unique<Circuit>();
  IRImporter importer(arch, dl, impl.get());

  importer.Emplace<InputInstructionBits>(remill::NthArgument(circuit_func, 0), kMaxNumInstBits);
  importer.Emplace<InputErrorFlag>(remill::NthArgument(circuit_func, 1));
  importer.Emplace<OutputErrorFlag>(remill::NthArgument(circuit_func, 2));

  auto num_input_regs = 0u;
  auto num_output_regs = 0u;
  for (auto &arg : circuit_func->args()) {
    auto arg_size = static_cast<unsigned>(dl.getTypeSizeInBits(arg.getType()));
    // Expected output register.
    if (Names::is_out_reg(&arg)) {
      importer.Emplace<OutputRegister>(&arg, arg_size, Names::name(&arg).str());
      ++num_output_regs;
    // Input register.
    } else if (Names::is_in_reg(&arg)) {
      importer.Emplace<InputRegister>(&arg, arg_size, Names::name(&arg).str());
      ++num_input_regs;
    }
  }

  // CHECK_LT(0u, num_inst_parts);
  CHECK_LT(0u, num_input_regs);
  CHECK_EQ(num_input_regs, num_output_regs);

  auto all_verifications = impl->Create<OnlyOneCondition>();
  impl->AddUse(all_verifications);

  auto visit_context = [&](llvm::CallInst *verify_inst) {
    importer.Visit(circuit_func, verify_inst);
    CHECK(importer.get(verify_inst)->op_code == Operation::kVerifyInstruction);
    all_verifications->AddUse(importer.get(verify_inst));
  };
  intrinsics::VerifyInst::ForAllIn(circuit_func, visit_context);

  return impl;
}

}  // namespace circuitous
