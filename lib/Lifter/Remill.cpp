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
#include <llvm/IR/CallSite.h>
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

// Keeps track of instruction dependencies.
template <typename T>
class BottomUpDependencyVisitor {
 public:
  void VisitArgument(llvm::Function *, llvm::Argument *) {}
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
    CHECK(val_to_op.count(val));
  }

  // Create an `size`-bit memory read.
  Operation *CreateMemoryRead(llvm::CallInst *read_call, unsigned size) {
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
    LOG(INFO) << "Handling extract intrinsic: " << LLVMName(fn);

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
    auto size = static_cast<uint32_t>(intrinsics::InputImmediate::ParseArgs(fn));
    auto arg_i = call->getOperand(0);
    if (val_to_op.count(arg_i) != 1) {
      LOG(FATAL) << "Something went wrong and argument"
                 << "of InputImmediate intrinsic was not visited";
    }
    auto full = val_to_op[arg_i];
    CHECK(full);
    auto as_input_imm = impl->Create<InputImmediate>(static_cast<uint32_t>(size));
    as_input_imm->AddUse(full);
    return as_input_imm;
  }

  Operation *VisitExtractRawIntrinsic(llvm::Function *fn) {
    LOG(INFO) << "Handling extract raw intrinsic: " << LLVMName(fn);

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
    LOG(INFO) << from << ", " << size;
    op->AddUse(inst_bytes);
    return op;
  }

  Operation *VisitOneOf(llvm::CallInst *call, llvm::Function *fn) {
    auto one_of = impl->Create<OnlyOneCondition>();
    for (auto &arg : llvm::CallSite{call}.args()) {
      arg.get()->print(llvm::errs());
      llvm::errs().flush();
      if (!val_to_op.count(arg.get())) {
        Visit(call->getParent()->getParent(), arg.get());
      }
      auto op = val_to_op[arg.get()];
      one_of->AddUse(op);
    }
    return one_of;
  }

  Operation *VisitConcat(llvm::CallInst *call, llvm::Function *fn) {
    auto size = static_cast<uint32_t>(intrinsics::Concat::ParseArgs(fn));
    auto acc = impl->Create<Concat>(size);

    auto args = CallArgs(call);
    if (args.size() == 1) {
      Visit(call->getParent()->getParent(), args[0]);
      return val_to_op[args[0]];
    }

    for (auto &arg : args) {
      if (!val_to_op.count(arg)) {
        Visit(call->getParent()->getParent(), arg);
      }
      auto op = val_to_op[arg];
      acc->AddUse(op);
    }
    return acc;
  }

  void VisitFunctionCall(llvm::Function *, llvm::CallInst *val) {
    auto &op = val_to_op[val];
    if (op) {
      return;
    }

    const auto res_size =
        static_cast<unsigned>(dl.getTypeSizeInBits(val->getType()));

    const auto func = val->getCalledFunction();
    LOG_IF(FATAL, !func) << "Cannot find called function used in call: "
                         << remill::LLVMThingToString(val);

    switch (func->getIntrinsicID()) {
      case llvm::Intrinsic::not_intrinsic: break;
      case llvm::Intrinsic::ctpop: {
        const auto op0 = val_to_op[val->getArgOperand(0u)];
        if (op0->op_code == Operation::kUndefined) {
          op = impl->Create<Undefined>(res_size);
        } else {
          op = impl->Create<PopulationCount>(res_size);
          op->AddUse(op0);
        }
        return;
      }
      case llvm::Intrinsic::ctlz: {
        const auto op0 = val_to_op[val->getArgOperand(0u)];
        if (op0->op_code == Operation::kUndefined) {
          op = impl->Create<Undefined>(res_size);
        } else {
          op = impl->Create<CountLeadingZeroes>(res_size);
          op->AddUse(op0);
        }
        return;
      }
      case llvm::Intrinsic::cttz: {
        const auto op0 = val_to_op[val->getArgOperand(0u)];
        if (op0->op_code == Operation::kUndefined) {
          op = impl->Create<Undefined>(res_size);
        } else {
          op = impl->Create<CountTrailingZeroes>(res_size);
          op->AddUse(op0);
        }
        return;
      }
      default:
        LOG(FATAL) << "Unsupported intrinsic call: "
                   << remill::LLVMThingToString(val);
        return;
    }

    auto name = func->getName();
    if (name.startswith("__remill_read_memory_")) {
      op = CreateMemoryRead(val, SizeFromSuffix(name));

    } else if (name.startswith("__remill_undefined_")) {
      op = impl->Create<Undefined>(SizeFromSuffix(name));

    } else if (name.startswith("__remill_write_memory_")) {
      LOG(FATAL) << "Memory write intrinsics not yet supported";
    } else if (intrinsics::Extract::IsIntrinsic(func)) {
      op = VisitExtractIntrinsic(func);
    } else if (intrinsics::ExtractRaw::IsIntrinsic(func)) {
      op = VisitExtractRawIntrinsic(func);
    } else if (intrinsics::InputImmediate::IsIntrinsic(func)) {
      op = VisitInputImmediate(val, func);
    } else if (intrinsics::OneOf::IsIntrinsic(func)) {
      op = VisitOneOf(val, func);
    } else if (intrinsics::Concat::IsIntrinsic(func)) {
      op = VisitConcat(val, func);
    } else {
      LOG(FATAL) << "Unsupported function: " << remill::LLVMThingToString(val);
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
    auto &op = val_to_op[val];
    if (op) {
      return;
    }

    const auto num_bits = dl.getTypeSizeInBits(val->getType());

    bits.clear();
    bits.reserve(num_bits);
    ap_val.toStringUnsigned(bits, 2);
    while (bits.size() < num_bits) {
      bits.insert(bits.begin(), '0');
    }
    std::reverse(bits.begin(), bits.end());
    auto bits_str = bits.str().str();

    auto &bits_op = bits_to_constants[bits_str];
    if (bits_op) {
      op = bits_op;
      return;
    }

    CHECK_EQ(num_bits, bits_str.size());

    bits_op = impl->Create<Constant>(std::move(bits_str),
                                     static_cast<unsigned>(num_bits));
    op = bits_op;
  }

  void VisitUndefined(llvm::Function *, llvm::UndefValue *val) {
    auto &op = val_to_op[val];
    if (op) {
      return;
    }

    const auto num_bits = dl.getTypeSizeInBits(val->getType());
    op = impl->Create<Undefined>(static_cast<unsigned>(num_bits));
  }

  void VisitConstantInt(llvm::Function *, llvm::ConstantInt *val) {
    VisitAPInt(val, val->getValue());
  }

  void VisitConstantFP(llvm::Function *, llvm::ConstantFP *val) {
    VisitAPInt(val, val->getValueAPF().bitcastToAPInt());
  }

  const remill::Arch *const arch;
  const llvm::DataLayout &dl;
  Circuit *const impl;
  VerifyInstruction *verifier{nullptr};

  llvm::SmallString<128> bits;
  std::unordered_map<llvm::Value *, Operation *> val_to_op;
  std::unordered_map<llvm::CallInst *, ReadMemoryCondition *> val_to_mem_cond;
  std::unordered_map<std::string, Constant *> bits_to_constants;

 private:
  IRImporter(void) = delete;
};

// Apply a callback `cb(llvm::CallInst *)` to each call of `callee` in the
// function `caller`.
template <typename T>
static void ForEachCallTo(llvm::Function *caller, llvm::Function *callee,
                          T cb) {
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

}  // namespace

std::unique_ptr<Circuit>
Circuit::CreateFromInstructions(const std::string &arch_name,
                                const std::string &os_name,
                                const std::string &file_name,
                                const Optimizations &opts) {
  auto maybe_buff = llvm::MemoryBuffer::getFile(file_name, -1, false);
  if (remill::IsError(maybe_buff)) {
    LOG(ERROR) << remill::GetErrorString(maybe_buff) << std::endl;
    return nullptr;
  }

  const auto buff = remill::GetReference(maybe_buff)->getBuffer();
  return CreateFromInstructions(arch_name, os_name, buff, opts);
}

std::unique_ptr<Circuit>
Circuit::CreateFromInstructions(const std::string &arch_name,
                                const std::string &os_name,
                                std::string_view bytes,
                                const Optimizations &opts) {
  return CreateFromInstructions(arch_name, os_name,
                                llvm::StringRef{bytes.data(),
                                                bytes.size()},
                                opts);
}

std::unique_ptr<Circuit>
Circuit::CreateFromInstructions(const std::string &arch_name,
                                const std::string &os_name,
                                const llvm::StringRef &buff,
                                const Optimizations &opts) {

  circuitous::Ctx ctx{ os_name, arch_name };
  circuitous::CircuitBuilder builder(ctx);
  builder.reduce_imms = opts.reduce_imms;

  const auto arch = builder.ctx.arch();
  const auto circuit_func = builder.Build(buff);

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
    num_inst_bits += static_cast<unsigned>(dl.getTypeSizeInBits(arg.getType()));
  }

  const auto inst_bits = impl->Create<InputInstructionBits>(num_inst_bits);
  for (auto &arg : circuit_func->args()) {
    const auto arg_size =
        static_cast<unsigned>(dl.getTypeSizeInBits(arg.getType()));
    Operation *op = nullptr;
    if (arg.hasName() && !arg.getName().empty()) {

      // CHECK(num_inst_parts);

      // Expected output register.
      if (arg.getName().endswith("_next")) {
        op = impl->Create<OutputRegister>(
            arg_size,
            arg.getName().substr(0u, arg.getName().size() - 5u).str());
        ++num_output_regs;

      // Input register.
      } else {
        op = impl->Create<InputRegister>(arg_size, arg.getName().str());
        ++num_input_regs;
      }

    // Extract from the instruction bits.
    } else {
      CHECK(!num_input_regs);
      CHECK(!num_output_regs);

      op = impl->Create<Extract>(num_inst_bits - arg_size, num_inst_bits);
      op->AddUse(inst_bits);

      ++num_inst_parts;
      num_inst_bits -= arg_size;
    }
    importer.val_to_op.emplace(&arg, op);
  }

  // CHECK_LT(0u, num_inst_parts);
  CHECK_LT(0u, num_input_regs);
  CHECK_EQ(num_input_regs, num_output_regs);

  std::unordered_set<Operation *> seen;

  auto verify_inst_func = intrinsics::VerifyInst::CreateFn(module);
  auto all_verifications = impl->Create<OnlyOneCondition>();
  impl->AddUse(all_verifications);

  ForEachCallTo(
      circuit_func, verify_inst_func, [&](llvm::CallInst *verify_isnt_call) {
        const auto verify_inst = impl->Create<VerifyInstruction>();
        importer.verifier = verify_inst;
        all_verifications->AddUse(verify_inst);

        seen.clear();

        for (auto &arg_use : verify_isnt_call->arg_operands()) {
          const auto val = arg_use.get();
          auto &op = importer.val_to_op[val];
          if (op) {
            if (seen.count(op)) {
              continue;
            }
            seen.insert(op);
            verify_inst->AddUse(op);
            continue;
          }

          const auto arg_cmp = llvm::cast<llvm::CallInst>(arg_use.get());

          if (intrinsics::OneOf::IsIntrinsic(arg_cmp->getCalledFunction())) {
            importer.Visit(circuit_func, arg_cmp);
            verify_inst->AddUse(importer.val_to_op[arg_cmp]);
            continue;
          }

          CHECK(intrinsics::Eq::IsIntrinsic(arg_cmp->getCalledFunction()) ||
                intrinsics::BitCompare::IsIntrinsic(arg_cmp->getCalledFunction()) ||
                intrinsics::OneOf::IsIntrinsic(arg_cmp->getCalledFunction()));

          const auto proposed_val = arg_cmp->getArgOperand(0u);
          const auto expected_val = arg_cmp->getArgOperand(1u);

          importer.Visit(circuit_func, arg_cmp->getArgOperandUse(0u));
          importer.Visit(circuit_func, arg_cmp->getArgOperandUse(1u));

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
                  op = impl->Create<PreservedCondition>();
                } else {
                  op = impl->Create<CopyCondition>();
                }
              } else {
                op = impl->Create<RegisterCondition>();
              }

            // Proposed value of this register is dynamically computed.
            } else {
              importer.Visit(circuit_func, arg_cmp->getArgOperandUse(0u));
              CHECK_NOTNULL(lhs_op);
              op = impl->Create<RegisterCondition>();
            }

          } else if (const auto output_bits = dynamic_cast<Extract *>(rhs_op);
                     output_bits) {

            CHECK_EQ(output_bits->operands[0]->op_code,
                     Operation::kInputInstructionBits);

            if (!lhs_op) {
              importer.Visit(circuit_func, arg_cmp->getArgOperandUse(0u));
              CHECK_NOTNULL(lhs_op);
            }

            op = impl->Create<DecodeCondition>();
          } else {
            LOG(WARNING) << "Unexpected argument: "
                       << remill::LLVMThingToString(expected_val);
          }

          op->AddUse(lhs_op);
          op->AddUse(rhs_op);
          verify_inst->AddUse(op);
          seen.insert(op);
        }
      });

  return impl;
}

}  // namespace circuitous
