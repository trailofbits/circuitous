/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Circuit.hpp>
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

namespace circ {
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

  // Bottom out at an argument; it should be an input register.
  if (auto arg_val = llvm::dyn_cast<llvm::Argument>(val)) {
    return self->VisitArgument(context, arg_val);
  }

  // Instruction; follow the dependency chain.
  if (auto inst_val = llvm::dyn_cast<llvm::Instruction>(val)) {
    if (auto call_val = llvm::dyn_cast<llvm::CallInst>(inst_val)) {
      for (auto &op_use : call_val->arg_operands()) {
        self->Visit(context, op_use);
      }

      return self->VisitFunctionCall(context, call_val);

    }
    for (auto &op_use : inst_val->operands()) {
      self->Visit(context, op_use);
    }

    if (llvm::isa<llvm::BinaryOperator>(inst_val) ||
        llvm::isa<llvm::CmpInst>(inst_val)) {
      return self->VisitBinaryOperator(context, inst_val);
    }

    if (llvm::isa<llvm::UnaryInstruction>(inst_val)) {
      return self->VisitUnaryOperator(context, inst_val);
    }
    if (llvm::isa<llvm::SelectInst>(inst_val)) {
      return self->VisitSelect(context, inst_val);
    }
    if (auto freeze = llvm::dyn_cast<llvm::FreezeInst>(inst_val)) {
      return self->VisitFreeze(context, freeze);
    }
    LOG(FATAL) << "Unexpected value during visit: " << remill::LLVMThingToString(inst_val);
  }

  // Bottom out at a constant, ignore for now.
  if (auto const_val = llvm::dyn_cast<llvm::Constant>(val)) {
    if (auto undef = llvm::dyn_cast<llvm::UndefValue>(const_val)) {
      return self->VisitUndefined(context, undef);
    }
    if (auto ce = llvm::dyn_cast<llvm::ConstantExpr>(const_val)) {
      auto ce_inst = ce->getAsInstruction();
      auto &entry_block = context->getEntryBlock();
      ce_inst->insertBefore(&*entry_block.getFirstInsertionPt());
      ce->replaceAllUsesWith(ce_inst);
      CHECK_EQ(val, ce_inst);
      return self->Visit(context, val);  // Revisit.
    }
    if (auto ci = llvm::dyn_cast<llvm::ConstantInt>(val)) {
      return self->VisitConstantInt(context, ci);
    }
    if (auto cf = llvm::dyn_cast<llvm::ConstantFP>(val)) {
      return self->VisitConstantFP(context, cf);
    }
    LOG(FATAL)
        << "Unexpected constant encountered during dependency visitor: "
        << remill::LLVMThingToString(val);
  }
  LOG(FATAL) << "Unexpected value encountered during dependency visitor: "
             << remill::LLVMThingToString(val);
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

  static unsigned SizeFromSuffix(llvm::StringRef name) {
    if (name.endswith("_8"))    return 8u;
    if (name.endswith("_16"))   return 16u;
    if (name.endswith("_32"))   return 32u;
    if (name.endswith("_64"))   return 64u;
    if (name.endswith("_f32"))  return 32u;
    if (name.endswith("_f64"))  return 64u;
    if (name.endswith("_f80"))  return 80u;
    if (name.endswith("_f128")) return 128u;

    LOG(FATAL) << "Unsupported memory read intrinsic: " << name.str();
  }

  Operation *VisitExtractIntrinsic(llvm::CallInst *call, llvm::Function *fn) {
    // TODO(lukas): Refactor into separate method and check in a better way
    //              that includes `_avx` variants.
    auto triple = llvm::Triple(fn->getParent()->getTargetTriple());
    CHECK(remill::GetArchName(triple) == remill::kArchAMD64);

    auto [from, size] = intrinsics::Extract::ParseArgs< uint32_t >(fn);

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
    partials = generate_fragments(from ,from + size);

    if (partials.size() == 1) {
      // `Emplace` was not called, therefore manual assignement is needed.
      val_to_op[call] = partials.front();
      return partials.front();
    }

    // x86 immediates are encoded using little-endian however instruction bytes
    // will be encoded differently:
    // ba 12 00 00 00 - mov 12, %rdx
    // If we do extract(32, 0) we end up with `12000000` as number, but we would
    // expect `00000012` therefore we must reorder them and then concat.
    auto full = Emplace< Concat >(call, size);
    for (auto x : partials) {
      full->AddUse(x);
    }
    return full;
  }

  Operation *VisitExtractRawIntrinsic(llvm::CallInst *call, llvm::Function *fn) {
    // TODO(lukas): Refactor into separate method and check in a better way
    //              that includes `_avx` variants.
    auto triple = llvm::Triple(fn->getParent()->getTargetTriple());
    CHECK(remill::GetArchName(triple) == remill::kArchAMD64);

    CHECK(impl->Attr<InputInstructionBits>().Size() == 1);
    const auto &inst_bytes = impl->input_inst_bits();

    auto [from, size] = intrinsics::ExtractRaw::ParseArgs< uint32_t >(fn);
    auto op = Emplace< Extract >(call, from, from + size);

    auto args = CallArgs(call);
    if (!args.empty()) {
      op->AddUse(Fetch(call->getParent()->getParent(), args[0]));
    } else {
      op->AddUse(inst_bytes);
    }
    return op;
  }

  template<typename O, typename ... Args>
  Operation *VisitGenericIntrinsic(llvm::CallInst *call, llvm::Function *fn, Args &&... args) {
    auto out = Emplace< O >(call, std::forward<Args>(args)...);
    for (auto arg : CallArgs(call)) {
      out->AddUse(Fetch(call->getParent()->getParent(), arg));
    }
    return out;
  }

  auto value_size(llvm::Value *val) {
    return static_cast<uint32_t>(dl.getTypeSizeInBits(val->getType()));
  }

  Operation *call_arg(llvm::CallInst *call, uint32_t idx) {
    auto op = call->getArgOperand(idx);
    return Fetch(call->getParent()->getParent(), op);
  }

  Operation *VisitLLVMIntrinsic(llvm::CallInst *call, llvm::Function *fn) {
    switch (fn->getIntrinsicID()) {
      case llvm::Intrinsic::ctpop :
        return VisitGenericIntrinsic< PopulationCount >(call, fn, value_size(call));
      case llvm::Intrinsic::ctlz  : {
        auto out = Emplace< CountLeadingZeroes >(call, value_size(call));
        out->AddUse(call_arg(call, 0u));
        return out;
      }
      case llvm::Intrinsic::cttz  :
        return VisitGenericIntrinsic< CountTrailingZeroes >(call, fn, value_size(call));
      default:
        LOG(FATAL) << "Unsupported intrinsic call: "
                   << remill::LLVMThingToString(call);
        return nullptr;
    }
  }

  Operation *VisitIntrinsic(llvm::CallInst *call, llvm::Function *fn) {
    auto name = fn->getName();
    if (name.startswith("__remill_read_memory_")) {
      LOG(FATAL) << "__remill_read_memory_* should not be present!";
    }
    if (name.startswith("__remill_write_memory_")) {
      LOG(FATAL) << "__remill_write_memory_* should not be present!";
    }

    if (name.startswith("__remill_undefined_")) {
      return Emplace< Undefined >(call, SizeFromSuffix(name));
    }
    if (intrinsics::Extract::IsIntrinsic(fn)) {
      return VisitExtractIntrinsic(call, fn);
    }
    if (intrinsics::ExtractRaw::IsIntrinsic(fn)) {
      return VisitExtractRawIntrinsic(call, fn);
    }
    if (intrinsics::InputImmediate::IsIntrinsic(fn)) {
      auto [size] = intrinsics::InputImmediate::ParseArgs<uint32_t>(fn);
      return VisitGenericIntrinsic< InputImmediate >(call, fn, size);
    }
    if (intrinsics::Xor::IsIntrinsic(fn)) {
      return VisitGenericIntrinsic< OnlyOneCondition >(call, fn);
    }
    if (intrinsics::Concat::IsIntrinsic(fn)) {
      auto [size] = intrinsics::Concat::ParseArgs< uint32_t >(fn);
      return VisitGenericIntrinsic< Concat >(call, fn, size);
    }
    if (intrinsics::Select::IsIntrinsic(fn)) {
      auto [bits, size] = intrinsics::Select::ParseArgs< uint32_t >(fn);
      return VisitGenericIntrinsic< Select >(call, fn, bits, size);
    }
    if (intrinsics::OutputCheck::IsIntrinsic(fn)) {
      return VisitGenericIntrinsic< RegConstraint >(call, fn);
    }
    if (intrinsics::BitCompare::IsIntrinsic(fn)) {
      return VisitGenericIntrinsic< DecodeCondition >(call, fn);
    }
    if (intrinsics::VerifyInst::IsIntrinsic(fn)) {
      return VisitGenericIntrinsic< VerifyInstruction >(call, fn);
    }
    if (intrinsics::Advice::IsIntrinsic(fn)) {
      auto [size] = intrinsics::Advice::ParseArgs< uint32_t >(fn);
      return VisitGenericIntrinsic< Advice >(call, fn, size);
    }
    if (intrinsics::AdviceConstraint::IsIntrinsic(fn)) {
      return VisitGenericIntrinsic< AdviceConstraint >(call, fn);
    }
    if (intrinsics::Or::IsIntrinsic(fn)) {
      return VisitGenericIntrinsic< Or >(call, fn);
    }
    if (intrinsics::Memory::IsIntrinsic(fn)) {
      auto [id, _] = intrinsics::Memory::ParseArgs< uint32_t >(fn);
      return VisitGenericIntrinsic< Memory >(call, fn, id);
    }
    if (intrinsics::And::IsIntrinsic(fn)) {
      return VisitGenericIntrinsic< And >(call, fn);
    }
    if (intrinsics::ReadConstraint::IsIntrinsic(fn)) {
      return VisitGenericIntrinsic< ReadConstraint >(call, fn);
    }
    if (intrinsics::WriteConstraint::IsIntrinsic(fn)) {
      return VisitGenericIntrinsic< WriteConstraint >(call, fn);
    }
    if (intrinsics::UnusedConstraint::IsIntrinsic(fn)) {
      return VisitGenericIntrinsic< UnusedConstraint >(call, fn);
    }
    LOG(FATAL) << "Unsupported function: " << remill::LLVMThingToString(call);
  }

  // This function is responsible for binding some node to `val` inside `val_to_op`.
  void VisitFunctionCall(llvm::Function *, llvm::CallInst *val) {
    if (val_to_op.count(val)) {
      return;
    }

    auto func = val->getCalledFunction();
    LOG_IF(FATAL, !func) << "Cannot find called function used in call: "
                         << remill::LLVMThingToString(val);

    Operation *op = nullptr;
    if (func->getIntrinsicID() != llvm::Intrinsic::not_intrinsic) {
      op = VisitLLVMIntrinsic(val, func);
    } else {
      op = VisitIntrinsic(val, func);
    }
    CHECK(op && val_to_op[val] == op);
  }


  Operation *HandleLLVMOP(llvm::Instruction *inst) {
    auto size = value_size(inst);

    auto handle_predicate = [&](llvm::Instruction *inst_) -> Operation * {
      auto cmp = llvm::dyn_cast< llvm::CmpInst >( inst_ );
      CHECK(cmp);

      switch (cmp->getPredicate()) {
        case llvm::CmpInst::ICMP_EQ:  return Emplace<Icmp_eq>(inst, size);
        case llvm::CmpInst::ICMP_NE:  return Emplace<Icmp_ne>(inst, size);
        case llvm::CmpInst::ICMP_ULT: return Emplace<Icmp_ult>(inst, size);
        case llvm::CmpInst::ICMP_SLT: return Emplace<Icmp_slt>(inst, size);
        case llvm::CmpInst::ICMP_UGT: return Emplace<Icmp_ugt>(inst, size);
        case llvm::CmpInst::ICMP_UGE:  return Emplace<Icmp_uge>(inst, size);
        case llvm::CmpInst::ICMP_ULE:  return Emplace<Icmp_ule>(inst, size);
        case llvm::CmpInst::ICMP_SGT:  return Emplace<Icmp_sgt>(inst, size);
        case llvm::CmpInst::ICMP_SGE:  return Emplace<Icmp_sge>(inst, size);
        case llvm::CmpInst::ICMP_SLE:  return Emplace<Icmp_sle>(inst, size);
        default: LOG(FATAL) << "Cannot lower llvm predicate " << cmp->getPredicate();
      }
    };

    auto op_code = inst->getOpcode();
    switch (op_code) {
      case llvm::Instruction::OtherOps::Select: return Emplace<BSelect>(inst, size);
      case llvm::BinaryOperator::Add: return Emplace<Add>(inst, size);
      case llvm::BinaryOperator::Sub: return Emplace<Sub>(inst, size);
      case llvm::BinaryOperator::Mul: return Emplace<Mul>(inst, size);

      case llvm::BinaryOperator::UDiv: return Emplace<UDiv>(inst, size);
      case llvm::BinaryOperator::SDiv: return Emplace<SDiv>(inst, size);

      case llvm::BinaryOperator::And: return Emplace<CAnd>(inst, size);
      case llvm::BinaryOperator::Or: return Emplace<COr>(inst, size);
      case llvm::BinaryOperator::Xor: return Emplace<CXor>(inst, size);

      case llvm::BinaryOperator::Shl: return Emplace<Shl>(inst, size);
      case llvm::BinaryOperator::LShr: return Emplace<LShr>(inst, size);
      case llvm::BinaryOperator::AShr: return Emplace<AShr>(inst, size);

      case llvm::BinaryOperator::Trunc: LOG(INFO) << size; return Emplace<Trunc>(inst, size);
      case llvm::BinaryOperator::ZExt: return Emplace<ZExt>(inst, size);
      case llvm::BinaryOperator::SExt: return Emplace<SExt>(inst, size);
      case llvm::BinaryOperator::ICmp: return handle_predicate(inst);

      default :
        LOG(FATAL) << "Cannot lower llvm inst: "
                   << llvm::Instruction::getOpcodeName(op_code);
    }
  }

  void VisitSelect(llvm::Function *func, llvm::Instruction *val) {
    if (val_to_op.count(val)) {
      return;
    }

    auto sel = llvm::dyn_cast<llvm::SelectInst>(val);
    if (Fetch(func, sel->getCondition())->op_code == Undefined::kind) {
      Emplace< Undefined >(sel, value_size(sel));
      return;
    }

    auto true_val = Fetch(func, sel->getTrueValue());
    auto false_val = Fetch(func, sel->getFalseValue());

    CHECK(true_val->op_code != Undefined::kind || false_val->op_code != Undefined::kind);

    auto define = [&](auto what) -> Operation * {
      if (what->op_code != Undefined::kind)
        return what;

      auto op = impl->Create< Not >(value_size(val));
      op->AddUse((what == true_val) ? false_val : true_val);
      return op;
    };

    auto op = HandleLLVMOP(val);
    for (const auto &op_ : val->operand_values()) {
      op->AddUse(define(Fetch(func, op_)));
    }
  }

  bool has_undefined_ops(llvm::Function *func, llvm::Instruction *inst) {
    for (const auto &op : inst->operand_values()) {
      if (Fetch(func, op)->op_code == Undefined::kind) {
        return true;
      }
    }
    return false;
  }

  void VisitLLVMOperator(llvm::Function *func, llvm::Instruction *val) {
    if (val_to_op.count(val)) {
      return;
    }

    if (has_undefined_ops(func, val)) {
      Emplace< Undefined >(val, value_size(val));
      return;
    }

    auto op = HandleLLVMOP(val);
    for (const auto &op_ : val->operand_values()) {
      op->AddUse(Fetch(func, op_));
    }
  }

  void VisitBinaryOperator(llvm::Function *func, llvm::Instruction *val) {
    VisitLLVMOperator(func, val);
  }

  void VisitUnaryOperator(llvm::Function *func, llvm::Instruction *val) {
   VisitLLVMOperator(func, val);
  }

  void VisitAPInt(llvm::Constant *val, llvm::APInt ap_val) {
    if (val_to_op.count(val)) {
      return;
    }

    auto num_bits = value_size(val);
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
    Emplace< Undefined >(val, num_bits);
  }

  void VisitConstantInt(llvm::Function *, llvm::ConstantInt *val) { VisitAPInt(val, val->getValue()); }
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
    populate_meta(key, it->second);
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
  std::unordered_map<std::string, Constant *> bits_to_constants;

 private:
  IRImporter(void) = delete;
};

}  // namespace


auto Circuit::make_circuit(
    std::string_view arch_name, std::string_view os_name,
    std::string_view bytes, const Optimizations &opts)
-> circuit_ptr_t
{
  return make_circuit(arch_name, os_name,
                      llvm::StringRef{bytes.data(),
                                      bytes.size()},
                      opts);
}

auto Circuit::make_circuit(
    std::string_view arch_name, std::string_view os_name,
    const llvm::StringRef &buff, const Optimizations &opts)
-> circuit_ptr_t
{

  circ::Ctx ctx{ std::string(os_name), std::string(arch_name) };
  circ::CircuitBuilder builder(ctx);
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

  importer.Emplace<InputTimestamp>(remill::NthArgument(circuit_func, 3));
  importer.Emplace<OutputTimestamp>(remill::NthArgument(circuit_func, 4));

  auto num_input_regs = 0u;
  auto num_output_regs = 0u;
  for (auto &arg : circuit_func->args()) {
    auto arg_size = static_cast<unsigned>(dl.getTypeSizeInBits(arg.getType()));
    // Expected output register.
    if (Names::is_out_reg(&arg)) {
      importer.Emplace<OutputRegister>(&arg, Names::name(&arg).str(), arg_size);
      ++num_output_regs;
    // Input register.
    } else if (Names::is_in_reg(&arg)) {
      importer.Emplace<InputRegister>(&arg, Names::name(&arg).str(), arg_size);
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
    CHECK(importer.get(verify_inst)->op_code == VerifyInstruction::kind);
    all_verifications->AddUse(importer.get(verify_inst));
  };
  intrinsics::VerifyInst::ForAllIn(circuit_func, visit_context);

  return impl;
}

}  // namespace circ
