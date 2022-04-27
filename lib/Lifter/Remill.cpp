/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/LLVMToCircIR.hpp>

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Memory.hpp>
#include <circuitous/IR/Lifter.hpp>
#include <circuitous/IR/Verify.hpp>

#include <circuitous/Lifter/CircuitBuilder.hpp>
#include <circuitous/Lifter/CircuitSmithy.hpp>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Logging.hpp>

#include <circuitous/Dbg/CtxPrint.hpp>

#include <iostream>

CIRCUITOUS_RELAX_WARNINGS
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
CIRCUITOUS_UNRELAX_WARNINGS


#include <remill/Arch/Arch.h>
#include <remill/Arch/Name.h>
#include <remill/BC/Compat/Error.h>
#include <remill/BC/Util.h>
#include <remill/OS/OS.h>
namespace circ {
namespace {

auto call_args(llvm::CallInst *call)
{
    std::vector<llvm::Value *> out;
    for (uint32_t i = 0; i < call->arg_size(); ++i) {
        // NOTE(lukas): Check if we do not include the called fn by accident.
        check(!llvm::isa<llvm::Function>(call->getArgOperand(i)));
        out.push_back(call->getArgOperand(i));
    }
    return out;
}

// Keeps track of instruction dependencies.
template < typename T >
struct BottomUpDependencyVisitor
{
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
template < typename T >
void BottomUpDependencyVisitor<T>::Visit(llvm::Function *context, llvm::Value *val)
{
    auto self = static_cast<T *>(this);

    // Bottom out at an argument; it should be an input register.
    if (auto arg_val = llvm::dyn_cast<llvm::Argument>(val))
        return self->VisitArgument(context, arg_val);

    // Instruction; follow the dependency chain.
    if (auto inst_val = llvm::dyn_cast<llvm::Instruction>(val))
    {
        if (auto call_val = llvm::dyn_cast<llvm::CallInst>(inst_val))
        {
            for (auto it = call_val->arg_begin(); it < call_val->arg_end(); ++it)
                self->Visit(context, *it);
            return self->VisitFunctionCall(context, call_val);

        }

        for (auto &op_use : inst_val->operands())
            self->Visit(context, op_use);

        if (llvm::isa<llvm::BinaryOperator>(inst_val) ||
            llvm::isa<llvm::CmpInst>(inst_val))
        {
            return self->VisitBinaryOperator(context, inst_val);
        }

        if (llvm::isa<llvm::UnaryInstruction>(inst_val))
            return self->VisitUnaryOperator(context, inst_val);
        if (llvm::isa<llvm::SelectInst>(inst_val))
            return self->VisitSelect(context, inst_val);
        if (auto freeze = llvm::dyn_cast<llvm::FreezeInst>(inst_val))
            return self->VisitFreeze(context, freeze);

        unreachable() << "Unexpected value during visit: "
                      << remill::LLVMThingToString(inst_val);
    }

    // Bottom out at a constant, ignore for now.
    if (auto const_val = llvm::dyn_cast<llvm::Constant>(val))
    {
        if (auto undef = llvm::dyn_cast<llvm::UndefValue>(const_val))
            return self->VisitUndefined(context, undef);

        if (auto ce = llvm::dyn_cast<llvm::ConstantExpr>(const_val))
        {
            auto ce_inst = ce->getAsInstruction();
            auto &entry_block = context->getEntryBlock();
            ce_inst->insertBefore(&*entry_block.getFirstInsertionPt());
            ce->replaceAllUsesWith(ce_inst);
            CHECK(val == ce_inst);
            return self->Visit(context, val);  // Revisit.
        }
        if (auto ci = llvm::dyn_cast<llvm::ConstantInt>(val))
            return self->VisitConstantInt(context, ci);
        if (auto cf = llvm::dyn_cast<llvm::ConstantFP>(val))
            return self->VisitConstantFP(context, cf);

        unreachable()
            << "Unexpected constant encountered during dependency visitor: "
            << remill::LLVMThingToString(val);
    }
    unreachable() << "Unexpected value encountered during dependency visitor: "
                  << remill::LLVMThingToString(val);
}

struct IRImporter : public BottomUpDependencyVisitor< IRImporter >
{
    explicit IRImporter(const remill::Arch *arch_, const llvm::DataLayout &dl_,
                      Circuit *impl_)
        : arch(arch_), dl(dl_), impl(impl_)
    {}

    void VisitArgument(llvm::Function *, llvm::Argument *val)
    {
        check(val_to_op.count(val))
            << remill::LLVMThingToString(val) << " not present IRImporter.";
    }

    void VisitFreeze(llvm::Function *fn, llvm::FreezeInst *freeze)
    {
        auto arg = freeze->getOperand(0u);
        Visit(fn, arg);
        val_to_op[freeze] = val_to_op[arg];
    }

    static unsigned SizeFromSuffix(llvm::StringRef name)
    {
        if (name.endswith("_8"))    return 8u;
        if (name.endswith("_16"))   return 16u;
        if (name.endswith("_32"))   return 32u;
        if (name.endswith("_64"))   return 64u;
        if (name.endswith("_f32"))  return 32u;
        if (name.endswith("_f64"))  return 64u;
        if (name.endswith("_f80"))  return 80u;
        if (name.endswith("_f128")) return 128u;

        unreachable() << "Unsupported memory read intrinsic: " << name.str();
    }

    auto is_supported(auto triple)
    {
        auto arch_name = remill::GetArchName(triple);
        return arch_name == remill::kArchAMD64 || arch_name == remill::kArchX86;
    }

    auto get_triple(llvm::Function *fn)
    {
        return llvm::Triple(fn->getParent()->getTargetTriple());
    }

    auto inst_bits_node()
    {
        CHECK(impl->Attr<InputInstructionBits>().Size() == 1);
        return *impl->Attr<InputInstructionBits>().begin();
    }

    Operation *extract_argument(llvm::CallInst *call, llvm::Function *fn)
    {
        auto args = call_args(call);
        CHECK(args.size() <= 1);
        return (args.size() == 0) ? inst_bits_node() : Fetch(fn, args[0]);
    }

    Operation *VisitExtractIntrinsic(llvm::CallInst *call, llvm::Function *fn)
    {
        // TODO(lukas): Refactor into separate method and check in a better way
        //              that includes `_avx` variants.
        check(is_supported(get_triple(fn)));

        auto [extract_from, size] = irops::Extract::parse_args< uint32_t >(fn);
        auto arg = extract_argument(call, fn);

        // We split extract to sepratate bytes. This is so we can reorder them,
        // which can be handy if the extracted data are in a different order
        // (endiannity for example).
        const unsigned step = 8;
        std::deque< Operation * > partials;
        auto generate_fragments = [&](uint32_t from, uint32_t to) {
            std::deque< Operation * > partials;
            while (true) {
                uint32_t y = std::min(from + (step - from % step), to);
                auto op = impl->Create< Extract >(from, y);
                op->AddUse(arg);
                partials.push_front(op);
                if (y == to) {
                    return partials;
                }
                from = y;
            }
        };
        partials = generate_fragments(extract_from, extract_from + size);

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

    Operation *VisitExtractRawIntrinsic(llvm::CallInst *call, llvm::Function *fn)
    {
        // TODO(lukas): Refactor into separate method and check in a better way
        //              that includes `_avx` variants.
        check(is_supported(get_triple(fn)));

        auto arg = extract_argument(call, fn);
        auto [from, size] = irops::ExtractRaw::parse_args< uint32_t >(fn);
        auto op = Emplace< Extract >(call, from, from + size);

        auto args = call_args(call);
        if (!args.empty()) {
            op->AddUse(Fetch(call->getParent()->getParent(), args[0]));
        } else {
            op->AddUse(arg);
        }
        return op;
    }

    template<typename O, typename ... Args>
    Operation *VisitGenericIntrinsic(llvm::CallInst *call, llvm::Function *fn,
                                     Args &&... args)
    {
        auto out = Emplace< O >(call, std::forward<Args>(args)...);
        for (auto arg : call_args(call))
            out->AddUse(Fetch(call->getParent()->getParent(), arg));
        return out;
    }

    template< typename IT, typename OT >
    auto VisitIOLeaf(llvm::CallInst *call, llvm::Function *fn,
                     uint32_t io_type, uint32_t size)
    {
        auto leaf = [&]() {
            if (io_type == irops::io_type::in)
                return fetch_leave< IT >(fn, size);
            if (io_type == irops::io_type::out)
                return fetch_leave< OT >(fn, size);
            unreachable() << "Unreachable";
        }();
        val_to_op[call] = leaf;
        return leaf;
    }

    auto value_size(llvm::Value *val)
    {
        return static_cast<uint32_t>(dl.getTypeSizeInBits(val->getType()));
    }

    Operation *call_arg(llvm::CallInst *call, uint32_t idx)
    {
        auto op = call->getArgOperand(idx);
        return Fetch(call->getParent()->getParent(), op);
    }

    Operation *VisitLLVMIntrinsic(llvm::CallInst *call, llvm::Function *fn)
    {
        switch (fn->getIntrinsicID()) {
            case llvm::Intrinsic::ctpop :
                return VisitGenericIntrinsic< PopulationCount >(call, fn, value_size(call));
            case llvm::Intrinsic::ctlz :
            {
                auto out = Emplace< CountLeadingZeroes >(call, value_size(call));
                out->AddUse(call_arg(call, 0u));
                return out;
            }
            case llvm::Intrinsic::cttz :
            {
                auto out = Emplace< CountTrailingZeroes >(call, value_size(call));
                out->AddUse(call_arg(call, 0u));
                return out;
            }
            default:
              unreachable() << "Unsupported intrinsic call: "
                            << remill::LLVMThingToString(call);
        }
    }

    Operation *VisitIntrinsic(llvm::CallInst *call, llvm::Function *fn)
    {
        auto name = fn->getName();
        check(!name.startswith("__remill_read_memory_"))
            << "__remill_read_memory_* should not be present!";
        check(!name.startswith("__remill_write_memory_"))
            << "__remill_write_memory_* should not be present!";

        if (name.startswith("__remill_undefined_")) {
            return Emplace< Undefined >(call, SizeFromSuffix(name));
        }
        if (irops::Extract::is(fn)) {
            return VisitExtractIntrinsic(call, fn);
        }
        if (irops::ExtractRaw::is(fn)) {
            return VisitExtractRawIntrinsic(call, fn);
        }
        if (irops::InputImmediate::is(fn)) {
            auto [size] = irops::InputImmediate::parse_args<uint32_t>(fn);
            return VisitGenericIntrinsic< InputImmediate >(call, fn, size);
        }
        if (irops::Xor::is(fn)) {
            return VisitGenericIntrinsic< OnlyOneCondition >(call, fn);
        }
        if (irops::Concat::is(fn)) {
            auto [size] = irops::Concat::parse_args< uint32_t >(fn);
            return VisitGenericIntrinsic< Concat >(call, fn, size);
        }
        if (irops::Select::is(fn)) {
            auto [select_bits, size] = irops::Select::parse_args< uint32_t >(fn);
            return VisitGenericIntrinsic< Select >(call, fn, select_bits, size);
        }
        if (irops::OutputCheck::is(fn)) {
            return VisitGenericIntrinsic< RegConstraint >(call, fn);
        }
        if (irops::DecodeCondition::is(fn)) {
            return VisitGenericIntrinsic< DecodeCondition >(call, fn);
        }
        if (irops::VerifyInst::is(fn)) {
            return VisitGenericIntrinsic< VerifyInstruction >(call, fn);
        }
        if (irops::Advice::is(fn)) {
            auto [size] = irops::Advice::parse_args< uint32_t >(fn);
            return VisitGenericIntrinsic< Advice >(call, fn, size, ++advice_idx);
        }
        if ( irops::Operand::is(fn)) {
            auto [_, size] = irops::Operand::parse_args< uint32_t >(fn);
            return VisitGenericIntrinsic< Advice >(call, fn, size, ++advice_idx);
        }

        if ( irops::RegSelector::is(fn)) {
            auto [_, size] = irops::RegSelector::parse_args< uint32_t >(fn);
            return VisitGenericIntrinsic< Advice >(call, fn, size, ++advice_idx);
        }

        if (irops::AdviceConstraint::is(fn)) {
            return VisitGenericIntrinsic< AdviceConstraint >(call, fn);
        }
        if (irops::Or::is(fn)) {
            return VisitGenericIntrinsic< Or >(call, fn);
        }
        if (irops::DecoderResult::is(fn)) {
            return VisitGenericIntrinsic< DecoderResult >(call, fn);
        }
        if (irops::Memory::is(fn)) {
            auto [_, id] = irops::Memory::parse_args< uint32_t >(fn);
            return VisitGenericIntrinsic< Memory >(call, fn,
                    irops::memory::size(impl->ptr_size), id);
        }
        if (irops::And::is(fn)) {
            return VisitGenericIntrinsic< And >(call, fn);
        }
        if (irops::ReadConstraint::is(fn)) {
            return VisitGenericIntrinsic< ReadConstraint >(call, fn);
        }
        if (irops::WriteConstraint::is(fn)) {
            return VisitGenericIntrinsic< WriteConstraint >(call, fn);
        }
        if (irops::UnusedConstraint::is(fn)) {
            return VisitGenericIntrinsic< UnusedConstraint >(call, fn);
        }
        if (irops::ErrorBit::is(fn)) {
            auto [size, io_type] = irops::ErrorBit::parse_args< uint32_t >(fn);
            return VisitIOLeaf< InputErrorFlag, OutputErrorFlag >(call, fn, io_type, size);
        }
        if (irops::Timestamp::is(fn)) {
            auto [size, io_type] = irops::Timestamp::parse_args< uint32_t >(fn);
            return VisitIOLeaf< InputTimestamp, OutputTimestamp >(call, fn, io_type, size);
        }
        unreachable() << "Unsupported function: " << remill::LLVMThingToString(call);
    }

    // This function is responsible for binding some node to `val` inside `val_to_op`.
    void VisitFunctionCall(llvm::Function *, llvm::CallInst *val)
    {
        if (val_to_op.count(val))
            return;

        auto func = val->getCalledFunction();
        CHECK(func) << "Cannot find called function used in call: "
                    << remill::LLVMThingToString(val);

        Operation *op = nullptr;
        if (func->getIntrinsicID() != llvm::Intrinsic::not_intrinsic) {
            op = VisitLLVMIntrinsic(val, func);
        } else {
            op = VisitIntrinsic(val, func);
        }
        check(op && val_to_op[val] == op);
    }

    // TODO(lukas): Hack since there is no `urem` node. Adding it is a desirable
    //              fix.
    // Simulate urem as
    // `a % b = a - (udiv(a, b) * b)`
    Operation *lower_urem(llvm::Instruction *inst)
    {
        auto a = Fetch(inst->getParent()->getParent(), inst->getOperand(0u));
        auto b = Fetch(inst->getParent()->getParent(), inst->getOperand(1u));
        auto size = value_size(inst);

        // div = a / b
        auto div = impl->Create< UDiv >(size);
        div->AddUse(a);
        div->AddUse(b);

        // mul = div * b
        auto mul = impl->Create< Mul >(size);
        mul->AddUse(div);
        mul->AddUse(b);

        // sub = a - mul
        auto sub = impl->Create< Sub >(size);
        sub->AddUse(a);
        sub->AddUse(mul);

        auto [it, _] = val_to_op.emplace(inst, sub);
        populate_meta(inst, it->second);
        annote_with_llvm_inst(it->second, inst);
        return it->second;
    }

    Operation *HandleLLVMOP(llvm::Function *func, llvm::Instruction *inst)
    {
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
                default: unreachable() << "Cannot lower llvm predicate " << cmp->getPredicate();
            }
        };

        auto op_code = inst->getOpcode();

        auto handle_op = [&]() {
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

                case llvm::BinaryOperator::Trunc: return Emplace<Trunc>(inst, size);
                case llvm::BinaryOperator::ZExt: return Emplace<ZExt>(inst, size);
                case llvm::BinaryOperator::SExt: return Emplace<SExt>(inst, size);
                case llvm::BinaryOperator::ICmp: return handle_predicate(inst);
                case llvm::BinaryOperator::URem: return Emplace<URem>(inst, size);
                case llvm::BinaryOperator::SRem: return Emplace<SRem>(inst, size);

                default :
                    unreachable() << "Cannot lower llvm inst: "
                                  << llvm::Instruction::getOpcodeName(op_code);
            }
        };

        auto op = handle_op();
        for (const auto &op_ : inst->operand_values())
            op->AddUse(Fetch(func, op_));

        return op;
    }

    void VisitSelect(llvm::Function *func, llvm::Instruction *val)
    {
        if (val_to_op.count(val))
            return;

        auto sel = llvm::dyn_cast<llvm::SelectInst>(val);
        if (Fetch(func, sel->getCondition())->op_code == Undefined::kind) {
            Emplace< Undefined >(sel, value_size(sel));
            return;
        }

        auto true_val = Fetch(func, sel->getTrueValue());
        auto false_val = Fetch(func, sel->getFalseValue());

        check(true_val->op_code != Undefined::kind || false_val->op_code != Undefined::kind);

        HandleLLVMOP(func, val);
    }

    bool has_undefined_ops(llvm::Function *func, llvm::Instruction *inst)
    {
        for (const auto &op : inst->operand_values())
            if (Fetch(func, op)->op_code == Undefined::kind)
                return true;
        return false;
    }

    void VisitLLVMOperator(llvm::Function *func, llvm::Instruction *val)
    {
        if (val_to_op.count(val))
            return;

        if (has_undefined_ops(func, val)) {
            Emplace< Undefined >(val, value_size(val));
            return;
        }

        HandleLLVMOP(func, val);
    }

    void VisitBinaryOperator(llvm::Function *func, llvm::Instruction *val)
    {
        VisitLLVMOperator(func, val);
    }

    void VisitUnaryOperator(llvm::Function *func, llvm::Instruction *val)
    {
        VisitLLVMOperator(func, val);
    }

    void VisitAPInt(llvm::Constant *val, llvm::APInt ap_val)
    {
        if (val_to_op.count(val))
            return;

        auto num_bits = value_size(val);
        llvm::SmallString<128> val_bits;

        val_bits.reserve(num_bits);
        ap_val.toStringUnsigned(bits, 2);
        while (val_bits.size() < num_bits)
        {
            val_bits.insert(val_bits.begin(), '0');
        }
        std::reverse(val_bits.begin(), val_bits.end());
        auto bits_str = val_bits.str().str();

        auto &bits_op = bits_to_constants[bits_str];
        if (!bits_op)
        {
            CHECK(num_bits == bits_str.size());
            bits_op = impl->Create<Constant>(std::move(bits_str),
                                             static_cast<unsigned>(num_bits));
            annote_with_llvm_inst(bits_op, val);
        }
        val_to_op[val] = bits_op;
    }

    void VisitUndefined(llvm::Function *, llvm::UndefValue *val)
    {
        if (val_to_op.count(val))
            return;

        auto num_bits = static_cast<uint32_t>(dl.getTypeSizeInBits(val->getType()));
        Emplace< Undefined >(val, num_bits);
    }

    void VisitConstantInt(llvm::Function *, llvm::ConstantInt *val)
    {
        VisitAPInt(val, val->getValue());
    }
    void VisitConstantFP(llvm::Function *, llvm::ConstantFP *val)
    {
       VisitAPInt(val, val->getValueAPF().bitcastToAPInt());
    }

    void conjure_instbits(uint32_t size)
    {
       inst_bits[size] = impl->Create< InputInstructionBits >(size);
    }

    template< typename I >
    Operation *fetch_leave(llvm::Function *fn, uint32_t size)
    {
        if (!leaves.count(fn)) {
            // TODO(lukas): What about possible metadata?
            leaves[fn] = impl->Create< I >(size);
        }
        return leaves[fn];
    }

    Operation *Fetch(llvm::Function *fn, llvm::Value *val)
    {
        if (!val_to_op.count(val))
            Visit(fn, val);
        return val_to_op[val];
    }

    void annote_with_llvm_inst(Operation *op, llvm::Value *val)
    {
        std::stringstream ss;
        ss << "[ " << op->id() << " ]: " << dbg_dump(val);
        op->set_meta(circir_llvm_meta::llvm_source_dump, ss.str());
    }

    template<typename T, typename ...Args>
    Operation* Emplace(llvm::Value *key, Args &&... args)
    {
        auto [it, _] = val_to_op.emplace(key, impl->Create<T>(std::forward<Args>(args)...));
        populate_meta(key, it->second);
        annote_with_llvm_inst(it->second, key);
        return it->second;
    }

    template<bool allow_failure=false>
    Operation *get(llvm::Value *key) const
    {
        auto it = val_to_op.find(key);
        if constexpr (!allow_failure) {
            check(it != val_to_op.end() && it->second);
        }
        return (it != val_to_op.end()) ? it->second : nullptr;
    }

    bool contains(llvm::Value *key) const { return val_to_op.count(key); }

    const remill::Arch *arch;
    const llvm::DataLayout &dl;
    Circuit *impl;
    VerifyInstruction *verifier{nullptr};

    uint32_t advice_idx = 0;

    llvm::SmallString<128> bits;
    std::unordered_map<llvm::Value *, Operation *> val_to_op;
    std::unordered_map<llvm::Value *, Operation *> leaves;
    std::unordered_map<uint32_t, Operation *> inst_bits;

    std::unordered_map<std::string, Constant *> bits_to_constants;

  private:
    IRImporter() = delete;
};

void clear_names(llvm::Function *fn)
{
    for (auto &bb : *fn)
        for (auto &inst : bb)
            inst.setName("");
}

}  // namespace


Circuit::circuit_ptr_t lower_fn(llvm::Function *circuit_func, Ctx &ctx)
{
    const auto &arch = ctx.arch();
    clear_names(circuit_func);

    const auto module = circuit_func->getParent();
    const auto &dl = module->getDataLayout();

    log_info() << "IRImpoter starting.";
    auto impl = std::make_unique<Circuit>(ctx.ptr_size);
    IRImporter importer(arch, dl, impl.get());

    //TODO(lukas): Since extract does need operand, we need to have the node already present
    importer.conjure_instbits(kMaxNumInstBits);

    auto num_input_regs = 0u;
    auto num_output_regs = 0u;
    for (auto &arg : circuit_func->args()) {
        auto arg_size = static_cast<unsigned>(dl.getTypeSizeInBits(arg.getType()));
        // Expected output register.
        if (auto out_name = circuit_builder::is_output_reg(&arg)) {
            importer.Emplace<OutputRegister>(&arg, *out_name, arg_size);
            ++num_output_regs;
            // Input register.
        } else if (auto in_name = circuit_builder::is_input_reg(&arg)) {
            importer.Emplace<InputRegister>(&arg, *in_name, arg_size);
            ++num_input_regs;
        }
    }

    CHECK(0u < num_input_regs);
    CHECK(num_input_regs == num_output_regs);

    auto all_verifications = impl->Create<OnlyOneCondition>();
    impl->AddUse(all_verifications);

    auto visit_context = [&](llvm::CallInst *verify_inst) {
        importer.Visit(circuit_func, verify_inst);
        CHECK(importer.get(verify_inst)->op_code == VerifyInstruction::kind);
        all_verifications->AddUse(importer.get(verify_inst));
    };
    irops::VerifyInst::for_all_in(circuit_func, visit_context);
    log_info() << "IRImpoter done.";

    VerifyCircuit("Lowered llvm circuit.", impl.get(), "Lowered circuit is valid.");
    return impl;
}

}  // namespace circ
