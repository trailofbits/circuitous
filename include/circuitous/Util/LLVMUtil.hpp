/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

// TODO(lukas): Fill llvm headers
#include <fstream>
#include <optional>
#include <string>

#include <remill/BC/Version.h>
#include <remill/BC/Util.h>
#include <remill/Arch/Arch.h>

#include <circuitous/Util/Logging.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/Transforms/Utils/Cloning.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

namespace circ
{

    static inline void disable_opts(llvm::Function *func)
    {
        func->removeFnAttr(llvm::Attribute::InlineHint);
        func->removeFnAttr(llvm::Attribute::AlwaysInline);
        func->setLinkage(llvm::GlobalValue::ExternalLinkage);
        func->addFnAttr(llvm::Attribute::NoInline);
    }

    static inline void disable_opts(const std::vector< llvm::Function * > &fns)
    {
        for (auto fn : fns)
            disable_opts(fn);
    }

    static inline void enable_opts(llvm::Function *func)
    {
        func->removeFnAttr(llvm::Attribute::InlineHint);
        func->removeFnAttr(llvm::Attribute::AlwaysInline);
        func->setLinkage(llvm::GlobalValue::ExternalLinkage);
        func->addFnAttr(llvm::Attribute::NoInline);
    }

    static inline void enable_opts(const std::vector< llvm::Function * > &fns)
    {
        for (auto fn : fns)
            enable_opts(fn);
    }

    static inline void EraseFn(llvm::Module *module, const std::string &fn_name)
    {
        if (auto fn = module->getFunction(fn_name))
        {
            fn->replaceAllUsesWith(llvm::UndefValue::get(fn->getType()));
            fn->eraseFromParent();
        }
    }

    static inline void EraseFns(llvm::Module *module, const std::vector< std::string > &fns)
    {
        for (auto &fn : fns)
            EraseFn(module, fn);
    }

    template< typename LLVM > requires std::is_base_of_v< llvm::Value, LLVM >
    static inline void safe_erase_from_parent(LLVM *what)
    {
        what->replaceAllUsesWith(llvm::UndefValue::get(what->getType()));
        what->eraseFromParent();
    }

    template< typename LLVM > requires std::is_base_of_v< llvm::Value, LLVM >
    static inline void safe_erase_from_parent(std::vector< LLVM * > &&ts)
    {
        for (auto &t : ts)
            safe_erase_from_parent(t);

    }
    static inline std::string LLVMName(llvm::Value *val,
                                       const std::string &def = "(name not set)")
    {
        if (!val->hasName())
            return def;
        return val->getName().str();
    }

    template< typename T >
    void AddMetadata(llvm::Instruction *inst, const std::string &kind, T value)
    {
        check(inst);
        check(!inst->getMetadata(kind));
        auto &ctx = inst->getContext();
        llvm::IRBuilder<> ir(ctx);

        auto format = [](const auto &val_) {
            if constexpr (std::is_integral_v< T >) {
                return std::to_string(val_);
            } else {
                return val_;
            }
        };

        auto node = llvm::MDNode::get(ctx, {llvm::MDString::get(ctx, format(value))});
        inst->setMetadata(kind, node);
    }

    // If you feel like you need to return different type just extend it.
    static inline std::optional< uint64_t > GetMetadata(llvm::Instruction *inst,
                                                        const std::string &kind)
    {
        auto node = inst->getMetadata(kind);
        if (!node)
            return {};
        check(node->getNumOperands() == 1);
        auto op = llvm::dyn_cast< llvm::MDString >(node->getOperand(0));
        return std::strtoull(op->getString().data(), nullptr, 10);
    }

    static inline void DumpFns(const std::string &filename,
                               const std::vector< llvm::Function * > &fns)
    {
        std::ofstream file(filename);
        llvm::raw_os_ostream out(file);
        for (auto fn : fns)
            fn->print(out);
        out.flush();
    }

    template< typename C >
    auto make_and(llvm::IRBuilder<> &ir, const C &vals)
    {
        llvm::Value *acc = ir.getTrue();
        for (auto val : vals)
            acc = ir.CreateAnd(acc, val);
        return acc;
    }

    template< typename C >
    auto make_or(llvm::IRBuilder<> &ir, const C &vals)
    {
        llvm::Value *acc = ir.getFalse();
        for (auto val : vals)
            acc = ir.CreateOr(acc, val);
        return acc;
    }

    static inline bool is_undef(llvm::Value *val)
    {
        return llvm::UndefValue::get(val->getType()) == val;
    }

    static inline auto extend(llvm::CallInst *call, std::vector< llvm::Value * > args)
    {
        llvm::IRBuilder<> ir(call);
        args.insert(args.begin(), call->data_operands_begin(), call->data_operands_end());
        auto callee = call->getCalledFunction();

        auto new_call = ir.CreateCall(callee, args);
        new_call->copyMetadata(*call);
        call->replaceAllUsesWith(new_call);
        call->eraseFromParent();
        return new_call;
    }

    static inline auto reg_from_gep(llvm::GetElementPtrInst *gep, const auto &arch)
    -> const remill::Register *
    {
        auto dl = llvm::DataLayout(gep->getModule());
        auto [_, off] = remill::StripAndAccumulateConstantOffsets(dl, gep);
        return arch->RegisterAtStateOffset(static_cast< uint64_t >(off));
    }

    static inline auto coerce_reg(llvm::LoadInst *load, const remill::Register *reg)
    -> const remill::Register *
    {
        if (!reg)
            return nullptr;
        auto dl = llvm::DataLayout(load->getModule());
        auto size = dl.getTypeSizeInBits (load->getType());
        return reg->EnclosingRegisterOfSize(size / 8);
    }

    static inline auto make_range(llvm::Instruction *from, llvm::Instruction *to)
    {
        using bb_t = llvm::BasicBlock::iterator;
        return llvm::iterator_range< bb_t >(bb_t{from}, bb_t{to});
    }

    template< typename F >
    static inline auto inline_flattened(llvm::CallInst *call, F &&f)
    {
        auto begin = f(llvm::IRBuilder<>(call));
        using bb_it = llvm::BasicBlock::iterator;
        auto end = f(llvm::IRBuilder<>(call->getParent(), std::next(bb_it{call})));

        auto block = call->getParent();
        if (std::prev(block->end())->isTerminator())
            log_error() << "Going to inline ill formed block (terminator missing).";

        check(call->getCalledFunction()->size() == 1);

        llvm::InlineFunctionInfo info;
    #if LLVM_VERSION_NUMBER < LLVM_VERSION(11, 0)
        llvm::InlineFunction(call, info);
    #else
        llvm::InlineFunction(*call, info);
    #endif

        check(begin->getParent() == end->getParent());
        return std::make_tuple(begin, end);
    }

    static inline auto inst_distance(llvm::Instruction *a, llvm::Instruction *b)
    {
        check(a->getParent() == b->getParent());
        using bb_it = llvm::BasicBlock::iterator;
        return std::distance(bb_it(a), bb_it(b));
    }

    static inline auto try_enclosing_reg(auto arch, const std::string &name)
    -> std::optional< const remill::Register * >
    {
        auto remill_reg = arch->RegisterByName(name);
        if (!remill_reg)
            return {};
        if (auto enclosed = remill_reg->EnclosingRegister())
            return { enclosed };
        return {};
    }

    static inline auto enclosing_reg(auto arch, const std::string &name)
    {
        auto remill_reg = arch->RegisterByName(name);
        check(remill_reg) << "Was not able to retrieve " << name;
        check(remill_reg->EnclosingRegister());
        return remill_reg->EnclosingRegister();
    }

    template< typename T > requires std::is_base_of_v< llvm::Value, T >
    std::string dbg_dump(const std::vector< T * > &vs)
    {
        std::stringstream os;
        os << "[" << std::endl;
        for (auto v : vs)
            os << remill::LLVMThingToString(v) << std::endl;
        os << "]";
        return os.str();
    }

    template< typename T > requires std::is_base_of_v< llvm::Value, T >
    std::string dbg_dump(const std::unordered_set< T * > &vs)
    {
        not_implemented();
    }

    static inline std::string dbg_dump(llvm::BasicBlock *block)
    {
        return remill::LLVMThingToString(block);
    }

    static inline std::string dbg_dump(llvm::Value *val)
    {
        return remill::LLVMThingToString(val);
    }

    static inline std::string dbg_dump(llvm::Type *ty)
    {
        return remill::LLVMThingToString(ty);
    }

    using verify_result_t = std::optional< std::string >;
    static inline verify_result_t verify_function(llvm::Function &fn)
    {
        std::string error;
        llvm::raw_string_ostream os(error);
        if (!llvm::verifyFunction(fn, &os))
            return {};
        os.flush();
        return { std::move(error) };
    }

    // `bits` is taken by copy since it is reversed inplace.
    static inline auto make_APInt(auto bits, std::size_t from, std::size_t size)
    {
        check(bits.size() >= from + size) << bits.size() << " >= " << from + size;

        std::string span;
        for (uint64_t i = 0; i < size; ++i)
            span += (bits[from + i]) ? '1' : '0';
        auto size_ = static_cast< uint32_t >(size);
        return llvm::APInt(size_, span, 2);
    }

} // namespace circ
