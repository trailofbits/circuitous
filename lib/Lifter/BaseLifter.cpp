/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/BaseLifter.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <remill/BC/Util.h>
#include <remill/BC/Optimizer.h>

#include <circuitous/Lifter/Flatten.hpp>
#include <circuitous/Fuzz/InstructionFuzzer.hpp>

#include <circuitous/Util/Warnings.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/Transforms/IPO.h>

#include <llvm/Support/raw_os_ostream.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ {

    struct InstrinsicHandler {
        using functions_t = std::vector< llvm::Function * >;
        using calls_t = std::vector< llvm::CallInst * >;
        using intrinsic_id = llvm::Intrinsic::ID;
        using intrinsic_map = std::map< intrinsic_id, calls_t >;

        intrinsic_map llvm_intrinsics;
        calls_t remill_compares;
        calls_t remill_flag_computations;

        bool can_llvm_intrinsic(llvm::CallInst *cs)
        {
            auto id = cs->getCalledFunction()->getIntrinsicID();
            return id == llvm::Intrinsic::usub_sat ||
                   id == llvm::Intrinsic::fshr ||
                   id == llvm::Intrinsic::fshl;
        }

        void catalogue_llvm_intrinsic(llvm::CallInst *cs)
        {
            auto id = cs->getCalledFunction()->getIntrinsicID();
            llvm_intrinsics[id].push_back(cs);
        }

        bool try_catalogue_remill_intrinsic(llvm::CallInst *cs)
        {
            auto callee = cs->getCalledFunction();
            if (!callee->hasName())
                return false;

            auto try_catalogue = [&](auto prefix, auto &storage) {
                if (!callee->getName().startswith(prefix))
                    return false;
                storage.push_back(cs);
                return true;
            };

            return    try_catalogue("__remill_compare_", remill_compares)
                   || try_catalogue("__remill_flag_computation_", remill_flag_computations);
        }

        void categorize_callsite(llvm::CallInst *cs)
        {
            if (can_llvm_intrinsic(cs))
                return catalogue_llvm_intrinsic(cs);

            try_catalogue_remill_intrinsic(cs);
        }

        void fetch(const functions_t &fns)
        {
            for (auto fn : fns)
                for (auto &bb : *fn)
                    for (auto &inst : bb)
                        // NOTE(lukas): I opted to go inst by inst to avoid accidentaly
                        //              modifying semantic functions we do not use.
                        if ( auto call = llvm::dyn_cast< llvm::CallInst >( &inst ) )
                            categorize_callsite( call );
        }

        // Try to use builtin llvm lowering.
        void lower(const std::vector<llvm::CallInst *> &calls) {
            if (calls.empty()) {
                return;
            }
            auto dl = calls[0]->getModule()->getDataLayout();
            llvm::IntrinsicLowering lower{dl};
            for (auto c : calls) {
                lower.LowerIntrinsicCall(c);
            }
        }

        void lower(const functions_t &fns) {
            fetch(fns);

            handle_usub_sat(llvm_intrinsics[llvm::Intrinsic::usub_sat]);

            handle_FSH_(llvm_intrinsics[llvm::Intrinsic::fshr], [](auto &ir, auto a, auto b) {
                return ir.CreateLShr(a, b);
            }, [](auto &ir, auto from, auto size) {
                // Least significant bytes
                return irops::make< irops::ExtractRaw >(ir, from, 0ul,
                                                        static_cast< std::size_t >(size));
            });

            handle_FSH_(llvm_intrinsics[llvm::Intrinsic::fshl], [](auto &ir, auto a, auto b) {
                return ir.CreateShl(a, b);
            }, [](auto &ir, auto from, auto size){
                // Most significant bytes
                auto coerced = static_cast< std::size_t >(size);
                return irops::make< irops::ExtractRaw >(ir, from, coerced, coerced);
            });

            handle_remill_compare(remill_compares);
            handle_remill_flag_computation(remill_flag_computations);
        }

        // Just wrappers that keep extra information in the bitcode, cirucitous
        // does not care about it.
        void handle_remill_compare(const calls_t &calls)
        {
            for (auto call : calls)
                call->replaceAllUsesWith(call->getArgOperand(0));
        }

        // Just wrappers that keep extra information in the bitcode, cirucitous
        // does not care about it.
        void handle_remill_flag_computation(const calls_t &calls)
        {
            for (auto call : calls)
                call->replaceAllUsesWith(call->getArgOperand(0));

        }

        llvm::Value *handle_FSH_(llvm::CallInst *call, auto make_shift, auto make_extract)
        {
            llvm::IRBuilder<> ir(call);
            auto &dl = call->getModule()->getDataLayout();

            std::vector< llvm::Value * > args {
                call->getArgOperand(1u), call->getArgOperand(0u)
            };

            // iN out = fshr(iN x, iN y, iZ z)
            // i(N * 2) x'y' = concat(x, y)
            auto full = irops::make< irops::Concat >(ir, args);

            auto size = static_cast<uint32_t>(
                    dl.getTypeSizeInBits(call->getArgOperand(0u)->getType()));
            auto shift_size = static_cast<uint32_t>(
                    dl.getTypeSizeInBits(call->getArgOperand(2u)->getType()));
            // z' = z % N
            auto shift_c = ir.CreateURem(call->getArgOperand(2u), ir.getIntN(shift_size, size));
            // shifted' = x'y' >> z'
            auto shifted = make_shift(ir, full, ir.CreateZExt(shift_c, full->getType()));
            // out' = extract.N.N*2(shifted')
            return make_extract(ir, shifted, size);
        }

        void handle_FSH_(const std::vector<llvm::CallInst *> &calls,
                        auto make_shift, auto make_extract)
        {
            for (auto call : calls) {
                auto nc = handle_FSH_(call, make_shift, make_extract);
                call->replaceAllUsesWith(nc);
                call->eraseFromParent();
            }
        }

        void handle_usub_sat(const std::vector<llvm::CallInst *> &calls)
        {
            for (auto inst : calls) {
                llvm::IRBuilder<> ir(inst);
                auto call = llvm::cast<llvm::CallInst>(inst);
                auto a = call->getOperand(0);
                auto b = call->getOperand(1);
                auto size = static_cast<uint32_t>(inst->getType()->getPrimitiveSizeInBits());
                // sub = a - b
                auto sub = ir.CreateSub(a, b);
                // select  = (a < b) ? 0 : a - b;
                auto flag = ir.CreateICmpULT(a, b);
                auto zero = ir.getIntN(size, 0);
                auto select = ir.CreateSelect(flag, zero, sub);
                // replace & erase
                call->replaceAllUsesWith(select);
                call->eraseFromParent();
            }
        }
    };

    void optimize_silently(llvm::Module *module,
                           const std::vector<llvm::Function *> &fns)
    {
        llvm::legacy::FunctionPassManager func_manager(module);

        llvm::PassManagerBuilder builder;
        builder.OptLevel = 3;
        builder.SizeLevel = 0;
        builder.Inliner = llvm::createFunctionInliningPass(250);
        builder.DisableUnrollLoops = false;
        builder.SLPVectorize = false;
        builder.LoopVectorize = false;

        builder.VerifyOutput = true;
        builder.VerifyInput = false;

        builder.MergeFunctions = false;

        builder.populateFunctionPassManager(func_manager);
        func_manager.doInitialization();
        for (auto fn : fns)
            func_manager.run(*fn);

        func_manager.doFinalization();

        InstrinsicHandler().lower(fns);
    }

    void optimize_silently( const std::vector< llvm::Function * > &fns )
    {
        if ( fns.empty() )
            return;
        auto m = ( *fns.begin() )->getParent();
        return optimize_silently( m, fns );

    }

    // Flatten all control flow into pure data-flow inside of a function.
    void flatten_cfg(llvm::Function *func, const remill::IntrinsicTable &intrinsics)
    {
        Flattener(func, intrinsics.error).Run();
    }

    void flatten_cfg( llvm::Function &fn )
    {
        remill::IntrinsicTable table( fn.getParent() );
        return flatten_cfg( &fn, table );
    }

    void post_lift( llvm::Function &fn )
    {
        disable_opts( &fn );
        verify_or_die( fn );

        optimize_silently( { &fn } );

        flatten_cfg( fn );

        check_unsupported_intrinsics( { &fn } );
        optimize_silently( { &fn } );

        enable_opts( &fn );
    }

} // namespace circ
