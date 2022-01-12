/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/BaseLifter.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <remill/BC/Compat/CallSite.h>
#include <remill/BC/Util.h>
#include <remill/BC/Optimizer.h>

#include <circuitous/Lifter/Flatten.hpp>
#include <circuitous/Fuzz/InstructionFuzzer.hpp>

#include <circuitous/Util/Logging.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ {

    struct InstrinsicHandler {
        using functions_t = std::vector< llvm::Function * >;
        using calls_t = std::vector< llvm::CallInst * >;
        using intrinsic_id = llvm::Intrinsic::ID;
        using intrinsic_map = std::map< intrinsic_id, calls_t >;

        using callsite_t = remill::compat::llvm::CallSite;

        intrinsic_map llvm_intrinsics;
        calls_t remill_compares;
        calls_t remill_flag_computations;

        bool can_llvm_intrinsic(callsite_t cs)
        {
            auto id = cs.getCalledFunction()->getIntrinsicID();
            return id == llvm::Intrinsic::usub_sat ||
                   id == llvm::Intrinsic::fshr ||
                   id == llvm::Intrinsic::fshl;
        }

        llvm::CallInst *get_call(callsite_t cs)
        {
            return llvm::dyn_cast< llvm::CallInst >(cs.getInstruction());
        }

        void catalogue_llvm_intrinsic(callsite_t cs)
        {
            auto id = cs.getCalledFunction()->getIntrinsicID();
            llvm_intrinsics[id].push_back(get_call(cs));
        }

        bool try_catalogue_remill_intrinsic(callsite_t cs)
        {
            auto callee = cs.getCalledFunction();
            if (!callee->hasName())
                return false;

            auto try_catalogue = [&](auto prefix, auto &storage) {
                if (!callee->getName().startswith(prefix))
                    return false;
                storage.push_back(get_call(cs));
                return true;
            };

            return    try_catalogue("__remill_compare_", remill_compares)
                   || try_catalogue("__remill_flag_computation_", remill_flag_computations);
        }

        void categorize_callsite(callsite_t cs)
        {
            if (!cs.isCall())
                return;
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
                    categorize_callsite(remill::compat::llvm::CallSite(&inst));
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
                return irops::make< irops::ExtractRaw >(ir, from, 0, size);
            });

            handle_FSH_(llvm_intrinsics[llvm::Intrinsic::fshl], [](auto &ir, auto a, auto b) {
                return ir.CreateShl(a, b);
            }, [](auto &ir, auto from, auto size){
                // Most significant bytes
                return irops::make< irops::ExtractRaw >(ir, from, size, size);
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

    void optimize_silently(const remill::Arch *arch, llvm::Module *module,
                           const std::vector<llvm::Function *> &fns)
    {
        // `remill::OptimizeModule` calls some transformation that pollute
        // output with some info, so we need to mute it.
        auto saved_threshold = module->getContext().getDiagnosticsHotnessThreshold();
        module->getContext().setDiagnosticsHotnessThreshold(1);
        remill::OptimizeModule(arch, module, fns);

        // Set the logging back to the original values.
        module->getContext().setDiagnosticsHotnessThreshold(saved_threshold);

        // Handle intrinsic which can be lowered && we do not have intrinsic for
        // them.
        InstrinsicHandler().lower(fns);

        // Verify we did not broke anything
        remill::VerifyModule(module);
    }


    // Flatten all control flow into pure data-flow inside of a function.
    void flatten_cfg(llvm::Function *func, const remill::IntrinsicTable &intrinsics)
    {
        Flattener(func, intrinsics.error).Run();
    }

    void fuzz_operands(const remill::Arch::ArchPtr &arch, InstSelections &insts)
    {
        for (auto &inst : insts)
            for (auto i = 0U; i < inst.instructions.size(); ++i)
                inst.shadows[i] = InstructionFuzzer{arch.get(), inst.instructions[i]}.FuzzOps();
    }

    bool IsDecodePositionIndependent(const remill::Arch::ArchPtr &arch,
                                   const remill::Instruction &inst)
    {
        remill::Instruction copy;
        if (!arch->DecodeInstruction(inst.pc + 32, inst.bytes, copy) ||
            inst.operands.size() != copy.operands.size())
        {
            return false;
        }

        for (auto i = 0u; i < inst.operands.size(); ++i)
            if (inst.operands[i].Serialize() != copy.operands[i].Serialize())
                return false;

        return true;
    }

} // namespace circ
