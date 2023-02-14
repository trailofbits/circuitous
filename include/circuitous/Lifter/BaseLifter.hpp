/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <cstdint>
#include <deque>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Lifter/Decoder.hpp>
#include <circuitous/Lifter/Instruction.hpp>
#include <circuitous/Util/LLVMUtil.hpp>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Util/Warnings.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/IntrinsicTable.h>
#include <remill/BC/Lifter.h>
#include <remill/BC/Optimizer.h>
#include <remill/BC/Util.h>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/Module.h>
#include <remill/BC/Annotate.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ
{

    void optimize_silently(llvm::Module *module,
                           const std::vector<llvm::Function *> &fns);

    void optimize_silently( const std::vector< llvm::Function * > &fns );

    static inline void optimize_silently( llvm::Module *lmodule )
    {
        std::vector< llvm::Function * > fns;
        for ( auto &fn : *lmodule )
            if ( !fn.isDeclaration() )
                fns.push_back( &fn );
        return optimize_silently( lmodule, fns );
    }

    // Flatten all control flow into pure data-flow inside of a function.
    // TODO(lukas): Write down what are guarantees w.r.t. to metadata.
    void flatten_cfg(llvm::Function *func, const remill::IntrinsicTable &intrinsics);
    void flatten_cfg( llvm::Function &fn );


    static inline auto report_unsupported_intrinsic_calls( llvm::Function &isem )
        -> std::optional< std::string >
    {
        auto fn_name = []( llvm::Function &f ) -> std::string
        {
            if ( f.hasName() )
                return "( not named )";
            return f.getName().str();
        };

        auto is_allowed = [ & ]( llvm::Function &callee )
        {
            // Indirect call is not an intrinsics and all functions should have names
            // at this point.
            check( callee.hasName() );

            static const std::unordered_set< std::string > allowed =
            {
                "__remill_atomic_begin",
                "__remill_atomic_end",
            };

            auto name = callee.getName();

            if ( allowed.count( name.str() ) )
                return true;

            // Cannot handle atomics right now
            if ( name.contains( "atomic" ) )
                return false;

            // Cannot handle any form of floats
            if ( name.contains( "float" ) )
                return false;

            if ( name.contains( "__remill_sync_hyper_call" ) )
                return false;

            // If something was missed, lifter will most likely crash and intrinsic can
            // be retroactively added here.
            return true;
        };

        // Go through all calls and collect those that call functions that are not allowed.
        std::stringstream errs;

        for ( auto &bb : isem )
            for ( auto &inst : bb )
                if ( auto call = llvm::dyn_cast< llvm::CallInst >( &inst ) )
                {
                    auto callee = call->getCalledFunction();
                    check( callee );
                    if ( !is_allowed( *callee ) )
                        errs << "\t" << fn_name( *callee ) << std::endl;
                }

        // Check if anything was logged.
        auto found_errs = errs.str();
        if ( found_errs.empty() )
            return {};

        // Prefix message with header
        std::stringstream report;
        report << isem.getName().str() << std::endl << std::move( found_errs );
        return report.str();
    }


    static inline void check_unsupported_intrinsics(
            const std::vector< llvm::Function * > &fns)
    {
        std::stringstream out;

        for ( auto fn : fns )
            if ( auto err = report_unsupported_intrinsic_calls( *fn ) )
                out << std::move( *err );

        auto reports = out.str();
        check( reports.empty() ) << "Found unsupported intrinsic calls, dumping report:\n"
                                 << reports;
    }


    void post_lift( llvm::Function &fn );

    struct ExaltedFunctionMeta
    {
        // Needs to be this way to be able to use API from `remill/BC/Annotate.h`
        static const inline std::string metadata_kind = "circ.exaltedfn";
        static const inline std::string metadata_value = "Function exalted by circuitous";
    };

    // Should remain stateless and cheap to construct.
    template< typename Impl >
    struct ILifter : has_ctx_ref
    {
        using has_ctx_ref::has_ctx_ref;

        // Optimize -> Flatten -> Optimize
        // Can return (but does not have to) new function.
        llvm::Function *post_process(llvm::Function * fn)
        {
            disable_opts(fn);

            auto has_error = verify_function(*fn);
            check(!has_error) << "Trying to post-process invalid function.\n" << *has_error;
            optimize_silently(ctx.module(), { fn });

            if (fn->size() == 1)
                return fn;
            disable_opts(fn);

            remill::IntrinsicTable intrinsics(ctx.module());
            flatten_cfg(fn, intrinsics);

            disable_opts(fn);
            check_unsupported_intrinsics({ fn });
            // TOOD(lukas): Inline into the loop once the `optimize_silently` does
            //              only function verification (now does module).
            optimize_silently(ctx.module(), { fn });

            // We're done; make the instruction functions more amenable for inlining
            // and elimination.
            enable_opts(fn);
            return fn;
        }

        static std::string lifted_name(const std::string &bytes)
        {
            std::stringstream ss;
            ss << "inst_";
            for (std::size_t i = 0; i < bytes.size(); ++i)
                ss << std::hex << static_cast<uint32_t>(bytes[i]);
            return ss.str();
        }

        std::optional< std::string > report_unsupported_intrinsic_calls(llvm::Function *fn)
        {
            auto fn_name = [](llvm::Function *fn) -> std::string {
                if (!fn || !fn->hasName())
                    return "( nullptr )";
                return fn->getName().str();
            };

            auto is_allowed = [&](llvm::Function *fn) {
                // Indirect call is not an intrinsics and all functions should have names
                // at this point.
                if (!fn || !fn->hasName())
                    return false;

                static const std::unordered_set< std::string > allowed = {
                  "__remill_atomic_begin",
                  "__remill_atomic_end",
                };

                auto fn_name = fn->getName();
                // TODO(lukas): Pull this from whatever class is responsible for lowering.
                //              May be tricky since it may be a user of this class.
                if (allowed.count(fn_name.str()))
                    return true;
                // Cannot handle atomics right now
                if (fn_name.contains("atomic"))
                    return false;
                // Cannot handle any form of floats
                if (fn_name.contains("float"))
                    return false;
                if (fn_name.contains("__remill_sync_hyper_call"))
                    return false;
                // If something was missed, lifter will most likely crash and intrinsic can
                // be retroactively added here.
                return true;
            };

            // Go through all calls and collect those that call functions that are not allowed.
            std::stringstream errs;
            for (auto &bb : *fn)
                for (auto &inst : bb)
                    if (auto call = llvm::dyn_cast< llvm::CallInst >(&inst))
                        if (!is_allowed(call->getCalledFunction()))
                            errs << "\t" << fn_name(call->getCalledFunction()) << std::endl;

            // Check if anything was logged.
            auto found_errs = errs.str();
            if (found_errs.empty())
                return {};

            // Prefix message with header
            std::stringstream report;
            report << fn->getName().str() << std::endl << std::move(found_errs);
            return report.str();
        }


        void check_unsupported_intrinsics(const std::vector< llvm::Function * > &fns)
        {
            std::stringstream out;

            for (auto fn : fns)
                if (auto err = report_unsupported_intrinsic_calls(fn))
                    out << std::move(*err);
            auto reports = out.str();
            check(reports.empty()) << "Found unsupported intrinsic calls, dumping report:\n"
                                   << reports;
        }

        bool was_lifted_correctly(auto status, const remill::Instruction &inst)
        {
            if (status == remill::LiftStatus::kLiftedInstruction)
                return true;

            if (status == remill::LiftStatus::kLiftedUnsupportedInstruction)
                log_error() << "Missing semantics for instruction:" << inst.Serialize();
            else if (status == remill::LiftStatus::kLiftedInvalidInstruction)
                log_error() << "Invalid instruction:" << inst.Serialize();
            else
                unreachable() << "Instruction lifter ended with unexpected error:"
                              << inst.Serialize();

            return false;
        }

        std::string lifted_name(const InstBytes &bytes, const std::string &suffix="")
        {
            return "lifted_inst_" + bytes.as_hex_str() + "." + suffix;
        }

        std::string craft_lifted_name(const std::string &raw_bytes, uint32_t repetitions = 0)
        {
            check(repetitions < 5) << "Already lifted these bytes 5 times!";
            auto name = lifted_name(InstBytes(raw_bytes), std::to_string(repetitions));
            if (!this->ctx.module()->getFunction(name))
                return name;
            return craft_lifted_name(raw_bytes, repetitions + 1);
        }

        // TODO(lukas): This should take `const InstructionInfo &` as argument.
        std::optional< llvm::Function * > lift(InstructionInfo &info)
        {
            check(info.has_shadow()) << "Cannot lift from InstructionInfo that has no shadow!";
            auto &rinst = info.rinst();
            auto name = craft_lifted_name(rinst.bytes);

            auto fn = ctx.arch()->DeclareLiftedFunction(name, ctx.module());
            ctx.arch()->InitializeEmptyLiftedFunction(fn);

            // We need lifetime of the table to outlast the life of lifter
            auto intrinsics = remill::IntrinsicTable(ctx.module());
            Impl lifter(ctx.arch(), ctx.module(), &intrinsics);

            lifter.SupplyShadow(&info.shadow());
            check(fn->size() == 1);

            auto block = &fn->getEntryBlock();
            auto status = lifter.LiftIntoBlock(rinst, block, false);

            if (!was_lifted_correctly(status, rinst))
                log_kill() << "Cannot recover from inccorectly lifted function.";

            // Check if unsupported intrinsics are present (some wild intrinsic can appear
            // when lifting new isels or llvm versions are changed).
            auto report = report_unsupported_intrinsic_calls(fn);
            // TODO(lukas): For now, this is hard error as there is no reasonable recovery,
            //              in future we can propage it up.
            check(!report) << "Unsupported intrinsics call, dumping report.\n" << *report;

            auto post_fn = post_process(fn);
            remill::Annotate< ExaltedFunctionMeta >( post_fn );
            return { post_fn };
        }
    };
} // namespace circ
