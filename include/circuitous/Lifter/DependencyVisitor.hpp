/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/CFG.h>
#pragma clang diagnostic pop

#include <sstream>

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Support/Check.hpp>

namespace circ
{

    template< typename I, typename F >
    auto order(const I &insts, F &&get_idx)
    {
        using val_t = typename I::value_type;

        std::array< val_t, 16 > ordered_selects;
        std::size_t last = 8;
        for (auto i = 0u; i < ordered_selects.size(); ++i) ordered_selects[i] = nullptr;

        for (auto x : insts)
        {
            if (auto idx = get_idx(x))
            {
                // TODO(lukas): For now it is assumed no intrinsic has more than 4 operands.
                check(*idx < 8);
                check(!ordered_selects[*idx]);
                ordered_selects[*idx] = x;
            } else {
                check(last < ordered_selects.size() - 1);
                ordered_selects[++last] = x;
            }
        }

        std::vector< val_t > vs;
        for (std::size_t i = 0; i < ordered_selects.size(); ++i)
            if (ordered_selects[i]) vs.push_back(ordered_selects[i]);
        return vs;
    }

    template< typename I, typename F >
    auto order_by_metadata(const I& insts, F &&kind)
    {
        auto get_idx = [&](auto value){ return GetMetadata(value, kind); };
        return order(insts, get_idx);
    }

    // TODO(lukas): Rename.
    // For each `Source` intrinsic collects all used `Sink` intrinsics.
    template< typename Source, typename Sink >
    struct ContextCollector
    {
        std::map< llvm::Value *, std::unordered_set< llvm::CallInst * > > data;
        llvm::Function *fn;


        ContextCollector(llvm::Function *fn_) : fn(fn_) {}

        void try_insert(llvm::CallInst *ctx, llvm::Instruction *current)
        {
            if (auto call = llvm::dyn_cast< llvm::CallInst >(current);
                call && Sink::is(call->getCalledFunction()))
            {
                data[ctx].insert(call);
            }
        }

        void run(llvm::CallInst *ctx, llvm::Instruction *current)
        {
            try_insert(ctx, current);
            for (auto op : current->operand_values())
                if (auto as_inst = llvm::dyn_cast< llvm::Instruction >(op))
                    run(ctx, as_inst);
        }

        auto get()
        {
            std::map< llvm::Value *, std::vector< llvm::CallInst * > > out;
            for (auto &[ctx, selects] : data)
                out[ctx] = order_by_metadata(selects, "__circuitous.ordering");

            return out;
        }

        auto run() {
            std::vector< llvm::CallInst * > sources;
            auto collect = [&](auto inst) { sources.push_back(inst); };
            irops::VerifyInst::for_all_in(fn, collect);

            for (auto source : sources)
                run(source, source);

            return get();
        }
    };

    struct CtxGatherer
    {
        using op_ctxs_t = std::unordered_map< llvm::Instruction *,
                                              std::unordered_set< llvm::Instruction * > >;
        op_ctxs_t op_ctxs;

        void color(llvm::Instruction *ctx, llvm::Instruction *current)
        {
            op_ctxs[current].insert(ctx);

            for (auto u : current->operand_values())
              if (auto inst = llvm::dyn_cast< llvm::Instruction >(u))
                color(ctx, inst);
        }

        op_ctxs_t run(llvm::Function *fn) {
          auto exec = [&](auto ctx) { this->color(ctx, ctx); };
          irops::VerifyInst::for_all_in(fn, exec);
          return std::move(op_ctxs);
        }
    };

    struct UndefReachability
    {
        std::unordered_set< llvm::CallInst * > rcs;
        // TODO(lukas): Possible `seen` is not needed due to structure.
        std::unordered_set< llvm::Value * > seen;

        bool visit( llvm::Instruction * val)
        {
            if (seen.count(val))
                return true;
            seen.insert(val);

            if (irops::Instance< irops::OutputCheck >(val))
            {
                rcs.insert(llvm::dyn_cast< llvm::CallInst >(val));
                return true;
            }

            // Derived value from this is used directly in verified inst
            // without going via reg constraint.
            if (irops::is< irops::VerifyInst >(val))
                return false;

            // NOTE(lukas): Defensively prevent going through calls
            if (llvm::isa< llvm::CallInst >(val))
                return false;

            for (auto op : val->users())
                if (auto as_inst = llvm::dyn_cast< llvm::Instruction >(op))
                    if (!visit(as_inst))
                        return false;
            return true;
        }

        std::optional< std::unordered_set< llvm::CallInst * > > run(llvm::Instruction *val)
        {
            if (visit(val))
                return { std::move( rcs ) };
            return {};
        }
    };

}  // namespace circ
