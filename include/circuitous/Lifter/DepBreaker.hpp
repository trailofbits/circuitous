/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Lifter/DependencyVisitor.hpp>
#include <circuitous/Util/LLVMUtil.hpp>

namespace circ {

    static inline uint32_t isize(llvm::Value *v) {
        auto type = llvm::dyn_cast< llvm::IntegerType >(v->getType());
        CHECK(type);
        return static_cast< uint32_t >(type->getBitWidth());
    }


    struct Manufacturer
    {
        using inst_t = llvm::Instruction *;
        using insts_t = std::vector< inst_t >;
        using val_t = llvm::Value *;
        using vals_t = std::vector< val_t >;

        virtual ~Manufacturer() = default;

        virtual bool is_target(inst_t inst) = 0;
        virtual uint32_t op_size() = 0;
        virtual inst_t make_bp(llvm::IRBuilder<> &irb) = 0;
        virtual inst_t make_bp(llvm::IRBuilder<> &irb, std::vector< inst_t > ops) = 0;

        inst_t as_inst(llvm::Value *v)
        {
            auto i = llvm::dyn_cast< llvm::Instruction >(v);
            check(i);
            return i;
        }

        virtual vals_t coerce_ins(inst_t inst, llvm::IRBuilder<> &irb)
        {
            vals_t out;
            for (uint32_t i = 0u; i < inst->getNumOperands(); ++i)
                out.push_back(coerce_in(inst->getOperand(i), irb));
            return out;
        }

        virtual val_t coerce_in(llvm::Value *val, llvm::IRBuilder<> &irb)
        {
            check(isize(val) <= this->op_size());
            return irb.CreateSExt(val, this->op_type(irb));
        }

        virtual val_t coerce_out(inst_t inst, inst_t bp)
        {
            check(isize(inst) <= isize(bp));
            auto ip = bp->getNextNonDebugInstruction();
            return llvm::IRBuilder<>(ip).CreateTrunc(bp, inst->getType());
        }

        virtual llvm::Type *op_type(llvm::IRBuilder<> &irb)
        {
            return irb.getIntNTy(this->op_size());
        }
    };

    struct BasicManufacturer : Manufacturer
    {
        using parent_t = Manufacturer;
        using inst_t = typename parent_t::inst_t;

        bool is_target(inst_t i) override
        {
            return i->getOpcode() == llvm::BinaryOperator::Add && isize(i) <= this->op_size();
        }

        uint32_t op_size() override
        {
            return 64u;
        }

        inst_t make_bp(llvm::IRBuilder<> &irb, std::vector< inst_t > ops) override
        {
            return this->as_inst(irb.CreateAdd(ops[0], ops[1]));
        }

        inst_t make_bp(llvm::IRBuilder<> &irb) override
        {
            std::vector< llvm::Instruction * > ops;
            for (std::size_t i = 0; i < 2; ++i)
                ops.push_back(irops::make_leaf< irops::Advice >(irb, this->op_type(irb)));
            return this->make_bp(irb, ops);
        }
    };

    struct DepBreaker
    {
        // Sets to lead to all problems with non-determinism.
        using deterministic_inst = std::tuple< llvm::Instruction *, uint32_t >;
        using insts_t = std::vector< deterministic_inst >;

        // Context is just call to proper intrinsic
        using ctx_t = llvm::Instruction *;

        // Blueprint is llvm instruction in a form `op A1, A2, ..., An`.
        using blueprint_t = llvm::Instruction *;
        // For each blueprint we must remember which contexts already use it.
        using usage_t = std::unordered_set< ctx_t >;
        using blueprint_usage_t = std::tuple< blueprint_t, usage_t >;

        // Collection of all existing blueprints with their usages. In vector
        // to avoid non-determinism.
        using blueprints_t = std::deque< blueprint_usage_t >;

        using to_update_t = std::unordered_map< ctx_t, std::vector< llvm::Instruction * > >;

        CtxGatherer info;
        llvm::Function *fn;
        Manufacturer &man;

        blueprints_t blueprints;
        to_update_t to_update;

        DepBreaker(llvm::Function *fn_, Manufacturer &man_) : fn(fn_), man(man_)
        {
            info.op_ctxs = info.run(fn);
        }

        void run()
        {
            assign_blueprints(collect());
            for (auto &[ctx, acs] : to_update)
                component::Context(ctx).add_operands(acs).regenerate();
        }


        insts_t collect()
        {
            insts_t out;
            uint32_t found = 1;
            for (auto &bb : *fn)
                for (auto &inst : bb)
                    if (auto as_inst = llvm::dyn_cast< llvm::Instruction >(&inst))
                        if (man.is_target(as_inst))
                        {
                            log_info() << dbg_dump(as_inst);
                            out.emplace_back(as_inst, ++found);
                        }
            return out;
        }

        void assign_blueprints(insts_t insts)
        {
            for (auto &[inst, _] : insts)
                use_bp(inst, select_bp(inst));
        }

        void use_bp(llvm::Instruction *inst, blueprint_usage_t &bp_usage)
        {
            auto &[bp, usage] = bp_usage;
            for (auto ctx : info.op_ctxs[inst])
                usage.insert(ctx);

            llvm::IRBuilder<> irb(inst);
            auto coerced_ops = man.coerce_ins(inst, irb);

            std::vector< llvm::Instruction * > acs;
            for (unsigned i = 0; i < inst->getNumOperands(); ++i)
            {
                auto ac = irops::make< irops::AdviceConstraint >(
                        irb, {coerced_ops[i], bp->getOperand(i)});
                acs.push_back(ac);
            }

            for (auto ctx : info.op_ctxs[inst])
            {
                auto &data = to_update[ctx];
                data.insert(data.end(), acs.begin(), acs.end());
            }

            inst->replaceAllUsesWith(man.coerce_out(inst, bp));
        }

        blueprint_usage_t &select_bp(llvm::Instruction *inst)
        {
            for (auto &bp_usage : blueprints)
            {
                auto &[bp, usage] = bp_usage;
                if (compatible(inst, usage))
                    return bp_usage;
            }
            return make_bp();
        }

        blueprint_usage_t &make_bp()
        {
            llvm::IRBuilder<> irb(&*fn->begin()->begin());
            blueprints.emplace_back(man.make_bp(irb), usage_t{});
            return blueprints.back();
        }

        bool compatible(llvm::Instruction *inst, usage_t &usage)
        {
            for (auto ctx : info.op_ctxs[inst])
                if (usage.count(ctx))
                    return false;
            return true;
        }
    };

} // namespace circ
