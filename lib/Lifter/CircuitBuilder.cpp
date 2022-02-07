/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/CircuitBuilder.hpp>

#include <circuitous/Lifter/BaseLifter.hpp>
#include <circuitous/Lifter/Component.hpp>
#include <circuitous/Lifter/DependencyVisitor.hpp>
#include <circuitous/Lifter/Error.hpp>
#include <circuitous/Lifter/Flatten.hpp>
#include <circuitous/Lifter/Memory.hpp>
#include <circuitous/Lifter/Instruction.hpp>
#include <circuitous/Lifter/SelectFold.hpp>

#include <circuitous/IR/Lifter.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Logging.hpp>
#include <circuitous/Support/Check.hpp>

#include <remill/BC/Compat/CallSite.h>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Verifier.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
#include <llvm/Transforms/Utils/Cloning.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <sstream>

DEFINE_bool(liftv2, true, "Produce circuit that allows only verify mode, but is smaller");

namespace circ {

    // TODO(pag): Add other architecture flag names here.
    static const std::string kFlagRegisters =
        ",SF,OF,PF,AF,ZF,CF"  // x86, amd64.
        ",N,Z,C,V"  // AArch64.
        ",icc_c,icc_v,icc_z,icc_n"  // SPARCv8.
        ",xcc_c,xcc_v,xcc_z,xcc_n"  // SPARCv9.
        ",";

    // Return an integral type that is big enough to hold any value can inhabit the
    // register associated with `reg`.
    static llvm::IntegerType *IntegralRegisterType(llvm::Module &module,
                                                   const remill::Register *reg)
    {
        if (reg->type->isIntegerTy())
        {
            // Optimization for flag registers, which should only occupy a single
            // bit. We look to see if it's in the set of
            if (auto found_at = kFlagRegisters.find(reg->name);
                found_at != std::string::npos && 0 < found_at &&
                (found_at + 1u) < kFlagRegisters.size() &&
                kFlagRegisters[found_at - 1u] == ',' &&
                kFlagRegisters[found_at + reg->name.size()] == ',')
            {
                return llvm::Type::getInt1Ty(module.getContext());
            }

            return llvm::dyn_cast<llvm::IntegerType>(reg->type);
        }

        return llvm::Type::getIntNTy(
            module.getContext(),
            static_cast<unsigned>(
                module.getDataLayout().getTypeAllocSize(reg->type) * 8u));
    }

    using reg_ptr_t = const remill::Register *;
    std::vector<reg_ptr_t> EnclosedClosure(reg_ptr_t ptr)
    {
        std::vector<reg_ptr_t> out;
        std::vector<reg_ptr_t> todo{ ptr };
        // Note(lukas): I assume that registers are a tree like structure!
        while (!todo.empty())
        {
            out.push_back(todo.back());
            todo.pop_back();
            for (auto x : out.back()->EnclosedRegisters())
              todo.push_back(x);
        }
        // Just a sanity check
        CHECK(std::unordered_set<reg_ptr_t>(out.begin(), out.end()).size() == out.size());
        return out;
    }

    void State::store(llvm::IRBuilder<> &ir, const reg_ptr_t reg, llvm::Value *val)
    {
        auto bb = ir.GetInsertBlock();
        const auto &dl = bb->getModule()->getDataLayout();
        auto gep = reg->AddressOf(state, bb);
        ir.SetInsertPoint(bb);

        // How much space does register occupy in form iN. There is an
        // optimization for flag registers.
        auto reg_type = IntegralRegisterType(*bb->getModule(), reg);
        auto store_type =
            ir.getIntNTy(static_cast<unsigned>(dl.getTypeAllocSize(reg_type) * 8u));
        auto coerced_type = ir.CreateBitCast(gep, llvm::PointerType::getUnqual(store_type));

        if (reg_type != store_type)
            val = ir.CreateZExt(val, store_type);
        ir.CreateStore(val, coerced_type);
    }

    llvm::Value *State::load(llvm::IRBuilder<> &ir, const reg_ptr_t reg)
    {
        auto bb = ir.GetInsertBlock();
        const auto &dl = bb->getModule()->getDataLayout();
        auto gep = reg->AddressOf(state, bb);
        ir.SetInsertPoint(bb);

        // How much space does register occupy in form iN. There is an
        // optimization for flag registers.
        auto reg_type = IntegralRegisterType(*bb->getModule(), reg);
        auto store_type =
            ir.getIntNTy(static_cast<unsigned>(dl.getTypeAllocSize(reg_type) * 8u));
        auto coerced_type = ir.CreateBitCast(gep, llvm::PointerType::getUnqual(store_type));

        auto loaded = ir.CreateLoad(coerced_type);
        if (reg_type != store_type)
            return ir.CreateTrunc(loaded, reg_type);

        return loaded;
    }


    // After optimizations some context may be merged, but llvm opt will not remove them
    // from the top-level xor function.
    llvm::Function *PostLiftOpt::crop_returns(llvm::Function *fn)
    {
        std::vector< llvm::Instruction * > rets;
        for (auto &bb : *fn)
            for (auto &inst : bb)
                if (auto ret = llvm::dyn_cast< llvm::ReturnInst >(&inst))
                    rets.push_back(ret);

        // There should always be one return
        CHECK(rets.size() == 1);
        auto returned = rets[0]->getOperand(0u);

        auto call = llvm::dyn_cast< llvm::CallInst >(returned);
        CHECK(returned && irops::Xor::is(call->getCalledFunction()));

        // Eliminate all duplicates
        std::unordered_set<llvm::Value *> verifies;
        // Skip all other operands
        std::vector<llvm::Value *> others;
        for (uint32_t i = 0; i < call->getNumArgOperands(); ++i) {
            if (auto verif = llvm::dyn_cast< llvm::CallInst >(call->getArgOperand(i))) {
                if (irops::VerifyInst::is(verif->getCalledFunction()))
                {
                    verifies.insert(verif);
                    continue;
                }
            }
            others.push_back(call->getArgOperand(i));
        }

        llvm::IRBuilder<> ir(call);
        others.insert(others.end(), verifies.begin(), verifies.end());

        auto xor_ = irops::make< irops::Xor >(ir, others);
        call->replaceAllUsesWith(xor_);
        call->eraseFromParent();
        return fn;
    }

    llvm::Function *PostLiftOpt::merge_ctxs(llvm::Function *fn)
    {
        using args_t = std::unordered_set< llvm::Value * >;
        std::map< llvm::CallInst *, args_t > ctxs;
        std::map< llvm::CallInst *, llvm::CallInst * > to_replace;

        auto collect = [&](auto call) {
            args_t args{ call->data_operands_begin(), call->data_operands_end() };

            for (auto &[ctx, c_args] : ctxs) {
                if (c_args == args) {
                    to_replace.emplace(call, ctx);
                    return;
                }
            }
            ctxs.emplace(call, std::move(args));
        };

        irops::VerifyInst::for_all_in(fn, collect);
        for (auto &[old, eq] : to_replace)
        {
            // TOOD(lukas): What about metadata?
            old->replaceAllUsesWith(eq);
            old->eraseFromParent();
        }
        return fn;
    };

    llvm::Function *PostLiftOpt::remove_unused(llvm::Function *fn)
    {
        std::vector< llvm::CallInst * > unused;
        for (auto &bb : *fn)
            for (auto &inst : bb)
                if (auto call = llvm::dyn_cast< llvm::CallInst >(&inst); irops::is_any(call))
                    if (call->hasNUses(0))
                        unused.push_back(call);
        for (auto call : unused)
            call->eraseFromParent();
        return fn;
    };

    llvm::Function *CircuitFunction::_make_fn(const std::string &name)
    {
        std::vector< llvm::Type * > params_types;
        for (auto reg : ctx.regs()) {
            const auto reg_type = IntegralRegisterType(*ctx.module(), reg);
            params_types.push_back(reg_type);
            params_types.push_back(reg_type);
        }
        auto fn_t = llvm::FunctionType::get(ctx.ir().getInt1Ty(), params_types, false);

        auto linkage = llvm::GlobalValue::ExternalLinkage;
        auto fn = llvm::Function::Create(fn_t, linkage, name, ctx.module());
        fn->addFnAttr(llvm::Attribute::ReadNone);

        CHECK(fn->arg_size() % 2 == 0 && fn->arg_size() == ctx.regs().size() * 2);
        for (uint32_t i = 0; i < fn->arg_size(); i += 2) {
            const auto &name = ctx.regs()[ i / 2 ]->name;
            remill::NthArgument(fn, i)->setName(name + ".in");
            remill::NthArgument(fn, i + 1)->setName(name + ".out");

            arg_map.emplace_back(ctx.regs()[ i / 2 ], fn->getArg(i), fn->getArg(i + 1));
        }
        return fn;
    }


    auto CircuitFunction::is_output_reg(llvm::Argument *arg) -> maybe_str
    {
        CHECK(arg->hasName());
        if (!arg->getName().endswith(".out"))
            return std::nullopt;

        auto [name, _] = arg->getName().rsplit('.');
        return std::make_optional(name.str());
    }

    auto CircuitFunction::is_input_reg(llvm::Argument *arg) -> maybe_str
    {
        CHECK(arg->hasName());
        if (!arg->getName().endswith(".in"))
            return std::nullopt;

        auto [name, _] = arg->getName().rsplit('.');
        return std::make_optional(name.str());
    }

    void circuit_builder::inject(const InstructionBatch &batch)
    {
        for (const auto &info : batch.get())
            inject(info);
    }

    void circuit_builder::inject(const InstructionInfo &info)
    {
        auto view = ISEL_view(info.rinst(), info.enc(), info.shadow(), info.lifted());
        inject_semantic_modular(view);
        this->move_head();
    }

    void circuit_builder::handle_undef(const std::string &name)
    {
        auto fn = ctx.module()->getFunction(name);
        // TODO(lukas): For now be defensive and demand that each intrinsic is
        //              at least declared.
        CHECK(fn);
        std::vector< llvm::CallInst * > to_replace;
        for (auto user : fn->users())
            if (auto call = llvm::dyn_cast< llvm::CallInst >(user))
                to_replace.push_back(call);

        for (auto v : to_replace)
            v->replaceAllUsesWith(llvm::UndefValue::get(v->getType()));
    }

    void circuit_builder::handle_undefs()
    {
        for (auto s : { 8, 16, 32, 64 })
            handle_undef("__remill_undefined_" + std::to_string(s));
    }

    void circuit_builder::propagate_undefs()
    {
        auto whose_rc = [&](llvm::CallInst *rc) {
            CHECK(rc->getNumArgOperands() == 2);
            return rc->getArgOperand(1);
        };

        auto get_in_twin = [&](llvm::Value *outreg) {
            for (const auto &[_, in, out] : arg_map)
                if (out == outreg)
                    return in;
            UNREACHABLE() << "Cannot match input register to output reg: " << dbg_dump(outreg);
        };

        auto replace = [&](llvm::Instruction *inst, llvm::Value *patch) {
            for (auto i = 0u; i < inst->getNumOperands(); ++i)
                if (llvm::isa< llvm::UndefValue >(inst->getOperand(i)))
                {
                    llvm::IRBuilder<> irb(inst);
                    // Truncating is probably not correct.
                    auto coerced = irb.CreateSExt(patch, inst->getOperand(i)->getType());
                    inst->setOperand(i,coerced);
                    return;
                }
            UNREACHABLE() << "Was not able to patch undef value";
        };

        std::vector< llvm::Instruction * > undefs;
        for (auto &bb : *circuit_fn)
            for (auto &inst : bb)
                for (auto user : inst.operand_values())
                    if (auto undef = llvm::dyn_cast< llvm::UndefValue >(user))
                    {
                        undefs.push_back(&inst);
                        break;
                    }

        // Function will be modified
        for (auto undef : undefs) {
            // Holes in selects cannot be patched.
            if (irops::is< irops::Select >(undef))
                continue;

            if (auto rcs = UndefReachability().run(undef))
            {
                CHECK(rcs->size() != 0);
                auto patch = get_in_twin(whose_rc(*rcs->begin()));
                for (auto rc : *rcs)
                    CHECK(patch == get_in_twin(whose_rc(rc)));
                // It is enough to replace only one as the `undef` source is exactly one.
                replace(undef, patch);
            } else {
                UNREACHABLE() << "verify depends on undef";
            }
        }
    }

    llvm::Function *circuit_builder::finish()
    {
        // First connect blocks
        tie_head();
        tie_entry();

        values_t ctx_vals;
        for (auto &c : ctxs) {
            // Skip terminator
            llvm::IRBuilder<> irb(&*std::prev(c.current->getParent()->end(), 1));
            ctx_vals.push_back(c.regenerate(irb));
        }

        llvm::IRBuilder<> irb(exit);
        auto all = irops::make< irops::Xor >(irb, ctx_vals);
        tie_exit(all);
        handle_undefs();

        ctx.clean_module({circuit_fn});

        irops::disable_opts< irops::VerifyInst, irops::Select >(ctx.module());

        remill::VerifyModule(ctx.module());
        optimize_silently(ctx.arch(), ctx.module(), {circuit_fn});

        auto selects_map =
            ContextCollector< irops::VerifyInst, irops::Select >(circuit_fn).run();
        SelectFolder folder{ std::move(selects_map), circuit_fn };
        folder.run();

        remill::VerifyModule(ctx.module());
        irops::disable_opts< irops::Select, irops::Advice >(ctx.module());
        irops::enable_opts< irops::VerifyInst, irops::AdviceConstraint,
                            irops::ReadConstraint, irops::WriteConstraint >(ctx.module());

        optimize_silently(ctx.arch(), ctx.module(), {circuit_fn});
        unfold_advice_constraints(circuit_fn);
        propagate_undefs();
        optimize_silently(ctx.arch(), ctx.module(), {circuit_fn});
        remill::VerifyModule(ctx.module());


        return circuit_fn;
    }

    void circuit_builder::inject_semantic_modular(ISEL_view isel)
    {
        CHECK(isel.lifted);
        log_info() << "Lifting: " << isel.instruction.Serialize();

        State state { this->head, ctx.state_ptr_type()->getElementType() };
        auto state_ptr = state.raw();
        llvm::IRBuilder<> ir(this->head);

        for (const auto &[reg, arg, _] : arg_map)
            state.store(ir, reg, arg);

        // Call semantic function
        auto sem_call = call_semantic(ir, isel.lifted, state_ptr, pc(), ctx.undef_mem_ptr());
        // Inline it
        auto make_breakpoint = [](auto ir) {
            return irops::make< irops::Breakpoint >(ir, ir.getTrue());
        };
        auto [begin, end] = inline_flattened(sem_call, make_breakpoint);
        ir.SetInsertPoint(this->head);

        auto params = decoder(ctx, ir, isel).byte_fragments();

        auto mem_checks = mem::synthetize_memory(begin, end, ctx.ptr_size);
        ir.SetInsertPoint(this->head);

        auto [err_checks, c_ebit] = handle_errors(begin, end);

        // Collect annotated instructions - this is the way separate components
        // of the lfiting pipleline communicate
        auto collected = shadowinst::collect_annotated(begin, end);
        auto dst_intrinsics = std::move(collected[Names::meta::dst_reg]);

        auto extra_params = std::move(collected[Names::meta::verify_args]);
        for (std::size_t i = 0; i < extra_params.size(); ++i)
            extra_params[i] = irops::unwrap< irops::Transport >(extra_params[i]);

        begin->eraseFromParent();
        end->eraseFromParent();

        auto dst_regs = lower_dst_regs(dst_intrinsics);
        auto preserved = emit_preserved_checks(dst_regs, isel, state);

        ctxs.emplace_back(this->head,
                          saturation_prop, timestamp_prop, params,
                          mem_checks, err_checks, extra_params
                          );
        auto [dst_cond, dst_regs_checks] = handle_dst_regs_(dst_regs, isel, state);
        ir.SetInsertPoint(this->head);
        auto computational_transition = ir.CreateAnd(dst_regs_checks, preserved);
        auto computational_res = ir.CreateOr(c_ebit, computational_transition);
        auto error_transition = emit_error_transitions(c_ebit);
        ir.SetInsertPoint(this->head);
        ctxs.back()._add(ir.CreateAnd(computational_res, error_transition));
        ctxs.back()._add(dst_cond);

        add_isel_metadata(ctxs.back().current, isel);
    }


    const shadowinst::Reg *get_written(std::size_t idx, ISEL_view isel)
    {
        for (std::size_t i = 0; i < isel.instruction.operands.size(); ++i) {
            // We care only for write operands
            if (isel.instruction.operands[i].action != remill::Operand::Action::kActionWrite)
                continue;

            if (!isel.shadow.operands[i].reg)
                continue;
            if (idx == 0)
                return &(*isel.shadow.operands[i].reg);
            --idx;
        }
        return nullptr;
    }

    llvm::Value *current_val(llvm::Value *dst_reg)
    {
        // Filter all stores
        std::vector< llvm::StoreInst * > stores;
        auto collect_stores = [&](auto src, auto next) -> void {
            for (auto user : src->users()) {
                if (auto store = llvm::dyn_cast< llvm::StoreInst >(user))
                    stores.push_back(store);
                if (auto bc = llvm::dyn_cast< llvm::BitCastInst >(user))
                    next(bc, next);
                CHECK(!llvm::isa< llvm::PtrToIntInst >(user) &&
                      !llvm::isa< llvm::GetElementPtrInst >(user));
            }
        };
        collect_stores(dst_reg, collect_stores);

        // NOTE(lukas): It is expected that if there are multiple stores,
        //              Flattener component will make sure they are properly guarded
        //              wrt path condition.
        CHECK(stores.size() >= 1) << dbg_dump(stores);

        // Next they are being ordered to determine which is last, therefore
        // they need to be in the same basic block
        auto bb = stores[0]->getParent();
        llvm::StoreInst *last = stores[0];

        for (auto store : stores)
            if (inst_distance(&*bb->begin(), store) > inst_distance(&*bb->begin(), last))
                last = store;
        return last->getOperand(0);
    }

    auto circuit_builder::handle_dst_reg(llvm::Instruction *dst_reg,
                                         const shadowinst::Reg &s_reg, State &state)
    -> cond_val_tuple
    {
        llvm::IRBuilder<> irb(this->head);

        auto locate_out_reg = [&](auto &ir, auto &name) { return this->locate_out_reg(name); };
        auto locate_in_reg = [&](auto &ir, auto &name) { return this->locate_in_reg(name); };

        auto [cond, select] = shadowinst::make_intrinsics_decoder(s_reg, irb, locate_out_reg);
        auto [_, full] = shadowinst::make_intrinsics_decoder(s_reg, irb, locate_in_reg);
        auto [dcond, updated] = shadowinst::store_fragment(
            current_val(dst_reg), full, irb, s_reg, *ctx.arch());
        return { dcond, irops::make< irops::OutputCheck >(irb, {updated, select}) };
    }

    auto circuit_builder::handle_dst_regs_(std::vector< llvm::Instruction * > &dst_regs,
                                            ISEL_view isel, State &state)
    -> cond_val_tuple
    {
        CHECK(dst_regs.size() < 3) << "TODO(lukas): Implement more general case.";

        std::vector< cond_val_tuple > partials;
        for (std::size_t i = 0; i < dst_regs.size(); ++i) {
            auto s_reg = get_written(i, isel);
            CHECK(s_reg);
            partials.push_back(handle_dst_reg(dst_regs[i], *s_reg, state));
        }

        llvm::IRBuilder<> irb(this->head);
        llvm::Value *dcond   = irb.getTrue();
        llvm::Value *updated = irb.getTrue();
        for (const auto &[p_cond, p_updated] : partials) {
            dcond =   irb.CreateAnd(dcond, p_cond);
            updated = irb.CreateAnd(updated, p_updated);
        }
        return std::make_tuple( dcond, updated );
    }

    llvm::Value *circuit_builder::emit_preserved_checks(
        instructions_t &dst_regs, ISEL_view &isel, State &state)
    {
        llvm::IRBuilder<> ir(this->head);

        // Returns `false || a0 || ... || an`.
        auto combine = [&](auto vals) {
            llvm::Value *init = ir.getFalse();
            for (auto val : vals)
                init = ir.CreateOr(init, val);
            return init;
        };

        // Return `rhs || lhs`. If either value is `nullptr` use `false` instead.
        auto update = [&](llvm::Value *rhs, llvm::Value *lhs) {
            if (!rhs)
                rhs = ir.getFalse();
            if (!lhs)
                lhs = ir.getFalse();
            return ir.CreateOr(lhs, rhs);
        };

        // Mapping of register to conditions when it is written into.
        std::map< std::string, llvm::Value * > conditions;
        std::unordered_set< std::string > dirty;

        auto current_value = [&](const auto &reg, auto reg_in) {
            // TODO(lukas): May require different behaviour for dirty regs.
            CHECK(!dirty.count(reg->name));
            return state.load(ir, reg);
        };

        auto guard = [&](const auto &name, auto cmp) -> llvm::Value * {
            if (conditions.count(name))
                return ir.CreateOr(conditions[name], cmp);
            return cmp;
        };

        for (std::size_t i = 0; i < isel.instruction.operands.size(); ++i)
        {
            if (isel.instruction.operands[i].action != remill::Operand::Action::kActionWrite)
                continue;

            auto &s_op = isel.shadow.operands[i];
            if (!s_op.reg)
                continue;

            auto &s_reg = *s_op.reg;
            dirty.insert(s_reg.dirty.begin(), s_reg.dirty.end());

            for (auto &[reg, vals] : shadowinst::decoder_conditions(ctx.arch(), s_reg, ir))
                conditions[reg] = update(combine(vals), conditions[reg]);
        }

        std::vector< llvm::Value * > args;
        for (auto [reg, reg_in, reg_out] : arg_map)
        {
            auto cmp =
                irops::make< irops::OutputCheck >(ir, {current_value(reg, reg_in), reg_out});
            args.push_back(guard(reg->name, cmp));
        }
        auto all = irops::make< irops::And >(ir, args);
        return all;
    }


    auto circuit_builder::handle_dst_regs(
        llvm::Value *current_ebit,
        std::vector<llvm::Instruction *> &dst_regs, ISEL_view isel, State &state)
    -> values_t
    {
        // Comparisons on whether or not the resulting
        // register after the semantic has executed matches the next state of that
        // register.
        std::vector<llvm::Value *> params;
        llvm::IRBuilder<> ir(this->head);

        for (auto [reg, input_reg, expected_reg_val] : this->arg_map)
        {
            llvm::Value *original_val = state.load(ir, reg);
            llvm::Value *reg_val = original_val;
            // We need to keep track which operand we are about to handle so we can index into
            // `dst_regs`.
            uint64_t proccessed = 0;

            for (std::size_t i = 0; i < isel.instruction.operands.size(); ++i) {
                // We care only for write operands
                if (isel.instruction.operands[i].action !=
                    remill::Operand::Action::kActionWrite)
                {
                  continue;
                }
                // Everything destination is "hardcoded", we do not need to take care
                // of anything.
                if (dst_regs.size() == 0)
                    continue;

                auto &s_op = isel.shadow.operands[i];
                if (!s_op.reg)
                    continue;

                ++proccessed;
                auto &table = s_op.reg->translation_map;

                if (s_op.reg->is_dirty(reg->name))
                    continue;

                for (auto reg_part : EnclosedClosure(reg)) {
                    if (!table.count(reg_part->name))
                        continue;


                    // The basic idea here (we need to handle partial registers)
                    // is that we first "refresh" the top-level with originally loaded value.
                    // That is needed because in previous iteration something else may have been
                    // store there.
                    // Then we write the value into the partial register and we again load from
                    // the top-level to retrieve the value (with correctly stored value).
                    // This is a lot of memory operations and we rely heavily on llvm
                    // `mm2reg` pass to help us out.

                    // Someone before us may have written something - we need to reset the value.
                    state.store(ir, reg, original_val);
                    auto reg_checks =
                        shadowinst::decoder_conditions(*s_op.reg, reg_part->name, ir);

                    // Check if everything is still valid.
                    CHECK(proccessed - 1 < dst_regs.size()) << proccessed - 1
                                                            << " >= " << dst_regs.size();
                    auto eq = irops::make< irops::Xor >(ir, reg_checks);
                    auto dst_load = ir.CreateLoad(dst_regs[proccessed - 1]);
                    auto reg_addr = reg_part->AddressOf(state.raw(), ir);

                    auto store_ty =
                        llvm::cast<llvm::PointerType>(reg_addr->getType())->getElementType();

                    ir.CreateStore(ir.CreateSExtOrTrunc(dst_load, store_ty), reg_addr);
                    auto full_val = state.load(ir, reg);
                    reg_val = ir.CreateSelect(eq, full_val, reg_val);
                }
            }
            CHECK(current_ebit);
            CHECK(input_reg);
            CHECK(reg_val);
            // If error bit is raised we are not moving anywhere
            auto guard = ir.CreateSelect(current_ebit, input_reg, reg_val);
            params.push_back(irops::make< irops::OutputCheck >(ir, {guard, expected_reg_val}));
        }
        return params;
    }

    auto circuit_builder::lower_dst_regs(const values_t & dsts) -> instructions_t
    {
        instructions_t out;

        for (auto dst : dsts) {
            auto p_type = llvm::dyn_cast< llvm::PointerType >(dst->getType());
            CHECK(p_type) << "Dst reg type before lowering is not pointer";

            llvm::IRBuilder<> ir(llvm::cast< llvm::Instruction >(dst));
            out.push_back(ir.CreateAlloca(p_type->getPointerElementType(), nullptr, "DSTA_"));
            dst->replaceAllUsesWith(out.back());
            llvm::dyn_cast< llvm::Instruction >(dst)->eraseFromParent();
        }
        return out;
    }

    llvm::Value *circuit_builder::emit_error_transitions(llvm::Value *current_ebit)
    {
        std::vector< llvm::Value * > args;
        for (const auto &[_, rc] : default_rcs)
            args.push_back(rc);

        llvm::IRBuilder<> irb(this->head);
        auto all_def_rcs = irops::make< irops::And >(irb, args);
        return irb.CreateOr(irb.CreateNot(current_ebit), all_def_rcs);
    }

    auto circuit_builder::handle_errors(llvm::Value *begin, llvm::Value *end)
    -> std::tuple< values_t, llvm::Value * >
    {
        values_t out;

        llvm::IRBuilder<> irb(this->head);
        auto [ebit_in, ebit_out] = irops::make_all_leaves< irops::ErrorBit >(irb);

        auto current_err = [&](llvm::Value *ebit_in_ = ebit_in) -> llvm::Value * {
            auto delta_err = err::synthesise_current(irb, begin, end);
            if (delta_err)
                // Error bit can be saturated, so we need to `or` input and current.
                return irb.CreateOr(ebit_in_, delta_err);

            // This instruction cannot raise error bit -> input error bit
            // cannot be set.
            out.push_back(irb.CreateICmpEQ(ebit_in_, irb.getFalse()));
            return irb.getFalse();
        }();
        out.push_back(irops::make< irops::OutputCheck >(irb, {current_err, ebit_out}));
        return std::make_tuple(out, current_err);
    }


    std::string decoder::generate_raw_bytes(const std::string &full, uint64_t from, uint64_t to)
    {
        std::string out;
        while(true) {
            // NOTE(lukas): To handle un-aligned values.
            uint64_t y = std::min(from + (8 - from % 8), to);
            std::string partial = full.substr(from, y - from);
            std::reverse(partial.begin(), partial.end());

            out = partial + out;
            if (y == to)
                return out;

            from = y;
        }
    }

    llvm::Value *decoder::create_bit_check(uint64_t from, uint64_t to)
    {
        auto encoding = convert_encoding(isel.encoding);
        std::string expected = generate_raw_bytes(encoding, from, to);

        auto size = static_cast< uint32_t >(expected.size());
        CHECK(size == to - from) << size << " != " << to - from;

        auto expected_v = ir.getInt(llvm::APInt(size, expected, 2));
        auto extracted = irops::make_leaf< irops::ExtractRaw >(ir, from, to - from);
        return irops::make< irops::DecodeCondition >(ir, {expected_v, extracted}, size);
    }

    auto decoder::byte_fragments() -> values_t
    {
        values_t out;

        auto unknown_regions = isel.shadow.UnknownRegions(rinst_size());
        // `unknown_regions` are in `[ from, size ]` format.
        for (auto [from, to] : shadowinst::FromToFormat(unknown_regions))
            out.push_back(create_bit_check(from, to));

        // TODO(lukas): Now we need to check the tail.
        //              Try to lift `6689d8` and `89d8` to demonstrate the issue.
        // TODO(lukas): For now we assume it is padded with 0s.
        auto tail_size = static_cast< uint32_t >(kMaxNumInstBits - rinst_size());
        auto tail = ir.getInt(llvm::APInt(tail_size, 0, false));

        auto extracted = irops::make_leaf< irops::ExtractRaw >(ir, rinst_size(), tail_size);
        auto compare = irops::make< irops::DecodeCondition >(ir, {tail, extracted}, tail_size);
        out.push_back(compare);
        return out;
    }

    void CircuitMaker::prepare_module()
    {
        if (auto used = ctx.module()->getGlobalVariable("llvm.used"))
            used->eraseFromParent();

        EraseFns(ctx.module(), { "__remill_intrinsics", "__remill_mark_as_used" });

        // These improve optimizability.
        mute_state_escape("__remill_function_return");
        mute_state_escape("__remill_error");
        mute_state_escape("__remill_missing_block");
    }

    llvm::Function * CircuitMaker::make_from(const InstructionBatch &batch)
    {
        prepare_module();

        circuit_builder builder(ctx, "circuit.1.0");
        builder.inject(batch);

        return PostLiftOpt::run(builder.finish());
    }
}  // namespace circ
