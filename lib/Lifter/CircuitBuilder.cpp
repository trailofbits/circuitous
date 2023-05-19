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
#include <circuitous/Lifter/Undefs.hpp>

#include <circuitous/Lifter/Components/Decoder.hpp>
#include <circuitous/Lifter/Components/OperandSelection.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Check.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Verifier.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
#include <llvm/Transforms/Utils/Cloning.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <sstream>

namespace circ
{
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
        check(std::unordered_set<reg_ptr_t>(out.begin(), out.end()).size() == out.size());
        return out;
    }

    wraps_remill_value::wraps_remill_value( llvm::Function *fn, llvm::Type *t )
    {
        check( !fn->isDeclaration() );
        storage = llvm::IRBuilder<>( &*fn->begin() ).CreateAlloca( t );
    }

    void State::store(llvm::IRBuilder<> &ir, const reg_ptr_t reg, llvm::Value *val)
    {
        auto bb = ir.GetInsertBlock();
        const auto &dl = bb->getModule()->getDataLayout();
        auto gep = reg->AddressOf(storage, bb);
        ir.SetInsertPoint(bb);

        // How much space does register occupy in form iN. There is an
        // optimization for flag registers.
        auto reg_type = irops::int_reg_type(*bb->getModule(), reg);
        auto store_type =
            ir.getIntNTy(static_cast<unsigned>(dl.getTypeAllocSize(reg_type) * 8u));
        auto coerced_type = ir.CreateBitCast(gep, llvm::PointerType::getUnqual(store_type));

        if (reg_type != store_type)
            val = ir.CreateZExt(val, store_type);
        ir.CreateStore(val, coerced_type);
    }

    llvm::Value *State::load(llvm::IRBuilder<> &ir, const reg_ptr_t reg)
    {
        check( reg );
        auto bb = ir.GetInsertBlock();
        const auto &dl = bb->getModule()->getDataLayout();
        auto gep = reg->AddressOf(storage, bb);
        ir.SetInsertPoint(bb);

        // How much space does register occupy in form iN. There is an
        // optimization for flag registers.
        auto reg_type = irops::int_reg_type(*bb->getModule(), reg);
        auto store_type =
            ir.getIntNTy(static_cast<unsigned>(dl.getTypeAllocSize(reg_type) * 8u));

        auto loaded = ir.CreateLoad(store_type, gep);
        if (reg_type != store_type)
            return ir.CreateTrunc(loaded, reg_type);

        return loaded;
    }


    void State::reset( llvm::IRBuilder<> &irb, const Ctx::regs_t &regs )
    {
        log_info() << "[state]: reset";
        for ( const auto &reg : regs )
            store( irb, reg, irops::input_reg( irb, reg ) );
    }

    void State::commit( llvm::IRBuilder<> &irb, CtxRef ctx )
    {
        std::vector< llvm::Value * > args;
        for ( const auto &reg : ctx.regs() )
            args.push_back( load( irb, reg ) );
        irops::make< irops::Commit >( irb, args, 1u );

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
        check(rets.size() == 1);
        auto returned = rets[0]->getOperand(0u);

        auto call = llvm::dyn_cast< llvm::CallInst >(returned);
        check(returned && irops::Xor::is(call->getCalledFunction()));

        // Eliminate all duplicates
        std::unordered_set<llvm::Value *> verifies;
        // Skip all other operands
        std::vector<llvm::Value *> others;
        for (uint32_t i = 0; i < call->arg_size(); ++i) {
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
            const auto reg_type = irops::int_reg_type(*ctx.module(), reg);
            params_types.push_back(reg_type);
            params_types.push_back(reg_type);
        }
        auto fn_t = llvm::FunctionType::get(ctx.ir().getInt1Ty(), params_types, false);

        auto linkage = llvm::GlobalValue::ExternalLinkage;
        auto fn = llvm::Function::Create(fn_t, linkage, name, ctx.module());
        fn->addFnAttr(llvm::Attribute::ReadNone);

        check(fn->arg_size() % 2 == 0 && fn->arg_size() == ctx.regs().size() * 2);
        for (uint32_t i = 0; i < fn->arg_size(); i += 2) {
            const auto &reg_name = ctx.regs()[ i / 2 ]->name;
            remill::NthArgument(fn, i)->setName(reg_name + ".in");
            remill::NthArgument(fn, i + 1)->setName(reg_name + ".out");

            arg_map.emplace_back(ctx.regs()[ i / 2 ], fn->getArg(i), fn->getArg(i + 1));
        }
        return fn;
    }


    auto CircuitFunction::is_output_reg(llvm::Argument *arg) -> maybe_str
    {
        check(arg->hasName());
        if (!arg->getName().endswith(".out"))
            return std::nullopt;

        auto [name, _] = arg->getName().rsplit('.');
        return std::make_optional(name.str());
    }

    auto CircuitFunction::is_input_reg(llvm::Argument *arg) -> maybe_str
    {
        check(arg->hasName());
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
        auto view = ISEL_view(info.rinst(), info.enc(), info.shadows, info.lifted());
        inject_semantic_modular(view);
        this->move_head();
    }

    void circuit_builder::handle_undef(const std::string &name)
    {
        auto fn = ctx.module()->getFunction(name);
        // TODO(lukas): For now be defensive and demand that each intrinsic is
        //              at least declared.
        check(fn);
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
            check(rc->arg_size() == 2);
            return rc->getArgOperand(1);
        };

        auto get_in_twin = [&](llvm::Value *outreg) {
            for (const auto &[_, in, out] : arg_map)
                if (out == outreg)
                    return in;
            unreachable() << "Cannot match input register to output reg: " << dbg_dump(outreg);
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
            unreachable() << "Was not able to patch undef value";
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
                check(rcs->size() != 0);
                auto patch = get_in_twin(whose_rc(*rcs->begin()));
                for (auto rc : *rcs)
                    check(patch == get_in_twin(whose_rc(rc)));
                // It is enough to replace only one as the `undef` source is exactly one.
                replace(undef, patch);
            } else {
                unreachable() << "verify depends on undef";
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
        optimize_silently(ctx.module(), {circuit_fn});

        SelectFolder::ctx_to_selects_t select_map =
            ContextCollector< irops::VerifyInst, irops::Select >( circuit_fn ).run();

        auto select_fold = SelectFolder( select_map, circuit_fn );
        select_fold.run();

        remill::VerifyModule(ctx.module());
        irops::disable_opts< irops::Select, irops::Advice >(ctx.module());
        irops::enable_opts< irops::VerifyInst, irops::AdviceConstraint,
                            irops::ReadConstraint, irops::WriteConstraint >(ctx.module());

        optimize_silently(ctx.module(), {circuit_fn});
        propagate_undefs();
        optimize_silently(ctx.module(), {circuit_fn});
        remill::VerifyModule(ctx.module());


        return circuit_fn;
    }

    void circuit_builder::inject_semantic_modular(ISEL_view isel)
    {
        check(isel.lifted);

        State state { this->head, ctx.state_ptr_type() };
        auto state_ptr = *state;
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

        auto [params, reg_selector_constraint] =
            build::Decoder(ir, isel).get_decoder_tree();

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
                          saturation_prop, timestamp_prop, params, reg_selector_constraint,
                          mem_checks, err_checks, extra_params
                          );
        auto [dst_cond, dst_regs_checks] = handle_dst_regs_(dst_regs, isel, state);
        ir.SetInsertPoint(this->head);
        auto computational_transition = ir.CreateAnd(dst_regs_checks, preserved);
        auto computational_res = ir.CreateOr(c_ebit, computational_transition);
        auto error_transition = emit_error_transitions(c_ebit);
        ir.SetInsertPoint(this->head);
        ctxs.back()._add(ir.CreateAnd(computational_res, error_transition));

        add_isel_metadata(ctxs.back().current, isel);
    }


    auto get_written(std::size_t idx, ISEL_view isel)
    -> std::tuple< const shadowinst::Reg *, std::size_t >
    {
        for (std::size_t i = 0; i < isel.instruction.operands.size(); ++i) {
            // We care only for write operands
            if (isel.instruction.operands[i].action != remill::Operand::Action::kActionWrite)
                continue;

            if (!isel.shadows[ 0 ].operands[i].reg())
                continue;
            if (idx == 0)
                return { &(*isel.shadows[ 0 ].operands[i].reg()), i };
            --idx;
        }
        return { nullptr, 0 };
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
                check(!llvm::isa< llvm::PtrToIntInst >(user) &&
                      !llvm::isa< llvm::GetElementPtrInst >(user));
            }
        };
        collect_stores(dst_reg, collect_stores);

        // NOTE(lukas): It is expected that if there are multiple stores,
        //              Flattener component will make sure they are properly guarded
        //              wrt path condition.
        check(stores.size() >= 1) << dbg_dump(stores);

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
                                         const shadowinst::Reg &s_reg, State &state,
                                         std::size_t reg_idx)
    -> cond_val_tuple
    {
        llvm::IRBuilder<> irb(this->head);

        auto locate_out_reg = [&](auto &ir, auto &name) { return this->locate_out_reg(name); };
        auto locate_in_reg = [&](auto &ir, auto &name) { return this->locate_in_reg(name); };

        auto m = shadowinst::Materializer(irb, s_reg);
        auto select = m.unguarded_decoder(locate_out_reg);
        auto full = m.unguarded_decoder(locate_in_reg);

        auto [dcond, updated] = shadowinst::store_fragment(
            current_val(dst_reg), full, irb, s_reg, *ctx.arch());
        return { dcond, irops::make< irops::OutputCheck >(irb, {updated, select}) };
    }

    auto circuit_builder::handle_dst_regs_(std::vector< llvm::Instruction * > &dst_regs,
                                            ISEL_view isel, State &state)
    -> cond_val_tuple
    {
        check(dst_regs.size() < 3) << "TODO(lukas): Implement more general case.";

        std::vector< cond_val_tuple > partials;
        for (std::size_t i = 0; i < dst_regs.size(); ++i) {
            auto [s_reg, reg_idx] = get_written(i, isel);
            check(s_reg);
            partials.push_back(handle_dst_reg(dst_regs[i], *s_reg, state, reg_idx));
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
            check(!dirty.count(reg->name));
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

            auto &s_op = isel.shadows[ 0 ].operands[i];
            if (!s_op.reg())
                continue;

            auto &s_reg = *s_op.reg();
            dirty.insert(s_reg.dirty.begin(), s_reg.dirty.end());

            auto m = shadowinst::Materializer(ir, s_reg);
            for (auto &[reg, vals] : m.translation_map(ctx.arch()))
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

                auto &s_op = isel.shadows[ 0 ].operands[i];
                if (!s_op.reg())
                    continue;

                ++proccessed;
                auto &table = s_op.reg()->translation_map;

                if (s_op.reg()->is_dirty(reg->name))
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

                    // Someone before us may have written something - we need to
                    // reset the value.

                    state.store(ir, reg, original_val);
                    auto m = shadowinst::Materializer(ir, *s_op.reg());
                    auto reg_checks = m.translation_entries_of(reg_part->name);

                    // Check if everything is still valid.
                    check(proccessed - 1 < dst_regs.size()) << proccessed - 1
                                                            << " >= " << dst_regs.size();
                    auto eq = irops::make< irops::Xor >(ir, reg_checks);
                    auto dst_load = make_non_opaque_load(ir, dst_regs[proccessed - 1]);
                    auto reg_addr = reg_part->AddressOf(*state, ir);

                    auto store_ty = llvm::cast<llvm::PointerType>(reg_addr->getType());

                    ir.CreateStore(ir.CreateSExtOrTrunc(dst_load, store_ty), reg_addr);
                    auto full_val = state.load(ir, reg);
                    reg_val = ir.CreateSelect(eq, full_val, reg_val);
                }
            }
            check(current_ebit);
            check(input_reg);
            check(reg_val);
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
            check(p_type) << "Dst reg type before lowering is not pointer";

            llvm::IRBuilder<> ir(llvm::cast< llvm::Instruction >(dst));
            out.push_back(ir.CreateAlloca(p_type, nullptr, "DSTA_"));
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

    void CircuitMaker::prepare_module()
    {
        if (auto used = ctx.module()->getGlobalVariable("llvm.used"))
            used->eraseFromParent();

        EraseFns(ctx.module(), { "__remill_intrinsics", "__remill_mark_as_used" });

        // These improve optimizability.
        mute_state_escape("__remill_function_return");
        mute_state_escape("__remill_error");
        mute_state_escape("__remill_missing_block");

        std::vector< llvm::Function * > to_remove;

        for (auto &fn : *ctx.module())
        {
            if (!remill::HasOriginType< remill::Semantics >(&fn))
                continue;
            if (fn.isDeclaration())
                continue;
            to_remove.push_back(&fn);
        }
        safe_erase_from_parent(std::move(to_remove));
    }

    llvm::Function * CircuitMaker::make_from(const InstructionBatch &batch)
    {
        prepare_module();

        circuit_builder builder(ctx, "circuit.1.0");
        builder.inject(batch);

        return PostLiftOpt::run(builder.finish());
    }


    /** _v2 **/

    CircuitFunction_v2::CircuitFunction_v2( Ctx &ctx )
        : fn( mk_function( ctx ) ),
          trace( ctx, State( &fn, ctx.state_type() ), State( &fn, ctx.state_type() ) ),
          memory_ptr( ctx.memory_ptr_type() ),
          irb_instance( &*fn.begin() )
    {
        log_info() << "[circ-fn]:" << "Adjusting insert point";
        auto bb = &*fn.begin();
        irb_instance.SetInsertPoint( bb, bb->begin() );
    }

    llvm::Function &CircuitFunction_v2::mk_function( Ctx &ctx )
    {
        auto type = llvm::FunctionType::get( ctx.ir().getInt1Ty(), {}, false );
        auto linkage = llvm::GlobalValue::ExternalLinkage;
        auto fn = llvm::Function::Create( type, linkage, "__circ.circuit_v2", ctx.module() );

        llvm::BasicBlock::Create( *ctx.llvm_ctx(), "entry", fn );

        log_info() << "[circ-fn]:" << "Dummy terminator.";
        llvm::IRBuilder<> irb( &*fn->begin() );
        //irb.CreateRet( irb.getTrue() );
        return *fn;
    }

    void ExaltationContext::exalt( unit_t &unit )
    {
        log_info() << "[exalt]: Starting to exalt a unit ...";

        for ( auto &atom : unit )
        {
            log_info() << "[exalt]:" << atom.concrete.Serialize();
            log_info() << "[exalt]:" << atom.abstract.to_string();
        }

        reset_state();

        log_info() << "[exalt]: Synthetizing decoders ...";
        auto decoders = synthetize_decoders( unit );

        auto &irb = this->irb();
        log_info() << "[exalt]: Bumping pc ...";
        bump_pc( decoders, unit );

        log_info() << "[exalt]: Fetching semantic ...";
        auto semantic = isem::semantic_fn( unit.isel, *ctx.module() );
        post_lift( **semantic );


        log_info() << "[exalt]: Lifting & binding operands ...";

        std::vector< llvm::Value * > lifted_operands = { *memory_ptr(), *input() };
        std::vector< std::tuple< llvm::Instruction *, std::size_t > > writes;

        auto requester = build::StatelessRequester( ctx, input() );
        for ( std::size_t i = 0; i < unit.operand_count(); ++i )
        {
            log_info() << "[exalt]:" << "Operand" << i;
            auto decoder_it = decoders.begin();

            auto arg = remill::NthArgument( *semantic, 2 + i );
            if ( unit.is_write( i ) &&
                 unit.atoms.front().concrete.operands[ i ].type == remill::Operand::kTypeRegister )
            {
                auto dst = irops::AllocateDst::make( irb, *input(), arg->getType() );

                log_info() << dbg_dump( (*semantic)->getType() );
                log_info() << dbg_dump( dst );
                lifted_operands.emplace_back( dst );
                writes.emplace_back( dst, i );
                continue;
            }

            std::vector< llvm::Value * > options;
            for ( auto view : unit.slices( i ) )
            {
                log_info() << "[exalt]:" << "Processing slice_view";
                auto lifter = build::OperandLifter( ctx, irb, requester, unit.is_read( i ) );
                auto operand = lifter.lift( view );

                if ( ctx.bw( operand ) < ctx.bw( arg ) )
                    operand = irb.CreateSExt( operand, arg->getType() );

                auto option = irops::Option::make( irb,
                                                   { operand, *( decoder_it++ ) },
                                                   ctx.bw( operand ) );
                options.emplace_back( option );
            }

            auto merged = irops::Switch::make( irb, options );
            lifted_operands.emplace_back( merged );

            log_info() << "[exalt]:" << "Operand" << i << "done";
        }

        for ( auto o : lifted_operands )
            log_info() << dbg_dump( o );

        auto sem_call = irb.CreateCall( *semantic, lifted_operands );

        auto make_breakpoint = []( auto ir )
        {
            return irops::make< irops::Breakpoint >( ir, ir.getTrue() );
        };
        auto [ begin, end ] = inline_flattened( sem_call, make_breakpoint );
        auto mem_checks = mem::synthetize_memory( begin, end, ctx.ptr_size );

        begin->eraseFromParent();
        end->eraseFromParent();

        log_info() << "[exalt]:" << "Post call";

        std::map< std::size_t, std::tuple< stores_t, reg_to_vals > > tmp;
        for ( auto [ dst, idx ] : writes )
        {
            auto conds = written_condition( decoders, unit, idx );
            auto stores = stores_to( dst );
            tmp.emplace( idx, std::make_tuple( std::move( stores ), std::move( conds ) ) );
        }

        auto merged_decoders = irops::Or::make( irb, decoders );
        auto unit_decoder = irops::DecoderResult::make( irb, merged_decoders );
        mem_checks.emplace_back( unit_decoder );

        auto [ebit_in, ebit_out] = irops::make_all_leaves< irops::ErrorBit >(irb);
        mem_checks.emplace_back( irb.CreateICmpEQ( ebit_in, ebit_out ) );
        auto unit_ctx = irops::VerifyInst::make( irb, mem_checks );
        sub_root.emplace_back( unit_ctx );

        for ( auto &reg : ctx.regs() )
        {
            auto normal_flow_value = input().load( irb, reg );

            std::vector< llvm::Value * > options;
            for ( auto [ _, data ] : tmp )
            {
                auto [ stores, conds ] = data;
                for ( auto &[ key, vals ] : conds )
                {
                    if ( enclosing_reg( ctx.arch(), key ) != reg )
                        continue;

                    auto runtime_value = last_store( stores );
                    input().store( irb, ctx.reg( key ), runtime_value );

                    auto full_val = input().load( irb, reg );

                    auto full_conds = irops::Or::make( irb, vals );
                    options.emplace_back( irops::Option::make( irb,
                                                               { full_val, full_conds },
                                                               ctx.bw( full_val ) ) );
                    // Clean up.
                    input().store( irb, reg, normal_flow_value );
                }

            }

            if ( options.empty() )
            {
                checks[ reg ].emplace_back( unit_decoder, normal_flow_value );
            } else {
                options.push_back( irops::Option::make( irb,
                                                        {normal_flow_value, irb.getTrue() },
                                                        ctx.bw( normal_flow_value ) ) );
                auto s = irops::Switch::make( irb, options );
                checks[ reg ].emplace_back( unit_decoder, s );
            }
        }
    }

    void ExaltationContext::remove_write( llvm::Value *dst )
    {
        auto p_type = llvm::dyn_cast< llvm::PointerType >( dst->getType() );
        check( p_type ) << "Dst reg type before lowering is not pointer";

        irb().SetInsertPoint( llvm::cast< llvm::Instruction >( dst ) );
        auto allocation = irb().CreateAlloca( p_type,
                                              nullptr, "DSTA_" );

        auto as_inst = llvm::dyn_cast< llvm::Instruction >( dst );
        check( as_inst );
        as_inst->replaceAllUsesWith( allocation );
        as_inst->eraseFromParent();
    }


    auto ExaltationContext::synthetize_decoders( unit_t &unit ) -> values_t
    {
        values_t out;

        auto &irb = this->irb();
        for ( auto &atom : unit )
            out.push_back( build::AtomDecoder( irb, atom ).get_decoder_tree() );
        return out;
    }

    auto ExaltationContext::written_condition( const values_t &decoders,
                                               unit_t &unit,
                                               std::size_t idx )
        -> reg_to_vals
    {
        reg_to_vals out;

        auto decoder = decoders.begin();

        auto &irb = this->irb();
        for ( auto view : unit.slices( idx ) )
        {
            auto reg = std::get_if< typename decltype( view )::reg >( &view.raw );
            check( reg );

            auto [ r_reg, s_reg ] = *reg;

            if ( !s_reg )
            {
                ++decoder;
                continue;
            }

            if ( s_reg->tm().empty() )
            {
                out[ r_reg->name ].emplace_back( *decoder );
                ++decoder;
                continue;
            }

            auto selector = shadowinst::Materializer( irb, *s_reg ).region_selector();
            check( decoder != decoders.end() );
            for ( auto [ name, keys ] : s_reg->tm() )
            {
                values_t conds;
                for ( auto key : keys )
                {
                    auto bstr = shadowinst::TM_t::make_bitstring( key );
                    auto c = llvm::APInt( static_cast< uint32_t >( bstr.size() ), bstr, 2 );
                    conds.emplace_back( irb.CreateICmpEQ( selector, irb.getInt( c ) ) );
                }

                auto selectors = irops::Or::make( irb, conds );
                out[ name ].emplace_back( irops::And::make( irb, { *decoder, selectors } ) );
            }
            ++decoder;
        }
        return out;
    }

    auto ExaltationContext::stores_to( llvm::Instruction *v ) -> stores_t
    {
        // Filter all stores
        std::vector< llvm::StoreInst * > stores;
        auto collect_stores = [ & ]( auto src, auto next ) -> void
        {
            for ( auto user : src->users() )
            {
                if ( auto store = llvm::dyn_cast< llvm::StoreInst >( user ) )
                    stores.push_back( store );
                if ( auto bc = llvm::dyn_cast< llvm::BitCastInst >( user ) )
                    next( bc, next );

                check( !llvm::isa< llvm::PtrToIntInst >( user ) &&
                       !llvm::isa< llvm::GetElementPtrInst >( user ) );
            }
        };
        collect_stores( v, collect_stores );
        return stores;
    }

    llvm::Value *ExaltationContext::last_store( const stores_t &stores )
    {
        // NOTE(lukas): It is expected that if there are multiple stores,
        //              Flattener component will make sure they are properly guarded
        //              wrt path condition.
        check( stores.size() >= 1, [ & ]{ return dbg_dump( stores ); } );

        // Next they are being ordered to determine which is last, therefore
        // they need to be in the same basic block
        auto bb = stores[ 0 ]->getParent();
        llvm::StoreInst *last = stores[ 0 ];

        for ( auto store : stores )
            if ( inst_distance( &*bb->begin(), store ) > inst_distance( &*bb->begin(), last ) )
                last = store;
        return last->getOperand( 0 );
    }

    auto ExaltationContext::reg_write_mux_operands( reg_ptr_t reg )
        -> gap::generator< llvm::Value * >
    {
        check( checks.count( reg ) );
        auto partial_checks = checks[ reg ];

        if ( partial_checks.empty() )
        {
            co_yield irops::input_reg( irb(), reg );
            co_return;
        }

        for ( auto [ unit_decoder, runtime ] : partial_checks )
            co_yield irops::Option::make( irb(), { runtime, unit_decoder }, ctx.bw( runtime ) );
    }


    llvm::Value *ExaltationContext::reg_check( reg_ptr_t reg )
    {
        llvm::Value *mux = irops::make< irops::Switch >( irb(), reg_write_mux_operands( reg ) );
        llvm::Value *out_reg = irops::output_reg( irb(), reg );
        return irops::OutputCheck::make( irb(), { mux, out_reg } );
    }

    void ExaltationContext::cse()
    {
        using entry_t = std::tuple< llvm::Value *, std::vector< llvm::Instruction * > >;
        std::unordered_map< llvm::Function *, entry_t > cache;

        for ( auto &inst : *( *circuit_fn ).begin() )
        {
            auto call = llvm::dyn_cast< llvm::CallInst >( &inst );
            if ( !call || call->arg_size() != 0 )
                continue;

            auto callee = call->getCalledFunction();
            if ( !cache.count( callee ) )
            {
                cache[ callee ] = std::make_tuple( call, std::vector< llvm::Instruction * >{} );
            } else {
                std::get< 1 >( cache[ callee ] ).push_back( call );
            }
        }

        log_dbg() << "[exalt:cse]:" << "Elimination.";
        for ( auto [ fn, e ] : cache )
        {
            const auto &[ keep, to_replace ] = e;
            log_dbg() << "[exalt:cse]:" << fn->getName().str()
                      << "has" << to_replace.size() << " calls to eliminate.";
            for ( auto v : to_replace )
            {
                v->replaceAllUsesWith( keep );
                v->eraseFromParent();
            }
        }

    }

    void ExaltationContext::finalize()
    {
        for ( auto reg : ctx.regs() )
            root.emplace_back( reg_check( reg ) );

        // Bump timestamp.
        auto [ ts_in, ts_out ] = irops::make_all_leaves< irops::Timestamp >( irb() );
        auto runtime_ts = irb().CreateAdd( ts_in, irb().getInt64( 1 ) );
        root.emplace_back( irops::OutputCheck::make( irb(), { runtime_ts, ts_out } ) );

        root.emplace_back( irops::Or::make( irb(), sub_root ) );

        auto result = irops::And::make( irb(), root );
        irb().CreateRet( result );


        auto exec = [ & ]( auto v ) { return remove_write( v ); };
        irops::AllocateDst::for_all_in( &*circuit_fn, exec );

        irops::Error::for_all_in( &*circuit_fn,
                                  []( auto c ) { c->eraseFromParent(); } );

        replace_remill_undefs( &*circuit_fn );

        // We firstly optimize, otherwise there would still be memory operations
        // and we would not be able to propagate undefs.
        optimize_silently( { &*circuit_fn } );
        propagate_remill_undefs( &*circuit_fn );

        cse();

        optimize_silently( { &*circuit_fn } );
    }

    /** State helpers **/
    void ExaltationContext::reset_state()
    {
        auto &irb = this->irb();
        return input().reset( irb, ctx.regs() );
    }

    void ExaltationContext::commit_state()
    {
        auto &irb = this->irb();
        return input().commit( irb, ctx );
    }

    void ExaltationContext::bump_pc( const values_t &decoders, unit_t &unit )
    {
        auto decoder = decoders.begin();

        std::vector< llvm::Value * > options;
        for ( auto &atom : unit )
        {
            auto inst_size = llvm::ConstantInt::get( ctx.word_type(), atom.encoding_size() );
            options.emplace_back( irops::Option::make( irb(), { inst_size, *( decoder++ ) },
                                                       ctx.bw( inst_size ) ) );
        }

        auto offset = irops::Switch::make( irb(), options );
        auto next_inst = irb().CreateAdd( input().load( irb(), ctx.pc_reg() ), offset  );
        input().store( irb(), ctx.pc_reg(), next_inst );
    }

}  // namespace circ
