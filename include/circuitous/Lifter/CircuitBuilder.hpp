/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Lifter/BaseLifter.hpp>
#include <circuitous/Lifter/Component.hpp>
#include <circuitous/Lifter/BaseLifter.hpp>
#include <circuitous/Lifter/Lifter.hpp>
#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/IntrinsicTable.h>
#include <remill/BC/Lifter.h>
#include <remill/BC/Optimizer.h>
#include <remill/BC/Util.h>

#include <iomanip>
#include <unordered_map>
#include <vector>

namespace circ
{
    // Forward declare
    struct InstructionBatch;

    using builder_ref = llvm::IRBuilder<> &;

    namespace isem
    {
        struct ISem;
    }

    struct PostLiftOpt
    {
        static llvm::Function *crop_returns(llvm::Function *fn);
        static llvm::Function *merge_ctxs(llvm::Function *fn);
        static llvm::Function *remove_unused(llvm::Function *fn);

        static auto run(llvm::Function *fn)
        {
            return remove_unused(crop_returns(merge_ctxs(fn)));
        }
    };

    llvm::Value *input_reg( llvm::IRBuilder<> &irb, const Ctx::reg_ptr_t &reg );
    llvm::Value *output_reg( llvm::IRBuilder<> &irb, const Ctx::reg_ptr_t &reg );

    static inline llvm::CallInst *call_semantic(
            llvm::IRBuilder<> &ir,
            llvm::Function *fn,
            llvm::Value *s, llvm::Value *pc, llvm::Value *m)
    {
        llvm::Value *inst_func_args[remill::kNumBlockArgs] = {};
        inst_func_args[remill::kPCArgNum] = pc;
        inst_func_args[remill::kMemoryPointerArgNum] = m;
        inst_func_args[remill::kStatePointerArgNum] = s;
        return ir.CreateCall(fn, inst_func_args);
    }

    static inline llvm::CallInst *call_semantic(
            llvm::IRBuilder<> &ir,
            llvm::Function *fn,
            llvm::Value *s, llvm::Value *m,
            std::vector< llvm::Value * > args)
    {
        std::vector< llvm::Value * > inst_func_args;
        inst_func_args.emplace_back( s );
        inst_func_args.emplace_back( m );

        inst_func_args.insert( inst_func_args.end(), args.begin(), args.end() );

        return ir.CreateCall(fn, inst_func_args);
    }
    struct wraps_remill_value
    {
      protected:
        llvm::Value *storage = nullptr;

      public:

        wraps_remill_value() = delete;
        wraps_remill_value( llvm::Value *storage ) : storage( storage ) {}

        wraps_remill_value( llvm::BasicBlock *where, llvm::Type *t )
            : storage( llvm::IRBuilder<>( where ).CreateAlloca( t ) )
        {}

        wraps_remill_value( llvm::Function *fn, llvm::Type *t );

        llvm::Value *operator->() const { return storage; }
        llvm::Value *operator->() { return storage; }

        llvm::Value *operator*() const { return storage; }
        llvm::Value *operator*() { return storage; }
    };

    struct State : wraps_remill_value
    {
        using reg_ptr_t = const remill::Register *;

        using wraps_remill_value::wraps_remill_value;

        void store(llvm::IRBuilder<> &ir, const reg_ptr_t where, llvm::Value *what);
        llvm::Value *load(llvm::IRBuilder<> &ir, const reg_ptr_t where);

        void reset( llvm::IRBuilder<> &irb, const Ctx::regs_t &regs );
        void commit( llvm::IRBuilder<> &irb, CtxRef ctx );
    };

    struct MemoryPtr : wraps_remill_value
    {
        MemoryPtr( llvm::Type *t )
            : wraps_remill_value( llvm::UndefValue::get( t ) )
        {}
    };

    struct Trace
    {
        CtxRef ctx;
        std::vector< State > storage;

        Trace( CtxRef ctx, State input, State output )
            : ctx( ctx ),
              storage{ input, output }
        {}

        void reset( builder_ref bld, const Ctx::regs_t &regs )
        {
            for ( auto &state : storage )
                state.reset( bld, regs );
        }

        void commit( builder_ref bld )
        {
            for ( auto &state : storage )
                state.commit( bld, ctx );
        }

        auto &input() { return storage[ 0 ]; }
        auto &output() { return storage[ 1 ]; }
    };


    struct CircuitFunction : has_ctx_ref
    {
        llvm::Function *circuit_fn = nullptr;

        llvm::BasicBlock *entry = nullptr;
        llvm::BasicBlock *start = nullptr;
        llvm::BasicBlock *head = nullptr;
        llvm::BasicBlock *exit = nullptr;

        using cr_reg = const remill::Register *;
        // [remill reg, in, out]
        using arg_map_t = std::vector<
            std::tuple< cr_reg, llvm::Argument *, llvm::Argument * > >;

        arg_map_t arg_map;

        CircuitFunction(CtxRef ctx_, const std::string &name) : has_ctx_ref(ctx_)
        {
            circuit_fn = _make_fn(name);
            _make_body();
        }

        llvm::Function *_make_fn(const std::string &name);

        void _make_body()
        {
            log_dbg() << "CircuitFunction::_make_body";
            check(circuit_fn && circuit_fn->isDeclaration());

            entry = llvm::BasicBlock::Create(*ctx.llvm_ctx(), "entry", circuit_fn);
            head = llvm::BasicBlock::Create(*ctx.llvm_ctx(), "", circuit_fn);
            start = head;
            exit = llvm::BasicBlock::Create(*ctx.llvm_ctx(), "exit", circuit_fn);
            log_dbg() << "\tBasic blocks created.";
        }

        void move_head()
        {
            auto next = llvm::BasicBlock::Create(*ctx.llvm_ctx(), "", circuit_fn);
            llvm::IRBuilder<>(head).CreateBr(next);
            head = next;
        }

        void tie_head() { llvm::IRBuilder<>(head).CreateBr(exit); }
        void tie_entry() { llvm::IRBuilder<>(entry).CreateBr(start); }
        void tie_exit(llvm::Value *v)
        {
            llvm::IRBuilder<>(exit).CreateRet(v);
        }

        auto pc()
        {
            for (auto &[reg, arg, _] : arg_map)
                if (reg->name == ctx.arch()->ProgramCounterRegisterName())
                    return arg;
            unreachable() << "Could not locate input pc register.";
        }

        void inspect_corpse()
        {
            circuit_fn->print(llvm::errs());
            llvm::errs().flush();
            unreachable() << "Corpse inspection";
        }

        using maybe_str = std::optional< std::string >;
        static maybe_str is_output_reg(llvm::Argument *arg);
        static maybe_str is_input_reg(llvm::Argument *arg);

        template< uint64_t Idx >
        llvm::Value *_locate_reg(const std::string &name)
        {
            for (const auto &arg_entry : arg_map)
            {
                auto reg = std::get< 0 >(arg_entry);
                auto of_interest = std::get< Idx >(arg_entry);
                if (enclosing_reg(ctx.arch(), name) == reg)
                    return of_interest;
            }
            unreachable() << "Did not locate at Idx: " << Idx << " reg named: " << name;
        }

        llvm::Value *locate_out_reg(const std::string &name) { return _locate_reg< 2 >(name); }
        llvm::Value *locate_in_reg(const std::string &name) { return _locate_reg< 1 >(name); }
    };

    struct circuit_builder : CircuitFunction
    {
        using parent_t = CircuitFunction;

        using isels_t  = std::vector< InstructionSelection >;
        using values_t = std::vector< llvm::Value * >;
        using instructions_t = std::vector< llvm::Instruction * >;

        using parent_t::parent_t;

        llvm::Value *saturation_prop;
        llvm::Value *timestamp_prop;

        using context_t = component::Context;
        std::vector< context_t > ctxs;

        std::unordered_map< std::string, llvm::Value * > default_rcs;

        circuit_builder(CtxRef ctx_, const std::string &name) : CircuitFunction(ctx_, name)
        {
            build_general_props();
            build_default_rcs();
        }

        void build_general_props()
        {
            using namespace component;
            std::tie(saturation_prop, timestamp_prop) =
                construct< SaturationProp, TimestampProp >(this->entry);
        }

        void build_default_rcs()
        {
            llvm::IRBuilder<> irb(this->entry);
            for (auto &[reg, in, out] : this->arg_map)
                default_rcs[reg->name] = irops::make< irops::OutputCheck >(irb, {in, out});
        }

        void inject(const InstructionBatch &batch);
        void inject(const InstructionInfo &batch);

        // TODO(lukas): Fix and replace the old way of lifting.
        void inject_semantic_modular(ISEL_view isel);
        llvm::Function *finish();
        void handle_undefs();
        // `fn_name` is intrinsic name that generates undef values, which for now
        // means members of `__remill_undefined_*` family.
        void handle_undef(const std::string &fn_name);

        // TODO(lukas): Type alias return type.
        std::tuple< values_t, llvm::Value * > handle_errors(llvm::Value *begin,
                                                            llvm::Value *end);
        llvm::Value *emit_error_transitions(llvm::Value *current_ebit);
        instructions_t lower_dst_regs(const values_t & dtst);

        values_t handle_dst_regs(llvm::Value *c_ebit, instructions_t &dst_regs,
                                 ISEL_view isel, State &state);

        using cond_val_tuple = std::tuple< llvm::Value *, llvm::Value * >;
        cond_val_tuple handle_dst_reg(llvm::Instruction *dst_reg,
                                      const shadowinst::Reg &s_reg, State &state,
                                      std::size_t reg_idx);

        cond_val_tuple handle_dst_regs_(std::vector< llvm::Instruction * > &dst_regs,
                                        ISEL_view isel, State &state);

        llvm::Value *emit_preserved_checks(instructions_t &dst_regs,
                                           ISEL_view &isel,
                                           State &state);

        void propagate_undefs();

        void add_isel_metadata(llvm::Instruction *call, ISEL_view isel)
        {
            std::stringstream ss;
            for (auto byte : isel.instruction.bytes) {
                ss << " " << std::setw(2) << std::setfill('0') << std::hex
                   << static_cast<unsigned>(static_cast<uint8_t>(byte));
            }
            annotate_llvm(call, circir_llvm_meta::lifted_bytes, ss.str());
        }
    };

    struct CircuitMaker
    {
        using InstSelections = std::vector<InstructionSelection>;

      private:

        CtxRef ctx;

        // TODO(lukas): Is this even needed anymore?
        void prepare_module();

      public:
        explicit CircuitMaker(CtxRef ctx_) : ctx(ctx_) {}

        // TODO(lukas): Is this still neeeded?
        // Looks for calls to a function like `__remill_error`, and
        // replace its state pointer with a null pointer so that the state
        // pointer never escapes.
        void mute_state_escape(const std::string &func_name)
        {
            auto func = ctx.module()->getFunction(func_name);
            if (!func)
              return;

            for (auto user : func->users()) {
                if (auto call_inst = llvm::dyn_cast< llvm::CallInst >(user))
                {
                    auto arg_op = call_inst->getArgOperand(remill::kStatePointerArgNum);
                    call_inst->setArgOperand(remill::kStatePointerArgNum,
                                             llvm::UndefValue::get(arg_op->getType()));
                }
            }
        }

        llvm::Function *make_from(const InstructionBatch &batch);
    };

    struct CircuitFunction_v2
    {
        llvm::Function &fn;
        Trace trace;
        MemoryPtr memory_ptr;

        llvm::IRBuilder<> irb_instance;

        CircuitFunction_v2( Ctx &ctx );

        static llvm::Function &mk_function( Ctx &ctx );

        auto &operator*() { return fn; }
        const auto &operator*() const { return fn; }

        llvm::IRBuilder<> &irb()
        {
            return irb_instance;
        }
    };

    struct ExaltationContext
    {
        using values_t = std::vector< llvm::Value * >;
        using unit_t = Unit< Atom >;
        using builder_t = llvm::IRBuilder<>;
        using reg_ptr_t = typename Ctx::reg_ptr_t;

        Ctx &ctx;
        CircuitFunction_v2 &circuit_fn;

        std::vector< llvm::Value * > sub_root;
        std::vector< llvm::Value * > root;

        // ( reg, ( decoder_cond, value ) ]
        std::map< reg_ptr_t, std::vector< std::tuple< llvm::Value *, llvm::Value * > > > checks;

        ExaltationContext( Ctx &ctx, CircuitFunction_v2 &circuit_fn )
            : ctx( ctx ), circuit_fn( circuit_fn )
        {}

        /* Utilities */

        auto &irb() { return circuit_fn.irb(); }

        auto &input() { return circuit_fn.trace.input(); }
        auto &output() { return circuit_fn.trace.output(); }

        auto &memory_ptr() { return circuit_fn.memory_ptr; }

        auto pc() { return irops::mk_reg( irb(), ctx.pc_reg(), irops::io_type::in ); }

        /* State helpers */

        void reset_state();
        void commit_state();

        /* Actual lifting */

        void finalize();
        llvm::Value *reg_check( reg_ptr_t );
        auto reg_write_mux_operands( reg_ptr_t ptr ) -> gap::generator< llvm::Value * >;

        void remove_write( llvm::Value * );

        void exalt( unit_t &unit );
        values_t synthetize_decoders( unit_t &unit );
        void bump_pc( const values_t &decoders, unit_t &unit );

        using reg_to_vals = std::unordered_map< std::string, values_t >;
        reg_to_vals written_condition( const values_t &decoders,
                                       unit_t &unit, std::size_t idx );

        using stores_t = std::vector< llvm::StoreInst * >;
        stores_t stores_to( llvm::Instruction *v );
        llvm::Value *last_store( const stores_t &stores );
    };

}  // namespace circ
