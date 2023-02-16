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

    struct State
    {
        using reg_ptr_t = const remill::Register *;

        llvm::Value *state = nullptr;

        State(llvm::BasicBlock *where, llvm::Type *type)
            : state(llvm::IRBuilder<>(where).CreateAlloca(type))
        {}

        auto raw() { return state; }
        auto type() { return state->getType(); }

        void store(llvm::IRBuilder<> &ir, const reg_ptr_t where, llvm::Value *what);
        llvm::Value *load(llvm::IRBuilder<> &ir, const reg_ptr_t where);

        void reset( llvm::IRBuilder<> &irb, const Ctx::regs_t &regs );
        void commit( llvm::IRBuilder<> &irb, CtxRef ctx );

        llvm::Value *operator*() const { return state; }
        llvm::Value *operator*() { return state; }
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

    struct CircuitMaker_v2 : has_ctx_ref
    {
        // Simple structure to keep track of all links and information
        // with lifted isems.
        struct ISemInstance
        {
            const isem::ISem *def;
            llvm::Instruction *inst_size;
            std::map< Ctx::reg_ptr_t, llvm::Value * > computationals;

            explicit ISemInstance( const isem::ISem *def, llvm::Instruction *inst_size )
                : def( def ), inst_size( inst_size )
            {}
        };

        struct Context
        {
            std::vector< llvm::Value * > _args;

            void add( llvm::Value *v ) { _args.push_back( v ); }

            template< gap::ranges::range R >
            void add( R &&r )
            {
                for ( auto x : r )
                    add( x );
            }

            llvm::Value *materialize( llvm::IRBuilder<> &irb );
        };

        isem::ISemBank isems = isem::ISemBank( this->ctx );

        llvm::Function *fn;
        std::optional< State > state;

        // One instance can be used by multiple isems.
        using instance_ptr_t = std::shared_ptr< ISemInstance >;
        std::unordered_map< const isem::ISem *, instance_ptr_t > def_to_instances;
        std::unordered_map< const InstructionInfo *, instance_ptr_t > info_to_instance;

        CircuitMaker_v2( CtxRef ctx_ref )
            : has_ctx_ref( ctx_ref )
        {
            init_function();
        }

        // Sets `fn`, `state`. Creates a function with a basic block that contains
        // `State` properly initialized to initial values.
        void init_function();

        llvm::Function *make_from( const InstructionBatch &batch );

        llvm::IRBuilder<> mk_irb() { return llvm::IRBuilder<>( &*fn->begin() ); }

        // State helpers
        void reset_state();
        void commit_state();

        // ISem  operations
        instance_ptr_t materialize( const isem::ISem * );
        void computationals( const isem::ISem * );

        instance_ptr_t instance_of( const isem::ISem *def )
        {
            auto it = def_to_instances.find( def );
            check( it != def_to_instances.end() );
            return it->second;
        }

        instance_ptr_t instance_of( const InstructionInfo &info )
        {
            auto it = info_to_instance.find( &info );
            check( it != info_to_instance.end() );
            return it->second;
        }
    };


}  // namespace circ
