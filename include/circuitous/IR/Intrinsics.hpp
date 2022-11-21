/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

 #pragma once

#include <circuitous/Support/Check.hpp>
#include <circuitous/Util/LLVMUtil.hpp>

#include <circuitous/Util/Warnings.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/AbstractCallSite.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/ADT/iterator_range.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <cstdint>
#include <utility>
#include <map>

#include <circuitous/IR/IntrinsicsHelpers.hpp>
#include <circuitous/IR/Memory.hpp>

#include <gap/core/generator.hpp>

namespace circ::irops
{

    #define sccc_prefix(what) static constexpr const char *fn_prefix = what
    #define dot_sep() static constexpr const char *delim = "."

    #define simple_intrinsic(what, code, attr) \
    namespace data { struct what { sccc_prefix(attr); dot_sep(); }; } \
    struct what : code< what, data::what > {}

    #define leaf_intrinsic(what, size, code, attr) \
    namespace data { struct what { sccc_prefix(attr); dot_sep(); }; } \
    struct what : code< data::what, size > {}

    // N-ary operation, returns true iff exactly one operand is true
    simple_intrinsic(Xor, impl::predicate_base_t, "__circuitous.xor");
    // N-ary operation, returns true iff all operands are true
    simple_intrinsic(And, impl::predicate_base_t, "__circuitous.and");
    // N-ary operation, return true iff at least one operand is true
    simple_intrinsic(Or, impl::predicate_base_t, "__circuitous.or");
    // Same as `And`.
    simple_intrinsic(VerifyInst, impl::predicate_base_t, "__circuitous.verify_inst");

    // Result of multiple decoder checks, semantically equivalent to `&&` on all its arguments.
    // Reason this is a separate intrinsic is that the node will be easily identifiable by
    // later analysis/transformation.
    simple_intrinsic(DecoderResult, impl::predicate_base_t, "__circuitous.decoder_result");

    // Binary operation, that returns true iff its operands are equal
    simple_intrinsic(Eq, impl::bin_predicate_t, "__circuitous.eq");
    // Same as `Eq`, serves to denote output comparison.
    simple_intrinsic(OutputCheck, impl::bin_predicate_t, "__circuitous.register_constraint");
    // Same as `Eq`, serves to denote constraint on value of Advice.
    simple_intrinsic(AdviceConstraint, impl::bin_predicate_t, "__circuitous.advice_constraint");
    // Same as `Eq`, serves to denote comparison with instruction bits.
    simple_intrinsic(DecodeCondition, impl::bin_predicate_t, "__circuitous.decode_condition");

    // Value of Error.
    simple_intrinsic(Error, impl::identity_t, "__circuitous.error");
    // Unary function used to wrap values so they are:
    //  * recognizable
    //  * not optimized away
    // Useful to communicate information between instruction lifter and circuit builder.
    simple_intrinsic(Transport, impl::identity_t, "__circuitous.transport");
    // See `Transport`.
    // Useful to hinder optimizations.
    simple_intrinsic(Identity, impl::identity_t, "__circuitous.identity");
    // Denotes that operand is operand of instruction that is immediate (usually either
    // constant or extract from instbits).
    simple_intrinsic(InputImmediate, impl::identity_t, "__circuitous.input_immediate");

    // Extract.X.Y() - extract [X, X + Y), from its operand.
    // If there is no operands, instbits are used instead.
    // Extracted bytes may be reordered to comply with architecture endianity.
    simple_intrinsic(Extract, impl::extract_t, "__circuitous.extract");
    // See `Extract`, without reordering.
    simple_intrinsic(ExtractRaw, impl::extract_t, "__circuitous.raw_extract");

    // Concats its operand, from right to left, e.g.
    // concat(x, y, z) -> xyz
    simple_intrinsic(Concat, impl::concat_t, "__circuitous.concat");
    // N-ary case of select, can be though of as a multiplexer
    // `select( iN selector, iX v0, iX, v1, ... , iX v(2^N - 1))`
    // There must be enough operands to satisfy all possible values of selector.
    // Returned value is that on position that is equal to runtime value of `selector`.
    simple_intrinsic(Select, impl::select_t, "__circuitous.select");

    // Creates Memory hint of fixed value. See `Parsed` for its layout.
    simple_intrinsic(Memory, impl::mem_allocator_t, "__circuitous.memory");
    // Create Advice of dynamic value.
    simple_intrinsic(Advice, impl::raw_allocator_t, "__circuitous.advice");
    simple_intrinsic(AdviceIndexed, impl::idx_allocator_t, "__circuitous.advice_i");
    simple_intrinsic(OpSelector, impl::idx_allocator_t, "__circuitous.op_selector");
    // Creates opaque pointer.
    // Used by instruction lifters to handle destination operands.
    simple_intrinsic(AllocateDst, impl::raw_allocator_t, "__circuitous.allocate_dst");
    // Used by instrution lifters to create opaque values that serve as operands
    // to intrinsics. This is useful, so that the data flow in llvm can be disconnected
    // ```
    // %x = __circuitous.operand_advice.1.64()
    // %constraint = __circuitous.advice_constraint(%real_value, %x)
    // ```
    // this should allow llvm optimizations to eliminate more instructions.
    simple_intrinsic(Operand, impl::type_idx_t, "__circuitous.operand_advice");

    // Helps to unify later all decoder related selections and operations
    simple_intrinsic(RegSelector, impl::type_idx_t, "__circuitous.reg_selector");

    simple_intrinsic(ISemSrcArg, impl::type_idx_t, "__circuitous.ise_src_arg");
    simple_intrinsic(ISemDstArg, impl::type_idx_t, "__circuitous.isem_dst_arg");
    simple_intrinsic(InstructionSize, impl::int_like_allocator, "__circuitous.inst_size");

    simple_intrinsic(WasDecoded, impl::type_idx_t, "__circuitous.op.was_decoded");
    // Denotes that given hint/advice is not used and should be zeroed.
    simple_intrinsic(UnusedConstraint, impl::unary_check_t, "__circuitous.unused_constraint");
    // Anchor some part of code - usefull to keep track of code regions (e.g. all instructions
    // that were inserted by inlining a function call).
    simple_intrinsic(Breakpoint, impl::identity_t, "__circuitous.breakpoint");

    // Memory operation constraints.
    simple_intrinsic(ReadConstraint, impl::frozen_predicate_t, "__circuitous.memread");
    simple_intrinsic(WriteConstraint, impl::frozen_predicate_t, "__circuitous.memwrite");

    // I/O values. Not included in argument list to make their usage more comfortable.
    // TODO(lukas): Maybe it is worth to represent all registers this way as well.
    leaf_intrinsic(ErrorBit, 1, impl::fixed_leaf_t, "__circuitous.error_bit");
    leaf_intrinsic(Timestamp, 64,  impl::fixed_leaf_t, "__circuitous.timestamp");
    leaf_intrinsic(InstBits, 15 * 8, impl::fixed_leaf_t, "__circuitous.instbits");

    simple_intrinsic(Reg, impl::reg_allocator_t, "__circuitous.reg");

    simple_intrinsic(Commit, impl::commit, "__circuitous.commit");

    simple_intrinsic(Entry, impl::bitcast, "__circuitous.entry");
    simple_intrinsic(Leave, impl::bitcast, "__circuitous.leave");


    simple_intrinsic(Switch, impl::concat_t, "__circuitous.switch");
    simple_intrinsic(Option, impl::option_t, "__circuitous.option");

    using io_type = impl::io_type;

    #undef sccc_prefix
    #undef dot_sep
    #undef simple_intrinsic
    #undef leaf_intrinsic

    // Create call to given intrinsic.
    // `args` are forwarded to the intrinsic creator
    // `c_args` are operands of the emitted llvm::CallInst.
    template< typename I, typename ...Args >
    auto make(llvm::IRBuilder<> &ir, const std::vector< llvm::Value * > &c_args, Args &&...args)
    {
        return I::emit(ir, c_args, std::forward<Args>(args)...);
    }

    template< typename I, typename ...Args >
    auto make(llvm::IRBuilder<> &ir, llvm::Value *c_arg, Args &&...args)
    {
        check(c_arg);
        return I::emit(ir, std::vector< llvm::Value * >{c_arg}, std::forward<Args>(args)...);
    }

    template< typename I, typename ... Args >
    auto make(llvm::IRBuilder<> &ir, gap::generator< llvm::Value * > cargs, Args && ... args)
    {
        std::vector< llvm::Value * > c;
        for ( auto a : cargs )
            c.push_back( a );
        return make< I >( ir, std::move( c ), std::forward< Args >( args ) ... );
    }

    // See `make`, but without any `c_args`.
    template< typename I, typename ...Args >
    auto make_leaf(llvm::IRBuilder<> &ir, Args &&...args)
    {
        return I::emit(ir, std::vector< llvm::Value * >{}, std::forward< Args >(args)...);
    }

    // Creates calls to all intrinsics that create `I` and returns them as tuple.
    template< typename I, typename ...Args >
    auto make_all_leaves(llvm::IRBuilder<> &ir, Args &&...args)
    {
        std::vector< llvm::Value * > empty;
        return std::make_tuple(I::emit(ir, empty, io_type::in, args ...),
                               I::emit(ir, empty, io_type::out, args ... ));
    }

    template< typename I > requires (std::is_same_v< I, Select >)
    auto make(llvm::IRBuilder<> &irb, const std::vector< llvm::Value * > &c_args)
    {
        auto selector_size = impl::uniform_size(c_args.begin(), std::next(c_args.begin()));
        auto ret_size = impl::uniform_size(std::next(c_args.begin()), c_args.end());
        check(selector_size && ret_size);
        return I::emit(irb, c_args, *ret_size, *selector_size);
    }

    // Return an integral type that is big enough to hold any value can inhabit the
    // register associated with `reg`.
    static inline llvm::IntegerType *int_reg_type( llvm::Module &module,
                                                           const auto *reg )
    {
        // TODO(pag): Add other architecture flag names here.
        static const std::unordered_set< std::string > flag_regs =
        {
            "SF", "OF", "PF", "AF", "ZF", "CF"
        };

        if ( reg->type->isIntegerTy() )
        {
            if ( flag_regs.count( reg->name ) )
                return llvm::Type::getInt1Ty( module.getContext() );

            return llvm::dyn_cast< llvm::IntegerType >( reg->type );
        }

        return llvm::Type::getIntNTy(
            module.getContext(),
            static_cast< unsigned >(
                module.getDataLayout().getTypeAllocSize( reg->type ) * 8u ) );
    }

    static inline llvm::Value *mk_reg( llvm::IRBuilder<> &irb, const auto &reg, auto io )
    {
        auto &m = *irb.GetInsertBlock()->getParent()->getParent();
        auto type = int_reg_type( m, reg );
        return irops::make_leaf< irops::Reg >( irb, bw( m, type ), reg->name, io );
    }

    static inline llvm::Value *input_reg( llvm::IRBuilder<> &irb, const auto &reg )
    {
        return mk_reg( irb, reg, irops::io_type::in );
    }

    static inline llvm::Value *output_reg( llvm::IRBuilder<> &irb, const auto &reg )
    {
        return mk_reg( irb, reg, irops::io_type::out );
    }


    // Queries.
    template< typename T, typename ... Ts >
    bool one_of(llvm::Function *fn)
    {
        if constexpr (sizeof...(Ts) == 0) return T::is(fn);
        else return T::is(fn) || one_of< Ts ... >(fn);
    }

    static inline bool is_any(llvm::Function *fn)
    {
        return fn->hasName() && fn->getName().startswith("__circuitous.");
    }

    static inline bool is_any(llvm::CallInst *call)
    {
        if (!call || !call->getCalledFunction()) return false;
        return is_any(call->getCalledFunction());
    }

    template< typename T, typename ... Ts >
    void enable_opts(llvm::Module *m)
    {
        T::melt(m);
        if constexpr (sizeof ... (Ts) != 0) return enable_opts< Ts... >(m);
    }

    template< typename T, typename ...Ts >
    void disable_opts(llvm::Module *m)
    {
        T::freeze(m);
        if constexpr (sizeof ... (Ts) != 0) return disable_opts< Ts... >(m);
    }

    // Returns all values in range that are calling instrinsic `T`.
    template<typename T, typename R = llvm::iterator_range< llvm::BasicBlock::iterator > >
    auto collect(R range)
    {
        std::vector< llvm::CallInst * > out;
        for (auto &inst : range)
            if (auto call_inst = llvm::dyn_cast< llvm::CallInst >(&inst))
                if (T::is(call_inst->getCalledFunction()))
                    out.push_back(call_inst);
        return out;
    }

    template< typename T >
    std::vector< llvm::CallInst * > collect(llvm::Value *from, llvm::Value *to)
    {
        check(llvm::isa< llvm::Instruction >(from) && llvm::isa< llvm::Instruction >(to));
        using bb_t = llvm::BasicBlock::iterator;
        auto begin = bb_t{ llvm::cast< llvm::Instruction >(from) };
        auto end = bb_t{ llvm::cast< llvm::Instruction >(to) };
        return collect< T >({ begin, end });
    }

    template< typename I >
    struct Instance_ : I
    {
        llvm::Function *fn = nullptr;

        Instance_(llvm::Function *fn_) : fn(fn_) {}
        Instance_(llvm::CallInst *call) : fn((call) ? call->getCalledFunction() : nullptr) {}
        Instance_(llvm::Value *val)
            : Instance_(llvm::dyn_cast_or_null< llvm::CallInst >(val))
        {}

        operator bool() const { return fn && I::is(fn); }
    };

    // Helper class that can wraps already existing function.
    // Can be specialized.
    template< typename I >
    struct Instance : Instance_< I > { using Instance_< I >::Instance_; };

    template<>
    struct Instance< Memory > : Instance_< Memory >
    {
        using Instance_< Memory >::Instance_;

        std::size_t id()
        {
            check(*this);
            return std::get< 1 >(Memory::parse_args(fn));
        }
    };

    template<>
    struct Instance< AdviceConstraint > : Instance_< AdviceConstraint >
    {
        llvm::CallInst *call;

        Instance(llvm::CallInst *call_) : Instance_(call_), call(call_) {}

        llvm::Value *advice() { check(*this); return call->getArgOperand(1); }
        llvm::Value *dynamic() { check(*this); return call->getArgOperand(0); }

        void set_dynamic(llvm::Value *v)
        {
            check(*this);
            call->setArgOperand(1, v);
        }
    };

    template<>
    struct Instance< Select > : Instance_< Select >
    {
        llvm::CallInst *call;

        Instance(llvm::CallInst *call_) : Instance_(call_), call(call_) {}

        llvm::Value *selector()
        {
            check(*this);
            return call->getArgOperand(0);
        }

        bool is_complete()
        {
            return std::none_of(call->arg_begin(), call->arg_end(), is_undef);
        }

        // TODO(lukas): I think we should make this more general by introducing some
        //              way to configure the stride if arguments counts are different.
        // Returns `true` is `lhs` is compatible (i.e. is the same or can be extended
        // with padding and undefs) to match `rhs`.
        static bool is_compatible_with(llvm::CallInst *lhs, llvm::CallInst *rhs)
        {
            if (lhs->arg_size() > rhs->arg_size())
                return false;

            auto size = std::min(lhs->arg_size(), rhs->arg_size());
            auto op = [&](auto from, auto i)
            {
                auto total = from->arg_size();
                auto idx = i + (i - 1) * ((total - 1) / (size - 1) - 1);
                return from->getArgOperand(idx);
            };

            for (uint32_t i = 1; i < size; ++i)
            {
              if (is_undef(op(lhs, i)) || is_undef(op(rhs, i)))
                continue;

              if (op(lhs, i) != op(rhs, i))
                return false;
            }
            return true;
        }

        // Returns `true` if `lhs` is compatible with `rhs` or vice versa.
        static bool are_compatible(llvm::CallInst *lhs, llvm::CallInst *rhs)
        {
            return is_compatible_with(lhs, rhs) || is_compatible_with(rhs, lhs);
        }
    };

    // Replaces all uses of `gift` with its argument and removes it.
    template< typename I >
    static auto unwrap(llvm::Value *gift)
    {
        auto as_call = llvm::dyn_cast_or_null< llvm::CallInst >(gift);
        if (!as_call || !Instance_< I >(as_call))
            return gift;

        auto surprise = as_call->getArgOperand(0u);
        as_call->eraseFromParent();
        return surprise;
    }

    template< typename I >
    static auto unwrap(const std::vector< llvm::CallInst * > &gifts)
    {
        std::vector<llvm::Value *> out;
        for (auto x : gifts)
            out.push_back(unwrap< I >(x));
        return out;
    }

    template< typename S, typename P >
    auto make_eqs(llvm::IRBuilder<> &irb, const std::map< llvm::Value *, S > &vals, P &&promote)
    {
        std::vector< llvm::Value * > c_args;
        for (const auto &[fst, snd] : vals)
            c_args.push_back(irb.CreateICmpEQ(fst, promote(snd)));
        return c_args;
    }

    template< typename S, typename P >
    auto make_eqs(llvm::IRBuilder<> &irb, llvm::Value *fst, const S &vals, P &&promote)
    {
        std::vector< llvm::Value * > c_args;
        for (const auto &snd : vals)
            c_args.push_back(irb.CreateICmpEQ(fst, promote(snd)));
        return c_args;
    }

    template< typename T >
    bool is(llvm::CallInst *call) { return call && static_cast< bool >(Instance< T >(call)); }

    template< typename T >
    bool is(llvm::Value *v) { return is< T >(llvm::dyn_cast_or_null< llvm::CallInst >(v)); }

    template< typename T >
    requires std::is_same_v< typename std::remove_cvref_t< T >::value_type, std::string >
    auto is_one_of(llvm::IRBuilder<> &irb, llvm::Value *fst, T &&vals)
    {
        auto promote = [&](auto bstr) {
            return irb.getInt(llvm::APInt(static_cast< uint32_t >(bstr.size()), bstr, 2));
        };
        return make< Or >(irb, make_eqs(irb, fst, std::forward< T >(vals), promote));
    }

} // namespace circ::irops
