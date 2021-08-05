/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

 #pragma once

#include <circuitous/Util/LLVMUtil.hpp>
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/ADT/iterator_range.h>
#pragma clang diagnostic pop

#include <cstdint>
#include <utility>

#include <circuitous/IR/IntrinsicsHelpers.hpp>

namespace circ::irops {

  #define sccc_prefix(what) static constexpr const char *fn_prefix = what
  #define dot_sep() static constexpr const char *separator = "."

  #define simple_intrinsic(what, code, attr) \
  namespace data { struct what { sccc_prefix(attr); dot_sep(); }; } \
  struct what : code< what, data::what > {}

  // N-ary operation, returns true iff exactly one operand is true
  simple_intrinsic(Xor, impl::predicate_base_t, "__circuitous.xor");
  // N-ary operation, returns true iff all operands are true
  simple_intrinsic(And, impl::predicate_base_t, "__circuitous.and");
  // N-ary operation, return true iff at least one operand is true
  simple_intrinsic(Or, impl::predicate_base_t, "__circuitous.or");
  // Same as `And`.
  simple_intrinsic(VerifyInst, impl::predicate_base_t, "__circuitous.verify_inst");

  // Binary operation, that returns true iff its operands are equal
  simple_intrinsic(Eq, impl::binary_check_t, "__circuitous.eq");
  // Same as `Eq`, serves to denote output comparison.
  simple_intrinsic(OutputCheck, impl::binary_check_t, "__circuitous.register_constraint");
  // Same as `Eq`, serves to denote constraint on value of Advice.
  simple_intrinsic(AdviceConstraint, impl::binary_check_t, "__circuitous.advice_constraint");
  // Same as `Eq`, serves to denote comparison with instruction bits.
  simple_intrinsic(DecodeCondition, impl::binary_check_t, "__circuitous.decode_condition");

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
  simple_intrinsic(Advice, impl::allocator_t, "__circuitous.advice");
  // Creates opaque pointer.
  // Used by instruction lifters to handle destination operands.
  simple_intrinsic(AllocateDst, impl::allocator_t, "__circuitous.allocate_dst");
  // Used by instrution lifters to create opaque values that serve as operands
  // to intrinsics. This is useful, so that the data flow in llvm can be disconnected
  // ```
  // %x = __circuitous.operand_advice.1.64()
  // %constraint = __circuitous.advice_constraint(%real_value, %x)
  // ```
  // this should allow llvm optimizations to eliminate more instructions.
  simple_intrinsic(Operand, impl::advice_allocator_t, "__circuitous.operand_advice");

  // Denotes that given hint/advice is not used and should be zeroed.
  simple_intrinsic(UnusedConstraint, impl::unary_check_t, "__circuitous.unused_constraint");
  // Anchor some part of code - usefull to keep track of code regions (e.g. all instructions
  // that were inserted by inlining a function call).
  simple_intrinsic(Breakpoint, impl::unary_check_t, "__circuitous.breakpoint");

  // Memory operation constraints.
  simple_intrinsic(ReadConstraint, impl::frozen_predicate_t, "__circuitous.memread");
  simple_intrinsic(WriteConstraint, impl::frozen_predicate_t, "__circuitous.memwrite");

  // I/O values. Not included in argument list to make their usage more comfortable.
  // TODO(lukas): Maybe it is worth to represent all registers this way as well.
  simple_intrinsic(ErrorBit, impl::ebit_t, "__circuitous.error_bit");
  simple_intrinsic(Timestamp, impl::timestamp_t, "__circuitous.timestamp");
  simple_intrinsic(InstBits, impl::instbit_t, "__circuitous.instbits");

  using io_type = impl::io_type;

  #undef sccc_prefix
  #undef dot_sep
  #undef simple_intrinsic

  // Create call to given intrinsic.
  // `args` are forwarded to the intrinsic creator
  // `c_args` are operands of the emitted llvm::CallInst.
  template< typename I, typename ...Args >
  auto make(llvm::IRBuilder<> &ir, const std::vector< llvm::Value * > &c_args, Args &&...args) {
    return I::make(ir, c_args, std::forward<Args>(args)...);
  }

  template< typename I, typename ...Args >
    auto make(llvm::IRBuilder<> &ir, llvm::Value *c_arg, Args &&...args) {
    CHECK(c_arg);
    return I::make(ir, {c_arg}, std::forward<Args>(args)...);
  }

  // See `make`, but without any `c_args`.
  template< typename I, typename ...Args >
  auto make_leaf(llvm::IRBuilder<> &ir, Args &&...args) {
    return I::make(ir, std::vector< llvm::Value * >{}, std::forward< Args >(args)...);
  }

  // Creates calls to all intrinsics that create `I` and returns them as tuple.
  template< typename I, typename ...Args >
  auto make_all_leaves(llvm::IRBuilder<> &ir, Args &&...args) {
    return I::make_all(ir, std::vector< llvm::Value * >{}, std::forward< Args >(args)...);
  }

  // Queries.
  template< typename T, typename ... Ts >
  bool one_of(llvm::Function *fn) {
    if constexpr (sizeof...(Ts) == 0) return T::is(fn);
    else return T::is(fn) || one_of< Ts ... >(fn);
  }

  static inline bool is_any(llvm::Function *fn) {
    return fn->hasName() && fn->getName().startswith("__circuitous.");
  }

  static inline bool is_any(llvm::CallInst *call) {
    if (!call || !call->getCalledFunction()) return false;
    return is_any(call->getCalledFunction());
  }

  template< typename T, typename ... Ts >
  void enable_opts(llvm::Module *m) {
    T::melt(m);
    if constexpr (sizeof ... (Ts) != 0) return enable_opts< Ts... >(m);
  }

  template< typename T, typename ...Ts >
  void disable_opts(llvm::Module *m) {
    T::freeze(m);
    if constexpr (sizeof ... (Ts) != 0) return disable_opts< Ts... >(m);
  }

  // Returns all values in range that are calling instrinsic `T`.
  template<typename T, typename R = llvm::iterator_range< llvm::BasicBlock::iterator > >
  auto collect(R range) {
    std::vector< llvm::CallInst * > out;
    for (auto &inst : range)
      if (auto call_inst = llvm::dyn_cast< llvm::CallInst >(&inst))
        if (T::is(call_inst->getCalledFunction()))
          out.push_back(call_inst);
    return out;
  }

  template< typename T >
  std::vector< llvm::CallInst * > collect(llvm::Value *from, llvm::Value *to) {
    CHECK(llvm::isa< llvm::Instruction >(from) && llvm::isa< llvm::Instruction >(to));
    using bb_t = llvm::BasicBlock::iterator;
    auto begin = bb_t{ llvm::cast< llvm::Instruction >(from) };
    auto end = bb_t{ llvm::cast< llvm::Instruction >(to) };
    return collect< T >({ begin, end });
  }

  template< typename I >
  struct Instance_ : I {

    llvm::Function *fn = nullptr;

    Instance_(llvm::Function *fn_) : fn(fn_) {}
    Instance_(llvm::CallInst *call) : fn((call) ? call->getCalledFunction() : nullptr) {}
    Instance_(llvm::Value *val) : Instance_(llvm::dyn_cast_or_null< llvm::CallInst >(val)) {}

    operator bool() { return fn && I::is(fn); }
  };

  // Helper class that can wraps already existing function.
  // Can be specialized.
  template< typename I >
  struct Instance : Instance_< I > { using Instance_< I >::Instance_; };

  template<>
  struct Instance< Memory > : Instance_< Memory > {
    using Instance_< Memory >::Instance_;

    uint32_t id() {
      CHECK(*this);
      return std::get< 0 >(Memory::parse_args< uint32_t >(fn));
    }
  };

  template<>
  struct Instance< Select > : Instance_< Select > {
    llvm::CallInst *call;

    Instance(llvm::CallInst *call_) : Instance_(call_), call(call_) {}

    llvm::Value *selector() {
      CHECK(*this);
      return call->getArgOperand(0);
    }

    bool is_complete() { return std::none_of(call->arg_begin(), call->arg_end(), is_undef); }

    // TODO(lukas): I think we should make this more general by introducing some
    //              way to configure the stride if arguments counts are different.
    static bool are_compatible(llvm::CallInst *lhs, llvm::CallInst *rhs) {
      auto size = std::min(lhs->getNumArgOperands(), rhs->getNumArgOperands());
      auto op = [&](auto from, auto i) {
        auto total = from->getNumArgOperands();
        auto idx = i + (i - 1) * ((total - 1) / (size - 1) - 1);
        return from->getArgOperand(idx);
      };
      for (uint32_t i = 1; i < size; ++i) {
        if (is_undef(op(lhs, i)) || is_undef(op(rhs, i))) {
          continue;
        }
        if (op(lhs, i) != op(rhs, i)) {
          return false;
        }
      }
      return true;
    }
  };

  // Replaces all uses of `gift` with its argument and removes it.
  template< typename I >
  static auto unwrap(llvm::Value *gift) {
    auto as_call = llvm::dyn_cast_or_null< llvm::CallInst >(gift);
    if (!as_call || !Instance_< I >(as_call))
      return gift;

    auto surprise = as_call->getArgOperand(0u);
    as_call->eraseFromParent();
    return surprise;
  }

  template< typename I >
  static auto unwrap(const std::vector< llvm::CallInst * > &gifts) {
    std::vector<llvm::Value *> out;
    for (auto x : gifts)
      out.push_back(unwrap< I >(x));
    return out;
  }


  namespace memory {

    // Layout of data inside Memory.
    template< typename V >
    struct Parsed {
      V used; // 1 bit
      V mode; // 1 bit
      // 6 bits reserved
      V id; // 4 bits
      V size; // 4 bits
      V addr; // 64 bits
      V value; // 64 bits
      V timestamp; // 64 buts

      bool operator==(const Parsed< V > &other) const = default;
    };

    // TODO(lukas): This two can probably be merged (easily) if reserved bits
    //              got their attribute.
    template< typename V, typename Inserter >
    static void construct(const Parsed< V > &parsed, Inserter &insert_) {
      auto current = 0u;
      auto exec = [&](auto elem, auto size) {
        insert_(elem, current, size);
        current += size;
      };

      exec(parsed.used, 1u);
      exec(parsed.mode, 1u);

      current += 6;

      exec(parsed.id, 4u);
      exec(parsed.size, 4u);
      exec(parsed.addr, 64u);
      exec(parsed.value, 64u);
      exec(parsed.timestamp, 64u);
    }

    template< typename V = llvm::Value *, typename Extractor >
    static Parsed< V > parse(V call, Extractor extract_) {
      auto current = 0u;
      auto extract = [&](auto size) -> V {
        auto out = extract_( call, current, size );
        current += size;
        return out;
      };

      Parsed< V > out;
      out.used = extract( 1u );
      out.mode = extract( 1u );
      std::ignore = extract( 6u );
      out.id = extract( 4u );
      out.size = extract( 4u );

      out.addr = extract( 64u  );
      out.value = extract( 64u );
      out.timestamp = extract( 64u );

      return out;
    }
  } // namespace memory

} // namespace circ::irops