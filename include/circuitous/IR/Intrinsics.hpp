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

namespace circ::intrinsics {

/* Typically with each intrinsic we want to do a fixed set of operations:
 *  - Create
 *  - Encode something in the name
 *  - Check if function is an instance
 *  - ...
 * This headers tries to hide implementation of these operations so that
 * builder and other users can use it as a black-box.
 */

namespace impl {

  // NOTE(lukas): This actually may not be needed here, as almost everything
  //              is `static`.
  template<typename Self_t, template<typename ...> class Derived>
  struct CRTP {
    Self_t &Self() { return static_cast<Self_t &>(*this); }
    const Self_t &Self() const { return static_cast<const Self_t &>(*this); }
  };

  template<typename Self_t>
  struct Base : CRTP<Self_t, Base> {
    static bool IsIntrinsic(llvm::Function *fn) {
      if (!fn->hasName() || !fn->isDeclaration()) {
        return false;
      }
      return fn->getName().startswith(Self_t::fn_prefix);
    }

    static std::vector<llvm::Function *> All(llvm::Module *module) {
      std::vector<llvm::Function *> out;
      for (auto &fn : *module) {
        if (IsIntrinsic(&fn)) {
          out.push_back(&fn);
        }
      }
      return out;
    }

    template<typename CB>
    static void ForAllIn(llvm::Function *trg, CB &&cb) {
      auto module = trg->getParent();
      std::vector<llvm::CallInst *> call_insts;
      for (auto fn : All(module)) {
        for (auto user : fn->users()) {
          if (auto call = llvm::dyn_cast<llvm::CallInst>(user);
              call && call->getParent()->getParent() == trg) {
            call_insts.push_back(call);
          }
        }
      }
      for (auto call: call_insts) {
        cb(call);
      }
    }

    static void freeze(llvm::Module *module) {
      for (auto fn : All(module)) {
        freeze(fn);
      }
    }

    static llvm::Function* freeze(llvm::Function *fn) {
      fn->addFnAttr(llvm::Attribute::NoMerge);
      fn->addFnAttr(llvm::Attribute::OptimizeNone);
      fn->addFnAttr(llvm::Attribute::NoInline);
      fn->removeFnAttr(llvm::Attribute::ReadNone);
      fn->setLinkage(llvm::GlobalValue::ExternalLinkage);
      return fn;
    }

    static void melt(llvm::Module *module) {
      for (auto fn : All(module)) {
        melt(fn);
      }
    }

    static llvm::Function* melt(llvm::Function *fn) {
      fn->removeFnAttr(llvm::Attribute::NoMerge);
      fn->removeFnAttr(llvm::Attribute::OptimizeNone);
      fn->removeFnAttr(llvm::Attribute::NoInline);
      fn->addFnAttr(llvm::Attribute::ReadNone);
      fn->setLinkage(llvm::GlobalValue::ExternalLinkage);
      return fn;
    }
  };

  // Parses `C` following the enc of the `fn_prefix`. Expects numbers
  // separated by `.`.
  template<typename Self_t, uint8_t C>
  struct ParseInts : Base<Self_t> {
    using Parent = Base<Self_t>;

    template<typename I=uint64_t>
    static auto ParseArgs(llvm::Function *fn) {
      CHECK(Parent::IsIntrinsic(fn))
        << "Cannot parse arguments of function: "
        << LLVMName(fn)
        << "that is not our intrinsic.";

      auto as_I = [](auto &str_ref) {
        uint64_t out;
        str_ref.getAsInteger(10, out);
        return static_cast<I>(out);
      };

      llvm::StringRef name = fn->getName();
      name.consume_front(Self_t::fn_prefix);
      name.consume_front(Self_t::separator);
      return Consume<C>(name, as_I);
    }

    template<uint8_t L, typename Convert>
    static auto Consume(llvm::StringRef str, Convert fn) {
      static_assert(L > 0);
      if constexpr (L == 1) {
        return std::make_tuple(fn(str));
      } else {
        const auto &[from, size] = str.split(Self_t::separator);
        return std::tuple_cat(std::make_tuple(fn(from)), Consume<L - 1>(size, fn));
      }
    }

    using intrinsic_args_t = decltype(ParseArgs<uint64_t>(nullptr));
  };

  template<typename Self_t>
  struct Interval : ParseInts<Self_t, 2> {
    using Parent = ParseInts<Self_t, 2>;

    static std::string Name(uint64_t from, uint64_t size) {
      std::stringstream ss;
      ss << Self_t::fn_prefix << Self_t::separator
         << from << Self_t::separator << size;
      return ss.str();
    }

    static llvm::Function *CreateFn(llvm::Module *module, uint64_t from, uint64_t size) {
      llvm::IRBuilder<> ir(module->getContext());
      auto r_ty = ir.getIntNTy(static_cast<uint32_t>(size));
      auto fn_t = llvm::FunctionType::get(r_ty, {}, true);
      auto callee = module->getOrInsertFunction(Name(from, size), fn_t);
      return Parent::melt(llvm::cast<llvm::Function>(callee.getCallee()));
    }
  };

  template<typename Self_t>
  struct Predicate : Base<Self_t> {
    using Parent = Base<Self_t>;

    static std::string Name() {
      return Self_t::fn_prefix;
    }

    static llvm::Function *CreateFn(llvm::Module *module) {
      llvm::IRBuilder<> ir(module->getContext());
      auto r_ty = ir.getInt1Ty();
      auto fn_t = llvm::FunctionType::get(r_ty, {}, true);
      auto callee = module->getOrInsertFunction(Name(), fn_t);
      return Parent::melt(llvm::cast<llvm::Function>(callee.getCallee()));
    }
  };

  // On contrast with `Predicate` it expects 2 runtime arguments of the same
  // types and returns bool. Size of arguments is encoded in the name.
  template<typename Self_t>
  struct BinaryPredicate : ParseInts<Self_t, 1> {
    using Parent = ParseInts<Self_t, 1>;

    static std::string Name(uint64_t size) {
      std::stringstream ss;
      ss << Self_t::fn_prefix << Self_t::separator << size;
      return ss.str();
    }

    static llvm::Function *CreateFn(llvm::Module *module, llvm::Type *type) {
      auto size = llvm::cast<llvm::IntegerType>(type)->getScalarSizeInBits();
      return CreateFn(module, size);
    }

    static llvm::Function *CreateFn(llvm::Module *module, uint64_t size) {
      llvm::IRBuilder<> ir(module->getContext());
      auto r_ty = ir.getInt1Ty();
      auto arg_ty = ir.getIntNTy(static_cast<uint32_t>(size));
      auto fn_t = llvm::FunctionType::get(r_ty, {arg_ty, arg_ty}, false);
      auto callee = module->getOrInsertFunction(Name(size), fn_t);
      return Parent::melt(llvm::cast<llvm::Function>(callee.getCallee()));
    }

    static_assert(std::is_same_v<typename Parent::intrinsic_args_t, std::tuple<uint64_t>>);
  };

  template<typename Self_t, template<typename ...> class Derived_>
  struct EncodeSize : ParseInts<Self_t, 1> {
    using Parent = ParseInts<Self_t, 1>;
    using Derived = Derived_<Self_t>;

    static std::string Name(uint64_t size) {
      std::stringstream ss;
      ss << Self_t::fn_prefix << Self_t::separator << size;
      return ss.str();
    }

    static llvm::Function *CreateFn(llvm::Module *module, uint64_t size) {
      llvm::IRBuilder<> ir(module->getContext());
      auto ret_ty = Derived::ResultType(module, size);
      auto args_ty = Derived::ArgTypes(module, size);
      auto fn_t = llvm::FunctionType::get(ret_ty, args_ty, Derived::is_vararg);
      auto callee = module->getOrInsertFunction(Name(size), fn_t);
      return Parent::melt(llvm::cast<llvm::Function>(callee.getCallee()));
    }

    static_assert(std::is_same_v<typename Parent::intrinsic_args_t, std::tuple<uint64_t>>);
  };

  template<typename Self_t>
  struct Identity : EncodeSize<Self_t, Identity> {
    using Parent = EncodeSize<Self_t, Identity>;

    using Parent::CreateFn;

    static llvm::Function *CreateFn(llvm::Module *module, llvm::Type *type) {
      auto size = llvm::cast<llvm::IntegerType>(type)->getScalarSizeInBits();
      return Parent::CreateFn(module, size);
    }

    static constexpr bool is_vararg = false;

    static llvm::Type *ResultType(llvm::Module *module, uint64_t size) {
      llvm::IRBuilder<> ir(module->getContext());
      return ir.getIntNTy(static_cast<uint32_t>(size));
    }

    static std::vector<llvm::Type *> ArgTypes(llvm::Module *module, uint64_t size) {
      return { ResultType(module, size) };
    }

    static llvm::Value *unwrap(llvm::Value *gift) {
      auto casted = llvm::dyn_cast_or_null<llvm::CallInst>(gift);
      if (!casted || !Parent::IsIntrinsic(casted->getCalledFunction())) {
        return gift;
      }
      auto surprise = casted->getArgOperand(0u);
      casted->eraseFromParent();
      return surprise;
    }

    static auto unwrap(const std::vector<llvm::CallInst *> &gifts) {
      std::vector<llvm::Value *> out;
      for (auto x : gifts) {
        out.push_back(unwrap(x));
      }
      return out;
    }

  };

  template <typename Self_t>
  struct Allocator : ParseInts<Self_t, 1> {
    using Parent = ParseInts<Self_t, 1>;

    static std::string Name(llvm::Type *type) {
      auto name = [](auto rec, auto type) -> std::string {
        if (auto p_type = llvm::dyn_cast<llvm::PointerType>(type)) {
          return "p." + rec(rec, p_type->getPointerElementType());
        }
        auto size = llvm::cast<llvm::IntegerType>(type)->getScalarSizeInBits();
        return std::to_string(size);
      };

      std::stringstream ss;
      ss << Self_t::fn_prefix << Self_t::separator << name(name, type);
      return ss.str();
    }

    static llvm::Function *CreateFn(llvm::Module *module, llvm::Type *type) {
      llvm::IRBuilder<> ir(module->getContext());
      auto fn_t = llvm::FunctionType::get(type, {}, true);
      auto callee = module->getOrInsertFunction(Name(type), fn_t);
      return Parent::freeze(llvm::cast<llvm::Function>(callee.getCallee()));
    }
  };

  template<typename Self_t, uint32_t allocated_size_>
  struct BucketAllocator : ParseInts<Self_t, 2>
  {
    using Parent = ParseInts<Self_t, 2>;

    static constexpr uint32_t allocated_size = allocated_size_;

    static std::string Name(uint32_t id) {
      std::stringstream ss;
      ss << Self_t::fn_prefix << Self_t::separator << id << Self_t::separator << allocated_size;
      return ss.str();
    }

    static llvm::Function *CreateFn(llvm::Module *module, uint32_t id) {
      llvm::IRBuilder<> ir(module->getContext());
      auto r_type = ir.getIntNTy(allocated_size);
      auto fn_t = llvm::FunctionType::get(r_type, {}, false);
      auto callee = module->getOrInsertFunction(Name(id), fn_t);
      return Parent::melt(llvm::cast<llvm::Function>(callee.getCallee()));
    }
  };

  template<typename Self_t>
  struct VarArg : EncodeSize<Self_t, VarArg> {
    using Parent = EncodeSize<Self_t, VarArg>;

    static constexpr bool is_vararg = true;

    static llvm::Type *ResultType(llvm::Module *module, uint64_t size) {
      llvm::IRBuilder<> ir(module->getContext());
      return ir.getIntNTy(static_cast<uint32_t>(size));
    }

    static std::vector<llvm::Type *> ArgTypes(llvm::Module *module, uint64_t size) {
      return {};
    }
  };

  template<typename Self_t>
  struct Select : ParseInts<Self_t, 2> {
    using Parent = ParseInts<Self_t, 2>;

    static std::string Name(uint64_t selector, uint64_t size) {
      std::stringstream ss;
      ss << Self_t::fn_prefix << Self_t::separator
         << selector << Self_t::separator << size;
      return ss.str();
    }

    static llvm::Function *CreateFn(llvm::Module *module, uint64_t selector, llvm::Type *type) {
      auto size = llvm::cast<llvm::IntegerType>(type)->getScalarSizeInBits();
      return CreateFn(module, selector, static_cast<uint64_t>(size));
    }

    static llvm::Function *CreateFn(llvm::Module *module, uint64_t selector, uint64_t size) {
      llvm::IRBuilder<> ir(module->getContext());
      auto r_ty = ir.getIntNTy(static_cast<uint32_t>(size));

      using size_type = std::vector<llvm::Type>::size_type;
      auto args_count = static_cast<size_type>((1 << selector) + 1);
      std::vector<llvm::Type *>args{ args_count, r_ty };
      args[0] = ir.getIntNTy(static_cast<uint32_t>(selector));

      auto fn_t = llvm::FunctionType::get(r_ty, args, false);
      auto callee = module->getOrInsertFunction(Name(selector, size), fn_t);
      return Parent::melt(llvm::cast<llvm::Function>(callee.getCallee()));
    }

    static_assert(std::is_same_v<typename Parent::intrinsic_args_t,
                                 std::tuple<uint64_t, uint64_t>
                                >);
  };


  template<typename I, typename C = std::vector< llvm::Value * >, typename ...Args>
  auto implement_call(llvm::IRBuilder<> &ir, const C &c_args, Args &&...args) {
    auto fn = I::CreateFn(ir.GetInsertBlock()->getModule(), std::forward<Args>(args)...);
    return ir.CreateCall(fn, c_args);
  }

  // Add size of each `llvm::Type` from `c_args` together.
  template<typename C>
  auto sum_sizes(const C &c_args) {
    uint64_t acc = 0;
    for (auto val : c_args) {
      auto int_ty = llvm::cast<llvm::IntegerType>(val->getType());
      acc += int_ty->getBitWidth();
    }
    return acc;
  }

} // namesapce impl

// TODO(lukas): We want to check that `fn_prefix` is never prefix of some
//              other `fn_prefix`.
namespace data {
  struct dot_seperator {
    static constexpr const char *separator = ".";
  };

  // extract.FROM.SIZE - where we include FROM and exclude FROM + SIZE.
  // If no argument is passed, instruction bits are considered to be an operand
  // in the lowering phase.
  // Data are reordered to fit endiannity as expected on instruction bits.

  #define sccc_prefix(what) static constexpr const char *fn_prefix = what

  struct Extract : dot_seperator { sccc_prefix("__circuitous.extract"); };
  struct BitCompare : dot_seperator { sccc_prefix("__circuitous.bitcompare"); };
  struct ByteSwap : dot_seperator { sccc_prefix("__circuitous.byteswap"); };
  struct Xor { sccc_prefix("__circuitous.xor"); };
  struct Or : dot_seperator { sccc_prefix("__circuitous.or"); };
  struct And : dot_seperator { sccc_prefix("__circuitous.and"); };
  struct VerifyInst { sccc_prefix("__circuitous.verify_inst"); };
  struct ExtractRaw : dot_seperator { sccc_prefix("__circuitous.raw_extract"); };
  struct Eq : dot_seperator { sccc_prefix("__circuitous.icmp_eq"); };
  struct Select : dot_seperator { sccc_prefix("__circuitous.select"); };
  struct InputImmediate : dot_seperator { sccc_prefix("__circuitous.input_imm"); };
  struct AllocateDst : dot_seperator { sccc_prefix("__circuitous.allocate_dst"); };
  struct Concat : dot_seperator { sccc_prefix("__circuitous.concat"); };
  struct BreakPoint : dot_seperator { sccc_prefix("__circuitous.breakpoint"); };
  struct Identity : dot_seperator { sccc_prefix("__circuitous.id"); };
  struct Advice : dot_seperator { sccc_prefix("__circuitous.advice"); };
  struct AdviceConstraint : dot_seperator { sccc_prefix("__circuitous.check_advice"); };
  struct OutputCheck : dot_seperator { sccc_prefix("__circuitous.out_check"); };
  struct Transport : dot_seperator { sccc_prefix("__circuitous.transport"); };
  struct Error : dot_seperator { sccc_prefix("__circuitous.error"); };
  struct Memory : dot_seperator { sccc_prefix("__circuitous.memory"); };

  struct ReadConstraint : dot_seperator { sccc_prefix("__circuitous.memread"); };
  struct WriteConstraint : dot_seperator { sccc_prefix("__circuitous.memwrite"); };
  struct UnusedConstraint : dot_seperator { sccc_prefix("__circuitous.unusued"); };

} //namespace data

// Intrinsic declaration. Overall, they all try to provide the following unified API
//  * `CreateFn` - creates (or returns an already present) corresponding `llvm::Function *`
//  * `make_*` - helpers to combine `CreateFn` and `llvm::CallInst` generation
//  * `IsIntrinsic` - checks if `llvm::Function` is intrinsic - based on its name
//  * `ParseArgs<T>` - if some information is encoded in the intrinsic (usually a size)
//                     returns `std::tuple` of such integer values casted to `T`.
//  * `ForAllIn` - for each call of the intrinsic in given function invoke a callback.

// NOTE(lukas): `data::` is needed because we would not be able to reference
//              the static attributes in the `impl::` if they were a part of struct
//              that is being defined.
struct BitCompare : impl::BinaryPredicate<data::BitCompare> {};
struct Extract : impl::Interval<data::Extract> {};

// Usual logical operators.
struct Xor : impl::Predicate<data::Xor> {};
struct Or : impl::Predicate<data::Or> {};
struct And : impl::Predicate<data::And> {};

// Top-level of separate instruction context -- if two instructions are identical
// e.g. `add rax, 9` and `add rbx, 12` it is desirable to merge them into one.
// It is expected exactly one of there intrinsic will be satisfied for any given valid
// instruction. (`xor` will be applied to their results)
struct VerifyInst : impl::Predicate<data::VerifyInst> {};

// See `Extract` but the reorder step is skipped.
struct ExtractRaw : impl::Interval<data::ExtractRaw> {};

// Equivalence between operands - should be used to check output valus
// of the circuit only.
struct Eq : impl::BinaryPredicate<data::Eq> {};

// Behaves in the same way as `Eq` but is reserved to represent output value
// comparisons. We may want to lower them in a different way (for example due
// to error bit value)
struct OutputCheck : impl::BinaryPredicate<data::OutputCheck> {};

// Identity wrapper denoting that something is an input immediate
// but can be in reality build from extracts and concats -- helps us
// in optimization phase.
struct InputImmediate : impl::Identity<data::InputImmediate> {};

// Returns a pointer to "allocated spaced". Intended to replace alloca 1:1
// without fear of it being removed by `mem2reg`.
// NOTE(lukas): Multiple calls should not be merged as we do not mark
//              the function as readnone etc.
struct AllocateDst : impl::Allocator<data::AllocateDst> {};

// Concats all its arguments together.
struct Concat : impl::VarArg<data::Concat> {};

// NOTE(lukas): Sometimes it is handy to fix some location in the
//              bb/function. This called should be removed manually
//              by the caller and it is undefined what happens
//              if optimizer is called (may merge multiple instances).
struct BreakPoint : impl::Predicate<data::BreakPoint> {};

struct Advice : impl::Allocator<data::Advice> {};
struct AdviceConstraint : impl::BinaryPredicate<data::AdviceConstraint> {};

// Has always `2 ** n + 1` operands and following prototype
// `iX select.N(iN selecort, iX v_1, iX v_2, ..., iX_(v ** n))`
// and returns the `v_(selector)` argument.
// Note that it will always return some argument, since there is one operand
// for each possible `selector` value.
struct Select : impl::Select<data::Select> {

  static llvm::Value *selector(llvm::CallInst *call) {
    CHECK(IsIntrinsic(call->getCalledFunction()));
    return call->getArgOperand(0);
  }

  static bool is_complete(llvm::CallInst *call) {
    return std::none_of(call->arg_begin(), call->arg_end(), is_undef);
  }

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

// Imposes a barrier that stops llvm optimizations (as they do not know)
// the body of this functions. Has no representation in the IR and will
// be thrown away during lowering process.
struct Identity : impl::Identity<data::Identity> {};

struct Transport : impl::Identity<data::Transport> {};

// hint, size, addr, ts, value
struct ReadConstraint : impl::Predicate<data::ReadConstraint> {};
struct WriteConstraint : impl::Predicate<data::WriteConstraint> {};
struct UnusedConstraint : impl::Predicate<data::UnusedConstraint> {};

struct Error : impl::Identity<data::Error> {};

struct Memory : impl::BucketAllocator<data::Memory, 16 + 64 + 64 + 64> {

  template<typename V>
  struct Parsed {
    // 1 bit
    V used;
    // 1 bit
    V mode;
    // 6 bit reserved

    // 4 bit id
    V id;

    // 4 bit
    V size;

    // 8 bytes
    V addr;
    // 8 bytes
    V value;
    // 8 bytes
    V timestamp;

    bool operator==(const Parsed< V > &other) const = default;
  };

  template< typename P >
  struct Validator {
    llvm::IRBuilder<> &ir;
    const P &parsed;

    Validator(llvm::IRBuilder<> &ir_, const P &parsed_) : ir( ir_ ), parsed( parsed_ ) {}

    auto iN(uint64_t size, uint64_t val) {
      return ir.getIntN(static_cast< uint32_t >(size), val);
    }
    auto make_cmp(llvm::Value *a, llvm::Value *b) { return ir.CreateICmpEQ(a, b); }

    auto read_flag() { return ir.getInt1(0u); }
    auto write_flag() { return ir.getInt1(0u); }

    auto read() { return ir.CreateICmpEQ(parsed.mode, read_flag()); }
    auto written() { return ir.CreateICmpEQ(parsed.mode, write_flag()); }

    auto used() { return ir.CreateICmpEQ(parsed.used, ir.getTrue()); }
    auto unused() { return ir.CreateICmpEQ(parsed.used, ir.getFalse()); }

    auto size(uint64_t expected) { return ir.CreateICmpEQ(parsed.size, iN(4, expected)); }

    auto addr(llvm::Value *a) { return ir.CreateICmpEQ(parsed.addr, a); }

    auto value(llvm::Value *v) { return make_cmp(parsed.value, v); }
    auto ts(llvm::Value *t) { return make_cmp(parsed.timestamp, t); }

    auto check_read(uint64_t size_, llvm::Value *addr_, llvm::Value *ts_) {
      return impl::implement_call< And >(
          ir, { used(), read(), size(size_), addr(addr_), ts(ts_) });
    }

    auto check_write(uint64_t size_, llvm::Value *addr_, llvm::Value *ts_, llvm::Value *val) {
      return impl::implement_call< And >(
          ir, { used(), read(), size(size_), addr(addr_), ts(ts_), value(val) });
    }

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
};

/* Helper functions to make creation of intrinsic calls easier for the user
 * C - container holding arguments
 * Args... - arguments to be forwarded to appropriate `CreateFn`.
 */
template<typename C>
auto make_transport(llvm::IRBuilder<> &ir, const C &args) {
  return impl::implement_call<Transport>(ir, {args}, args->getType());
}

template<typename C>
auto make_error(llvm::IRBuilder<> &ir, const C &arg) {
  return impl::implement_call<Error>(ir, {arg}, arg->getType());
}

template<bool reduce=false, typename C>
auto make_xor(llvm::IRBuilder<> &ir, const C &args) ->llvm::Value *
{
  if constexpr (reduce) {
    if (args.size() == 1) {
      return args[0];
    }
  }
  return impl::implement_call<Xor>(ir, args);
}

template<typename C>
auto make_and(llvm::IRBuilder<> &ir, const C &args) {
  return impl::implement_call<And>(ir, args);
}
template<typename C>
auto make_or(llvm::IRBuilder<> &ir, const C &args) {
  return impl::implement_call<Or>(ir, args);
}

template<typename C>
auto make_verify(llvm::IRBuilder<> &ir, const C &args) {
  return impl::implement_call<VerifyInst>(ir, args);
}

template<typename ...Args>
auto make_extract(llvm::IRBuilder<> &ir, Args &&...args) {
  return impl::implement_call<Extract>(ir, {}, std::forward<Args>(args)...);
}

template<typename ...Args>
auto make_raw_extract(llvm::IRBuilder<> &ir, llvm::Value *v, Args &&...args) {
  return impl::implement_call<ExtractRaw>(ir, {v}, std::forward<Args>(args)...);
}

template<typename ...Args>
auto make_raw_ib_extract(llvm::IRBuilder<> &ir, Args &&...args) {
  return impl::implement_call<ExtractRaw>(ir, {}, std::forward<Args>(args)...);
}

template<typename C = std::vector<llvm::Value *>, typename ...Args>
auto make_alloca(llvm::IRBuilder<> &ir, const C &c_args, Args &&...args) {
  return impl::implement_call<AllocateDst>(ir, c_args, std::forward<Args>(args)...);
}

template<typename C = std::vector<llvm::Value *>, typename ...Args>
auto make_bitcompare(llvm::IRBuilder<> &ir, const C &c_args, Args &&...args) {
  return impl::implement_call<BitCompare>(ir, c_args, std::forward<Args>(args)...);
}

template<typename C = std::vector<llvm::Value *>>
auto make_concat(llvm::IRBuilder<> &ir, const C &c_args) {
  auto result_size = impl::sum_sizes(c_args);
  return impl::implement_call<Concat>(ir, c_args, result_size);
}

static inline auto make_breakpoint(llvm::IRBuilder<> &ir) {
  return impl::implement_call<BreakPoint>(ir, {});
}

template<typename ...Args>
auto make_hint(llvm::IRBuilder<> &ir, Args &&...args) {
  return impl::implement_call<Advice>(ir, {}, std::forward<Args>(args)...);
}

template<typename C = std::vector<llvm::Value *>>
auto make_hintcheck(llvm::IRBuilder<> &ir, const C &c_args) {
  CHECK_EQ(c_args.size(), 2) << "make_hintcheck expects 2 call args.";
  CHECK(c_args[0]->getType() == c_args[1]->getType());
  return impl::implement_call<AdviceConstraint>(ir, c_args, c_args[0]->getType());
}

template<typename C = std::vector<llvm::Value *>, typename ...Args>
auto make_read_constraint(llvm::IRBuilder<> &ir, const C &c_args, Args &&... args) {
  return impl::implement_call<ReadConstraint>(ir, c_args, std::forward<Args>(args)...);
}
template<typename C = std::vector<llvm::Value *>, typename ...Args>
auto make_write_constraint(llvm::IRBuilder<> &ir, const C &c_args, Args &&... args) {
  return impl::implement_call<WriteConstraint>(ir, c_args, std::forward<Args>(args)...);
}


template<typename C = std::vector<llvm::Value *>, typename ...Args>
auto make_select(llvm::IRBuilder<> &ir, const C &c_args, Args &&... args) {
  return impl::implement_call<Select>(ir, c_args, std::forward<Args>(args)...);
}

template<typename C = std::vector<llvm::Value *>>
auto make_outcheck(llvm::IRBuilder<> &ir, const C &c_args) {
  CHECK_EQ(c_args.size(), 2) << "make_outcheck expects 2 call args,.";
  CHECK(c_args[0]->getType() == c_args[1]->getType());
  return impl::implement_call<OutputCheck>(ir, c_args, c_args[0]->getType());
}

template<typename ...Args>
auto make_memory(llvm::IRBuilder<> &ir, Args &&...args) {
  return impl::implement_call<Memory>(ir, {}, std::forward<Args>(args)...);
}

template<typename C = std::vector< llvm::Value * > >
auto make_unused_constraint(llvm::IRBuilder<> &ir, const C &c_args) {
  return impl::implement_call<UnusedConstraint>(ir, c_args);
}

template<typename T, typename ... Ts>
bool one_of(llvm::Function *fn) {
  if constexpr (sizeof...(Ts) == 0) {
    return T::IsIntrinsic(fn);
  } else {
    return T::IsIntrinsic(fn) || one_of<Ts ...>(fn);
  }
}

static inline bool is_any(llvm::Function *fn) {
  return fn->hasName() && fn->getName().startswith("__circuitous.");
}

template<typename T, typename ... Ts>
void enable_opts(llvm::Module *m) {
  T::melt(m);
  if constexpr (sizeof ... (Ts) != 0) return enable_opts<Ts...>(m);
}

template<typename T, typename ...Ts>
void disable_opts(llvm::Module *m) {
  T::freeze(m);
  if constexpr (sizeof ... (Ts) != 0) return disable_opts<Ts...>(m);
}

template<typename T, typename R = llvm::iterator_range<llvm::BasicBlock::iterator>>
auto collect(R range) {
  std::vector<llvm::CallInst *> out;
  for (auto &inst : range) {
    if (auto call_inst = llvm::dyn_cast<llvm::CallInst>(&inst)) {
      if (T::IsIntrinsic(call_inst->getCalledFunction())) {
        out.push_back(call_inst);
      }
    }
  }
  return out;
}

template<typename T>
std::vector<llvm::CallInst *> collect(llvm::Value *from, llvm::Value *to) {
  CHECK(llvm::isa<llvm::Instruction>(from) && llvm::isa<llvm::Instruction>(to));

  using bb_t = llvm::BasicBlock::iterator;
  auto begin = bb_t{llvm::cast<llvm::Instruction>(from)};
  auto end = bb_t{llvm::cast<llvm::Instruction>(to)};

  return collect<T>({ begin, end });
}

} // namespace circ::intrinsics