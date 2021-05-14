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
#pragma clang diagnostic pop

#include <cstdint>
#include <utility>

namespace circuitous::intrinsics {

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

    static llvm::Function *AddAttrs(llvm::Function *fn) {
      fn->addFnAttr(llvm::Attribute::ReadNone);
      fn->setLinkage(llvm::GlobalValue::ExternalLinkage);
      return fn;
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
      return Parent::AddAttrs(llvm::cast<llvm::Function>(callee.getCallee()));
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
      return Parent::AddAttrs(llvm::cast<llvm::Function>(callee.getCallee()));
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
      return Parent::AddAttrs(llvm::cast<llvm::Function>(callee.getCallee()));
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
      return Parent::AddAttrs(llvm::cast<llvm::Function>(callee.getCallee()));
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

  };

  template <typename Self_t>
  struct Allocator : Base<Self_t> {
    using Parent = Base<Self_t>;

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
      auto fn_t = llvm::FunctionType::get(type, {}, false);
      auto callee = module->getOrInsertFunction(Name(type), fn_t);
      return llvm::cast<llvm::Function>(callee.getCallee());
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
      return Parent::AddAttrs(llvm::cast<llvm::Function>(callee.getCallee()));
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
  struct Extract : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.extract";
  };

  // Used to compared extracted parts of instruction bits with expected values
  struct BitCompare : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.bitcompare";
  };

  // Reorder w.r.t to endiannity
  struct ByteSwap : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.byteswap";
  };

  // xor of all arguments - must have at least one argument.
  struct Xor {
    static constexpr const char *fn_prefix = "__circuitous.xor";
  };

  struct Or : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.or";
  };

  struct And : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.and";
  };

  // Top-level semantics that defines "a context" of a single instruction.
  struct VerifyInst {
    static constexpr const char *fn_prefix = "__circuitous.verify_inst";
  };

  struct ExtractRaw : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.raw_extract";
  };

  struct Eq : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.icmp_eq";
  };

  struct Select : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.select";
  };

  struct InputImmediate : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.input_imm";
  };

  struct AllocateDst : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.allocate_dst";
  };

  struct Concat : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.concat";
  };

  struct BreakPoint : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.breakpoint";
  };

  struct Identity : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.id";
  };

  struct OutputCheck : dot_seperator {
    static constexpr const char *fn_prefix = "__circuitous.out_check";
  };
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

// Has always `2 ** n + 1` operands and following prototype
// `iX select.N(iN selecort, iX v_1, iX v_2, ..., iX_(v ** n))`
// and returns the `v_(selector)` argument.
// Note that it will always return some argument, since there is one operand
// for each possible `selector` value.
struct Select : impl::Select<data::Select> {};

// Imposes a barrier that stops llvm optimizations (as they do not know)
// the body of this functions. Has no representation in the IR and will
// be thrown away during lowering process.
struct Identity : impl::Identity<data::Identity> {};

/* Helper functions to make creation of intrinsic calls easier for the user
 * C - container holding arguments
 * Args... - arguments to be forwarded to appropriate `CreateFn`.
 */
template<typename C>
auto make_xor(llvm::IRBuilder<> &ir, const C &args) {
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

template<typename ...Args>
auto make_alloca(llvm::IRBuilder<> &ir, Args &&...args) {
  return impl::implement_call<AllocateDst>(ir, {}, std::forward<Args>(args)...);
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

template<typename C = std::vector<llvm::Value *>, typename ...Args>
auto make_select(llvm::IRBuilder<> &ir, const C &c_args, Args &&... args) {
  return impl::implement_call<Select>(ir, c_args, std::forward<Args>(args)...);
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

} // namespace circuitous::intrinsics