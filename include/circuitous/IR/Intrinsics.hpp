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

  template<typename Self_t, uint8_t C>
  struct ParseInts : Base<Self_t> {
    using Parent = Base<Self_t>;

    static auto ParseArgs(llvm::Function *fn) {
      CHECK(Parent::IsIntrinsic(fn))
        << "Cannot parse arguments of function: "
        << LLVMName(fn)
        << "that is not our intrinsic.";

      auto as_uint64_t = [](auto &str_ref) {
        uint64_t out;
        str_ref.getAsInteger(10, out);
        return out;
      };

      llvm::StringRef name = fn->getName();
      name.consume_front(Self_t::fn_prefix);
      name.consume_front(Self_t::separator);
      return Consume<C>(name, as_uint64_t);
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

    using intrinsic_args_t = decltype(ParseArgs(nullptr));
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

  template<typename Self_t>
  struct EncodeSize : Base<Self_t> {
    using Parent = Base<Self_t>;

    static std::string Name(uint64_t size) {
      std::stringstream ss;
      ss << Self_t::fn_prefix << Self_t::separator << size;
      return ss.str();
    }

    static llvm::Function *CreateFn(llvm::Module *module, uint64_t size) {
      llvm::IRBuilder<> ir(module->getContext());
      auto ret_ty = Self_t::ResultType(module, size);
      auto args_ty = Self_t::ArgTypes(module, size);
      auto fn_t = llvm::FunctionType::get(ret_ty, args_ty, Self_t::is_vararg);
      auto callee = module->getOrInsertFunction(Name(size), fn_t);
      return Parent::AddAttrs(llvm::cast<llvm::Function>(callee.getCallee()));
    }

    using intrinsic_args_t = uint64_t;
    static intrinsic_args_t ParseArgs(llvm::Function *fn) {
      CHECK(Parent::IsIntrinsic(fn))
        << "Cannot parse arguments of function: "
        << LLVMName(fn)
        << "that is not our intrinsic.";

      llvm::StringRef name = fn->getName();
      name.consume_front(Self_t::fn_prefix);
      name.consume_front(Self_t::separator);

      uint64_t out;
      name.getAsInteger(10, out);
      return out;
    }

  };

  template<typename Self_t>
  struct Identity : EncodeSize<Self_t> {
    using Parent = EncodeSize<Self_t>;

    using Parent::CreateFn;

    static llvm::Function *CreateFn(llvm::Module *module, llvm::Type *type) {
      auto size = llvm::cast<llvm::IntegerType>(type)->getScalarSizeInBits();
      return EncodeSize<Self_t>::CreateFn(module, size);
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
  struct VarArg : EncodeSize<Self_t> {
    using Parent = EncodeSize<Self_t>;

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

  template<typename C>
  auto add_sizes(const C &c_args) {
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

  // extract.FROM.SIZE - where we include FROM and exclude FROM + SIZE.
  // If no argument is passed, instruction bits are considered to be an operand
  // in the lowering phase.
  // Data are reordered to fit endiannity as expected on instruction bits.
  struct Extract {
    static constexpr const char *fn_prefix = "__circuitous.extract";
    static constexpr const char *separator = ".";
  };

  // Used to compared extracted parts of instruction bits with expected values
  struct BitCompare {
    static constexpr const char *fn_prefix = "__circuitous.bitcompare";
    static constexpr const char *separator = ".";
  };

  // Reorder w.r.t to endiannity
  struct ByteSwap {
    static constexpr const char *fn_prefix = "__circuitous.byteswap";
    static constexpr const char *separator = ".";
  };

  // xor of all arguments - must have at least one argument.
  struct OneOf {
    static constexpr const char *fn_prefix = "__circuitous.one_of";
  };

  // Top-level semantics that defines "a context" of a single instruction.
  struct VerifyInst {
    static constexpr const char *fn_prefix = "__circuitous.verify_inst";
  };
} //namespace data

// TODO(lukas): I guess the `data::` is actually not needed + the `impl::`
//              can be generalized a bit more. If you want to add a new one
//              check if some already existing one cannot be generalized.
struct BitCompare : data::BitCompare, impl::BinaryPredicate<BitCompare> {};
struct Extract : data::Extract, impl::Interval<Extract> {};
struct OneOf : data::OneOf, impl::Predicate<OneOf> {};
struct VerifyInst : data::VerifyInst, impl::Predicate<VerifyInst> {};

// See `Extract` but the reorder step is skipped.
struct ExtractRaw : impl::Interval<ExtractRaw> {
  static constexpr const char *fn_prefix = "__circuitous.raw_extract";
  static constexpr const char *separator = ".";
};

// Equivalence between operands - should be used to check output valus
// of the circuit only.
struct Eq : impl::BinaryPredicate<Eq> {
  static constexpr const char *fn_prefix = "__circuitous.icmp_eq";
  static constexpr const char *separator = ".";
};

// Identity wrapper denoting that something is an input immediate
// but can be in reality build from extracts and concats -- helps us
// in optimization phase.
struct InputImmediate : impl::Identity<InputImmediate> {
  static constexpr const char *fn_prefix = "__circuitous.input_imm";
  static constexpr const char *separator = ".";
};

// Returns a pointer to "allocated spaced". Intended to replace alloca 1:1
// without fear of it being removed by `mem2reg`.
// NOTE(lukas): Multiple calls should not be merged as we do not mark
//              the function as readnone etc.
struct AllocateDst : impl::Allocator<AllocateDst> {
  static constexpr const char *fn_prefix = "__circuitous.allocate_dst";
  static constexpr const char *separator = ".";
};

// Concats all its arguments together.
struct Concat : impl::VarArg<Concat> {
  static constexpr const char *fn_prefix = "__circuitous.concat";
  static constexpr const char *separator = ".";
};

// NOTE(lukas): Sometimes it is handy to fix some location in the
//              bb/function. This called should be removed manually
//              by the caller and it is undefined what happens
//              if optimizer is called (may merge multiple instances).
struct BreakPoint : impl::Predicate<BreakPoint> {
  static constexpr const char *fn_prefix = "__circuitous.breakpoint";
  static constexpr const char *separator = ".";
};


/* Helper functions to make creation of intrinsic calls easier for the user
 * C - container holding arguments
 * Args... - arguments to be forwarded to appropriate `CreateFn`.
 */
template<typename C>
auto make_xor(llvm::IRBuilder<> &ir, const C &args) {
  auto xor_fn = OneOf::CreateFn(ir.GetInsertBlock()->getModule());
  return ir.CreateCall(xor_fn, args);
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
auto make_raw_extract(llvm::IRBuilder<> &ir, Args &&...args) {
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
  auto result_size = impl::add_sizes(c_args);
  return impl::implement_call<Concat>(ir, c_args, result_size);
}

static inline auto make_breakpoint(llvm::IRBuilder<> &ir) {
  return impl::implement_call<BreakPoint>(ir, {});
}

} // namespace circuitous::intrinsics