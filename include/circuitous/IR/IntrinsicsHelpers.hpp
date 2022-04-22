/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Support/Check.hpp>

#include <vector>
#include <span>

namespace circ::irops {

  /* Intrinsic definitions.
   * There are 2 ways of navigating, `self_t` references the bottom class (CTRP-ish)
   * and `data_t` class that holds any data definitions (such as `fn_prefix` or `separator`).
   * Implementation is split into smaller classes so it is easier to define new ones by creating
   * inheritance chain.
   * `create_fn` creates the selected intrinsic function in module
   * `make` creates the selected intrinscis function and call instruction with given arguments.
   * `I::is` returns true iff given function is of intrinsic type `I` - information is encoded
   *         in the name of the function.
   * `parse_args< T >` parses arguments present in intrinsics name (usually size or id) and converts
   *                   these values to `T` using `static_cast` (to avoid further casting).
   *                   TODO(lukas): What if argument is not and integer?
   */

  namespace impl {

    // TODO(lukas): Remove once we have `std::span`.
    static inline std::optional< uint32_t > uniform_size(auto begin, auto end)
    {
      check(begin != end);
      std::optional< uint32_t > size;
      for (auto what : llvm::iterator_range(begin, end)) {
        auto what_size = what->getType()->getIntegerBitWidth();
        if (size && what_size != size)
          return std::nullopt;
        if (!size)
          size = std::make_optional(what_size);
      }
      return size;
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

    // Base of hierarchy, provides basic API
    template< typename Self_t, typename Data >
    struct Base : Data {
      using data_t = Data;
      using self_t = Self_t;

      using types_t = std::vector< llvm::Type * >;
      using values_t = std::vector< llvm::Value * >;

      static bool is(llvm::Function *fn) {
        if (!fn->hasName() || !fn->isDeclaration())
          return false;
        auto name = fn->getName();
        auto eaten = name.consume_front(data_t::fn_prefix);
        return eaten && (name.empty() || name.substr(0, 1) == data_t::separator);
      }

      static bool is(llvm::CallInst *call) {
        if (!call->getCalledFunction())
          return false;
        return is(call->getCalledFunction());
      }

    static std::vector<llvm::Function *> all(llvm::Module *module) {
      std::vector<llvm::Function *> out;
      for (auto &fn : *module) {
        if (is(&fn)) {
          out.push_back(&fn);
        }
      }
      return out;
    }

      template< typename CB >
      static void for_all_in(llvm::Function *trg, CB &&cb)
      {
        auto module = trg->getParent();

        std::vector<llvm::CallInst *> call_insts;
        for (auto fn : all(module)) {
          for (auto user : fn->users()) {
            if (auto call = llvm::dyn_cast<llvm::CallInst>(user);
                call && call->getParent()->getParent() == trg) {
              call_insts.push_back(call);
            }
          }
        }

        for (auto call: call_insts)
          cb(call);
      }

      static void freeze(llvm::Module *module) {
        for (auto fn : all(module)) {
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
        for (auto fn : all(module)) {
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

    // Inject `parse_args` method that parses last `C` arguments from fn name.
    template< typename N, uint8_t C >
    struct ParseInts : N
    {
      using self_t = typename N::self_t;
      using data_t = typename N::data_t;

      template< typename I = uint64_t >
      static auto parse_args(llvm::Function *fn) {
        check(N::is(fn))
          << "Cannot parse arguments of function: "
          << LLVMName(fn)
          << "that is not our intrinsic.";

        auto as_I = [](auto &str_ref) {
          uint64_t out;
          str_ref.getAsInteger(10, out);
          return static_cast< I >(out);
        };

        llvm::StringRef name = fn->getName();
        name.consume_front(data_t::fn_prefix);
        name.consume_front(data_t::separator);
        return consume< C >(name, as_I);
      }

      template< uint8_t L, typename Convert >
      static auto consume(llvm::StringRef str, Convert fn) {
        static_assert(L > 0);
        if constexpr (L == 1) {
          return std::make_tuple(fn(str));
        } else {
          const auto &[from, size] = str.split(data_t::separator);
          return std::tuple_cat(std::make_tuple(fn(from)), consume< L - 1 >(size, fn));
        }
      }

      using intrinsic_args_t = decltype(parse_args<uint64_t>(nullptr));
    };

    template< typename N >
    struct Name : N {
      using self_t = typename N::self_t;
      using data_t = typename N::data_t;

      template< typename H, typename ...T >
      static auto build_name(std::stringstream &ss, H &&h, T &&...t) {
        ss << std::forward< H >(h);
        if constexpr (sizeof...(T) != 0) {
          ss << data_t::separator;
          return Name::build_name(ss, std::forward< T >(t)...);
        }
      }

      template< typename ... Strs >
      static std::string name_(Strs &&... strs) {
        std::stringstream ss;
        Name::build_name(ss, std::forward< Strs >(strs)...);
        return ss.str();
      }

      template< typename ... Strs >
      static std::string name(Strs &&... strs) {
        return name_(self_t::fn_prefix, std::forward< Strs >(strs)...);
      }
    };

    template< typename N >
    struct Fn : N {
      using types_t = std::vector< llvm::Type * >;
      using self_t = typename N::self_t;

      static llvm::Type *llvm_t(llvm::Module *module, auto size_)
      {
        auto size = static_cast< uint32_t >(size_);
        return llvm::IRBuilder<>(module->getContext()).getIntNTy(size);
      }

      static llvm::Function *create_fn(
          llvm::Module *module, const std::string &name,llvm::FunctionType *fnt)
      {
        auto callee = module->getOrInsertFunction(name, fnt);
        return llvm::dyn_cast< llvm::Function >(callee.getCallee());
      }

      template< typename ...Args >
      static auto create_fn(llvm::IRBuilder<> &ir, Args &&...args) {
        return self_t::create_fn(ir.GetInsertBlock()->getModule(), std::forward< Args >(args)...);
      }
    };


    template< typename N, uint8_t C >
    struct Encodes : ParseInts< N, C > {
      using self_t = typename N::self_t;
      using values_t = typename N::values_t;

      static constexpr inline uint8_t encoded_args_size = C;

      template< typename ... Is >
      static auto name(Is ...is) {
        static_assert(sizeof...(Is) == C);
        return N::name(is...);
      }

      template< typename ... Is >
      static auto make(llvm::IRBuilder<> &ir, const values_t &c_args, Is ... is) {
        return ir.CreateCall(self_t::create_fn(ir, is ...), c_args);
      }
    };


    template< typename N >
    struct Predicate : N {
      using values_t = typename N::values_t;
      using self_t = typename N::self_t;
      using N::create_fn;

      static auto create_fn(llvm::Module *module, uint32_t size) {
        auto fnt = llvm::FunctionType::get(N::llvm_t(module, size), {}, true);
        return N::create_fn(module, self_t::name(size), fnt);
      }
    };

    template< typename N >
    struct BoolPredicate : N {
      using self_t = typename N::self_t;
      using N::create_fn;

      static auto create_fn(llvm::Module *module) {
        auto fnt = llvm::FunctionType::get(N::llvm_t(module, 1u), {}, true);
        return N::create_fn(module, self_t::name(1u), fnt);
      }
    };

    template< uint64_t S, typename N >
    struct Check : N {
      using values_t = typename N::values_t;
      using types_t = typename N::types_t;
      using self_t = typename N::self_t;
      using N::create_fn;

      static auto create_fn(llvm::Module *module, uint32_t size) {
        auto argts = types_t(S, N::llvm_t(module, size));
        auto fnt = llvm::FunctionType::get(N::llvm_t(module, 1u), argts, false);
        return N::create_fn(module, self_t::name(size), fnt);
      }
    };

    template< typename N > using BinaryCheck = Check< 2ul, N >;
    template< typename N > using UnaryCheck = Check< 1ul, N >;

    template< typename N >
    struct Identity : N {
      using values_t = typename N::values_t;
      using self_t = typename N::self_t;
      using N::create_fn;

      static auto create_fn(llvm::Module *module, uint32_t size) {
        auto argt = N::llvm_t(module, size);
        auto fnt = llvm::FunctionType::get(argt, {argt}, false);
        return N::create_fn(module, self_t::name(size), fnt);
      }
    };

    template< typename N >
    struct Extract : N {
      using values_t = typename N::values_t;
      using self_t = typename N::self_t;

      using N::create_fn;
      static_assert(N::encoded_args_size == 2);

      static auto create_fn(llvm::Module *module, auto from, auto size) {
        auto argt = N::llvm_t(module, size);
        // Anything can be extracted from, even some weird bitsizes, one intrinsic
        // should be good enough.
        auto fnt = llvm::FunctionType::get(argt, {}, true);
        return N::create_fn(module, self_t::name(from, size), fnt);
      }
    };

    template< typename N >
    struct Concat : N {
      using values_t = typename N::values_t;
      using self_t = typename N::self_t;
      using N::create_fn;

      static auto create_fn(llvm::Module *module, auto size) {
        auto argt = N::llvm_t(module, size);
        // Anything can be extracted from, even some weird bitsizes, one intrinsic
        // should be good enough.
        auto fnt = llvm::FunctionType::get(argt, {}, true);
        return N::create_fn(module, self_t::name(size), fnt);
      }

      static auto make(llvm::IRBuilder<> &ir, const values_t &c_args) {
        return N::make(ir, c_args, sum_sizes(c_args));
      }
    };

    template< typename N >
    struct Select : N {
      using values_t = typename N::values_t;
      using self_t = typename N::self_t;
      using N::create_fn;

      static auto create_fn(llvm::Module *module, auto selector, auto size) {
        auto argt = N::llvm_t(module, selector);
        auto rett = N::llvm_t(module, size);

        auto fnt = llvm::FunctionType::get(rett, {argt}, true);
        return N::create_fn(module, self_t::name(selector, size), fnt);
      }

      using N::make;

      static auto make(llvm::IRBuilder<> &ir, const values_t &c_args) {
        auto selector_size = uniform_size(c_args.begin(), std::next(c_args.begin()));
        auto ret_size = uniform_size(std::next(c_args.begin()), c_args.end());
        check(selector_size && ret_size);
        return N::make(ir, c_args, *selector_size, *ret_size);
      }
    };

    template< typename N >
    struct IdxIAllocator : N {
      using values_t = typename N::values_t;
      using self_t = typename N::self_t;

      using N::create_fn;
      static_assert(N::encoded_args_size == 2);

      static auto create_fn(llvm::Module *module, auto size, auto idx) {
        auto fnt = llvm::FunctionType::get(N::llvm_t(module, size), {}, true);
        return N::create_fn(module, self_t::name(size, idx), fnt);
      }
    };

    template< typename N >
    struct OperandLeaf : N {
      using values_t = typename N::values_t;
      using self_t = typename N::self_t;

      using N::create_fn;
      static_assert(N::encoded_args_size == 3);

      static auto name(auto x, auto y, llvm::Type *type) {
        auto int_type = llvm::dyn_cast< llvm::IntegerType >(type);
        check(int_type);
        auto size = int_type->getScalarSizeInBits();
        return N::name(static_cast< uint64_t >(x), static_cast< uint64_t >(y), size);
      }

      static auto create_fn(llvm::Module *module, auto x, auto y, llvm::Type *type) {
        auto fnt = llvm::FunctionType::get(type, {}, true);
        return N::create_fn(module, self_t::name(x, y, type), fnt);
      }
    };


    template< typename N >
    struct AdviceAllocator : N {
      using values_t = typename N::values_t;
      using self_t = typename N::self_t;

      using N::create_fn;
      static_assert(N::encoded_args_size == 2);

      static auto name(auto idx, llvm::Type *type) {
        auto int_type = llvm::dyn_cast< llvm::IntegerType >(type);
        check(int_type);
        auto size = int_type->getScalarSizeInBits();
        return N::name(static_cast< uint64_t >(idx), size);
      }

      static auto create_fn(llvm::Module *module, auto idx, llvm::Type *type) {
        auto fnt = llvm::FunctionType::get(type, {}, true);
        return N::create_fn(module, self_t::name(idx, type), fnt);
      }
    };

    template< typename N >
    struct Allocator : N {
      using values_t = typename N::values_t;
      using self_t = typename N::self_t;
      using N::create_fn;

      static auto name(llvm::Type *t) {
        auto type_name = [](auto rec, auto type) -> std::string {
          if (auto p_type = llvm::dyn_cast<llvm::PointerType>(type)) {
            return "p." + rec(rec, p_type->getPointerElementType());
          }
          auto size = llvm::cast<llvm::IntegerType>(type)->getScalarSizeInBits();
          return std::to_string(size);
        };
        return N::name(type_name(type_name, t));
      }

      static auto create_fn(llvm::Module *module, llvm::Type *t) {
        auto fnt = llvm::FunctionType::get(t, {}, true);
        return N::create_fn(module, self_t::name(t), fnt);
      }
    };

    enum io_type : uint32_t {
      in = 0,
      out = 1,
    };

    template< typename N, uint32_t S, uint32_t ... is >
    struct IOLeaf : N {
      using values_t = typename N::values_t;
      using self_t = typename N::self_t;
      using N::create_fn;

      static constexpr inline uint32_t size = S;

      static auto create_fn(llvm::Module *module, auto io_) {
        auto io = static_cast< uint32_t >(io_);
        auto fnt = llvm::FunctionType::get(N::llvm_t(module, S), {}, true);
        return N::create_fn(module, self_t::name(S, io), fnt);
      }

      template< uint32_t H, uint32_t ... T >
      static auto make_all_(llvm::IRBuilder<> &ir, const values_t &c_args) {
        auto current = std::make_tuple(self_t::make(ir, c_args, H));
        if constexpr (sizeof...(T) != 0) {
          return std::tuple_cat(current, make_all_< T ... >(ir, c_args));
        } else {
          return current;
        }
      }

      static auto make_all(llvm::IRBuilder<> &ir, const values_t &c_args) {
        return make_all_< is ... >(ir, c_args);
      }
    };


    template< typename N >
    struct DerivesSize : N {
      using values_t = typename N::values_t;
      using N::make;

      static auto make(llvm::IRBuilder<> &ir, const values_t &c_args) {
        auto size = uniform_size(c_args.begin(), c_args.end());
        check(size, [&](){ return "Operands are not of uniform size\n" + dbg_dump(c_args); });
        return N::make(ir, c_args, *size);
      }
    };


    template< typename N >
    struct Frozen : N {
      using self_t = typename N::self_t;

      template< typename ... Args >
      static auto create_fn(Args && ...args) {
        return N::freeze(N::create_fn(std::forward< Args >(args)...));
      }
    };

    template< typename N >
    struct Melted : N {
      using values_t = typename N::values_t;

      template< typename ... Args >
      static auto create_fn(Args && ...args) {
        return N::melt(N::create_fn(std::forward< Args >(args)...));
      }
    };


    template< typename S, typename D, uint8_t C >
    using def_base_t = Encodes< Fn< Name< Base< S, D > > >, C >;

    template< typename S, typename D >
    using predicate_base_t = Melted< DerivesSize< Predicate< def_base_t< S, D, 1 > > > >;

    template< typename S, typename D >
    using frozen_predicate_t = Frozen< BoolPredicate< def_base_t< S, D, 1 > > >;

    template< typename S, typename D >
    using binary_check_t = Melted< DerivesSize< BinaryCheck< def_base_t< S, D, 1 > > > >;

    template< typename S, typename D >
    using unary_check_t = Melted< DerivesSize< UnaryCheck< def_base_t< S, D, 1 > > > >;

    template< typename S, typename D >
    using identity_t = Melted< DerivesSize< Identity< def_base_t< S, D, 1 > > > >;

    template< typename S, typename D >
    using extract_t = Melted< Extract< def_base_t< S, D, 2 > > >;

    template< typename S, typename D >
    using op_selector_t = Melted< OperandLeaf< def_base_t< S, D, 3 > > >;

    template< typename S, typename D >
    using was_decoded_t = Melted< OperandLeaf< def_base_t< S, D, 3 > > >;

    template< typename S, typename D >
    using concat_t = Melted< Concat< def_base_t< S, D, 1 > > >;

    template< typename S, typename D >
    using select_t = Melted< Select < def_base_t< S, D, 2 > > >;

    template< typename S, typename D >
    using mem_allocator_t = Melted< IdxIAllocator< def_base_t < S, D, 2 > > >;

    template< typename S, typename D >
    using advice_allocator_t = Melted< AdviceAllocator< def_base_t< S, D, 2 > > >;

    template< typename S, typename D >
    using allocator_t = Frozen< Allocator< def_base_t< S, D, 1 > > >;

    template< typename S, typename D, uint32_t C >
    using io_leaf_t = Melted< IOLeaf< def_base_t< S, D, 2 >, C, io_type::in, io_type::out > >;

    template< typename S, typename D > using ebit_t = io_leaf_t< S, D, 1u >;
    template< typename S, typename D > using timestamp_t = io_leaf_t< S, D, 64u >;
    // TODO(lukas): Remove magic constant.
    template< typename S, typename D > using instbit_t = io_leaf_t< S, D, 15 * 8 >;

  } // namespace impl

} // namespace circ::irops
