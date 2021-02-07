/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once
/* Idea is that we provide some basic wrappers that bind llvm::Module and
 * llvm::LLVMContext so we do not need to specify them everywhere (mostly
 * when creating types or values).
 */

namespace circuitous {
  template<typename Self_t, template<typename ...> class Derived>
  struct Crtp {
    Self_t &Self() { return static_cast<Self_t &>(*this); }
    const Self_t &Self() const { return static_cast<Self_t &>(*this); }

    auto ctx() { return Self()._ctx; }
  };

  template<typename Self_t>
  struct LLVMTypesMixin : Crtp<Self_t, LLVMTypesMixin> {
    using parent_t = Crtp<Self_t, LLVMTypesMixin>;
    using parent_t::ctx;

    using types_t = std::vector<llvm::Type *>;

    auto Void() { return llvm::Type::getVoidTy(*ctx()); }
    auto FnTy(llvm::Type *ret, const types_t &args, bool is_vararg=false) {
        return llvm::FunctionType::get(ret, args, is_vararg);
    }
  };

  struct WithModule {
      llvm::Module *_module;
      llvm::LLVMContext *_ctx;

      WithModule(llvm::Module *module_)
        : _module(module_), _ctx(&_module->getContext())
      {}
  };

  struct LLVMTypes : LLVMTypesMixin<LLVMTypes>, WithModule {
      using WithModule::WithModule;
  };


} // namespace circuitous
