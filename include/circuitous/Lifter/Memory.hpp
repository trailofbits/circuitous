/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Intrinsics.hpp>

namespace circ::mem {

  // Returns `[explicit_errors, implicit_erros]`
  template<typename R = llvm::iterator_range<llvm::BasicBlock::iterator>>
  static inline auto collect(R range)
  {
    auto get_size = [](auto name) -> uint64_t {
      auto [_, tail] = name.rsplit('_');
      uint64_t out;
      tail.getAsInteger(10, out);
      CHECK(out != 0) << tail.str() << " from " << name.str() << " got parsed as 0.";
      return out;
    };

    using call_t = std::tuple< llvm::CallInst *, uint64_t >;
    std::vector< call_t > writes;
    std::vector< call_t > reads;
    for (auto &inst : range) {
      if (auto call = llvm::dyn_cast< llvm::CallInst >(&inst)) {
        auto callee = call->getCalledFunction();
        if (!callee || !callee->hasName()) continue;

        if (callee->getName().startswith("__remill_read_memory")) {
          reads.emplace_back( call, get_size( callee->getName() ) );
        }
        if (callee->getName().startswith("__remill_write_memory")) {
          writes.emplace_back( call, get_size( callee->getName() ) );
        }

      }
    }
    return std::make_tuple( std::move( reads ), std::move( writes ) );
  }

  static inline auto collect(llvm::Value *from, llvm::Value *to) {
    CHECK(llvm::isa<llvm::Instruction>(from) && llvm::isa<llvm::Instruction>(to));

    auto as_it = [](auto what) {
      return llvm::BasicBlock::iterator{ llvm::cast<llvm::Instruction>(what) };
    };
    return collect({as_it(from), as_it(to)});
  }

  static inline auto constrained_by(llvm::CallInst *ctx) {
    std::unordered_set<uint64_t> constrained;
    for (auto i = 0u; i < ctx->arg_size(); ++i) {
      auto call = llvm::dyn_cast<llvm::CallInst>(ctx->getArgOperand(i));

      if (!call ||
          !irops::one_of< irops::ReadConstraint,
                          irops::WriteConstraint >(call->getCalledFunction()))
      {
        continue;
      }

      auto mem_hint = irops::Instance< irops::Memory >(call->getArgOperand(0u));
      constrained.insert(mem_hint.id());
    }
    return constrained;
  }

  static inline auto get_all(llvm::Module *m) {
    std::unordered_map<uint64_t, llvm::Function *> out;
    for (auto fn : irops::Memory::all(m)) {
      out[irops::Instance< irops::Memory >(fn)] = fn;
    }
    return out;
  }

  struct Synthetizer {

    using call_t = std::tuple< llvm::CallInst *, uint64_t >;
    std::vector< call_t > reads;
    std::vector< call_t > writes;

    uint32_t next = 0;
    llvm::Value *ts = nullptr;
    uint32_t ptr_size = 0;
    std::vector<llvm::Value *> checks;

    Synthetizer(llvm::Value *ts_, uint32_t ptr_size_)
        : ts( ts_ ), ptr_size(ptr_size_) {}

    auto parse_hint(llvm::IRBuilder<> &ir, llvm::CallInst *call) {
      auto extractor = [&](auto inst, auto from, auto to) {
        return irops::make< irops::ExtractRaw >(ir, {inst}, from, to);
      };
      return irops::memory::parse< llvm::Value * >(call, extractor, ptr_size);
    }

    auto next_hint(llvm::IRBuilder<> &ir, llvm::CallInst *call) {
      auto hint = irops::make_leaf< irops::Memory >(ir, irops::memory::size(ptr_size), next);
      ++next;
      return hint;
    }

    auto handle_read(const call_t &call_) {
      auto [call, size] = call_;
      llvm::IRBuilder<> ir(call);

      auto addr = call->getArgOperand(1u);

      auto hint = next_hint(ir, call);
      auto out = parse_hint(ir, hint);

      auto coerced_value = [&](auto size_) {
        CHECK( size_ <= ptr_size && size_ > 0);

        if (size_ == ptr_size) return out.value();
        return ir.CreateTrunc(out.value(), ir.getIntNTy(static_cast< uint32_t >(size_)));
      }(size);

      auto llvm_size = ir.getIntN(4u, size / 8);
      checks.push_back(irops::make< irops::ReadConstraint >(ir, {hint, llvm_size, addr, ts}));

      call->replaceAllUsesWith(coerced_value);
      call->eraseFromParent();
    }

    auto handle_write(const call_t &call_) {
      auto [call, size] = call_;
      llvm::IRBuilder<> ir(call);

      auto addr = call->getArgOperand(1u);
      auto value = call->getArgOperand(2u);

      auto hint = next_hint(ir, call);

      auto coerced_value = [&](auto size_) {
        CHECK( size_ <= ptr_size && size_ > 0);

        if (size_ == ptr_size) return value;
        return ir.CreateZExt(value, ir.getIntNTy(static_cast< uint32_t >(size_)));
      }(size);

      CHECK(size <= ptr_size) << "Cannot emit read bigger than ptr_size.";
      auto s = ir.getIntN(4u, size / 8);
      checks.push_back(irops::make< irops::WriteConstraint >(
            ir, {hint, s, addr, ts, coerced_value}));
      call->replaceAllUsesWith(call->getArgOperand(0));
      call->eraseFromParent();
    }

    std::vector<llvm::Value *> all(llvm::Value *from, llvm::Value *to) {
      std::tie( reads, writes ) = collect( from, to );

      for (const auto &call : reads) {
        handle_read(call);
      }
      for (const auto &call : writes) {
        handle_write(call);
      }
      return std::move(checks);
    }

  };

  auto synthetize_memory(llvm::Value *ts, llvm::Value *from,
                         llvm::Value *to, uint32_t ptr_size)
  {
    return Synthetizer(ts, ptr_size).all(from, to);
  }

  auto synthetize_memory(llvm::Instruction *from, llvm::Instruction *to, uint32_t ptr_size) {
    llvm::IRBuilder<> ir(from);
    return synthetize_memory(irops::make_leaf< irops::Timestamp >(ir, irops::io_type::in),
                             from, to, ptr_size);
  }

} // namespace circ::mem
