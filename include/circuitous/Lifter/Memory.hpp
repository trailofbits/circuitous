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
    for (auto i = 0u; i < ctx->getNumArgOperands(); ++i) {
      auto call = llvm::dyn_cast<llvm::CallInst>(ctx->getArgOperand(i));

      if (!call ||
          !intrinsics::one_of<intrinsics::ReadConstraint,
                              intrinsics::WriteConstraint>(call->getCalledFunction()))
      {
        continue;
      }

      auto mem_hint = llvm::dyn_cast< llvm::CallInst >(call->getArgOperand(0u));
      CHECK(mem_hint && intrinsics::Memory::IsIntrinsic(mem_hint->getCalledFunction()));
      constrained.insert(intrinsics::Memory::id(mem_hint->getCalledFunction()));
    }
    return constrained;
  }

  static inline auto get_all(llvm::Module *m) {
    std::unordered_map<uint64_t, llvm::Function *> out;
    for (auto fn : intrinsics::Memory::All(m)) {
      out[intrinsics::Memory::id(fn)] = fn;
    }
    return out;
  }

  struct Synthetizer {
    using mem_t = intrinsics::Memory;

    using call_t = std::tuple< llvm::CallInst *, uint64_t >;
    std::vector< call_t > reads;
    std::vector< call_t > writes;

    uint32_t next = 0;
    llvm::Value *ts;
    std::vector<llvm::Value *> checks;

    Synthetizer(llvm::Value *ts_) : ts( ts_ ) {}

    auto parse_hint(llvm::IRBuilder<> &ir, llvm::CallInst *call) {
      auto extractor = [&](auto inst, auto from, auto to) {
        return intrinsics::make_raw_extract(ir, {inst}, from, to);
      };
      return mem_t::parse< llvm::Value * >(call, extractor);
    }

    auto next_hint(llvm::IRBuilder<> &ir, llvm::CallInst *call) {
      auto hint = intrinsics::make_memory(ir, next);
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
        CHECK( size_ <= 64 && size_ > 0);

        if (size_ == 64) return out.value;
        return ir.CreateTrunc(out.value, ir.getIntNTy(static_cast< uint32_t >(size_)));
      }(size);

      auto llvm_size = ir.getIntN(4u, size / 8);
      checks.push_back(intrinsics::make_read_constraint(ir, {hint, llvm_size, addr, ts}));

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
        CHECK( size_ <= 64 && size_ > 0);

        if (size_ == 64) return value;
        return ir.CreateZExt(value, ir.getIntNTy(static_cast< uint32_t >(size_)));
      }(size);

      CHECK(size / 8 <= 8) << "Cannot emit read bigger than 64 bytes.";
      auto llvm_size = ir.getIntN(4u, size / 8);
      checks.push_back(
          intrinsics::make_write_constraint(ir, {hint, llvm_size, addr, ts, coerced_value}));
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

  auto synthetize_memory(llvm::Value *ts, llvm::Value *from, llvm::Value *to) {
    return Synthetizer(ts).all(from, to);
  }

  auto synthetize_memory(llvm::Instruction *from, llvm::Instruction *to) {
    llvm::IRBuilder<> ir(from);
    return synthetize_memory(intrinsics::make_timestamp(ir, intrinsics::io_type::in), from, to);
  }

} // namespace circ::mem