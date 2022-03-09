/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */


#pragma once

#include <circuitous/IR/Intrinsics.hpp>
#include <circuitous/Lifter/ShadowMat.hpp>

namespace circ::component {

    template< typename Self >
    struct Component_ {
        llvm::IRBuilder<> &ir;
        Component_(llvm::IRBuilder<> &ir_) : ir(ir_) {}

        auto &self() { return static_cast< Self & >(*this); }

        template< typename ... Args >
        static auto construct(Args &&... args) {
            return Self(std::forward< Args >(args)...)._construct();
        }
    };

    struct SaturationProp_ : Component_< SaturationProp_ > {
        using Component_::Component_;

        auto _construct() {
            auto [ebit_in, ebit_out] = irops::make_all_leaves< irops::ErrorBit >(ir);
            return ir.CreateOr(ir.CreateNot(ebit_in), ebit_out);
        }
    };

    struct TimestampProp_ : Component_< TimestampProp_ > {
        using Component_::Component_;

        auto _construct() {
            auto [ts_in, ts_out] = irops::make_all_leaves< irops::Timestamp >(ir);
            auto inc =  ir.CreateAdd(ts_in, ir.getInt64(1));
            return irops::make< irops::OutputCheck >(ir, {inc, ts_out});
        }
    };

    using SaturationProp = SaturationProp_;
    using TimestampProp = TimestampProp_;

    struct Context {
        using self_t = Context;

        llvm::Instruction *current = nullptr;
        // To eliminate duplicate calls
        std::unordered_set< llvm::Value * > operands;

        template< typename ... Args >
        Context(llvm::BasicBlock *bb_, Args && ... args)
            : current(_make_dummy(bb_))
        {
          add_operands(std::forward< Args >(args)...);
        }

        Context(llvm::Instruction *c)
            : current(c)
        {
            log_info() << "Start.";
            auto call = llvm::dyn_cast< llvm::CallInst >(c);
            check(call);
            for (uint32_t i = 0; i < call->getNumArgOperands(); ++i)
            {
                check(!llvm::isa< llvm::Function >(call->getArgOperand(i)));
                operands.insert(call->getArgOperand(i));
            }
            log_info() << "Done.";
        }

        template< typename L > requires (std::is_base_of_v< llvm::Value, L >)
        void _add(const std::vector< L * > &vs) {
            operands.insert(vs.begin(), vs.end());
        }
        void _add(llvm::Value *v) { operands.insert(v); }

        template< typename H, typename ...Args >
        self_t &add_operands(H &&h, Args && ...args)
        {
            _add(std::forward< H >(h));
            if constexpr (sizeof...(Args) == 0) {
                return *this;
            } else {
                return add_operands< Args ... >(std::forward< Args >(args)...);
            }
        }

        static llvm::Instruction *_make_dummy(llvm::BasicBlock  *bb) {
            llvm::IRBuilder<> ir(bb);
            return irops::make< irops::VerifyInst >(ir, ir.getTrue());
        }

        // Caller is responsible for correctly setting insertion point wrt to domination
        llvm::Value *regenerate()
        {
            llvm::IRBuilder<> irb(current);
            return regenerate(irb);
        }
        llvm::Value *regenerate(llvm::IRBuilder<> &irb) {
            // TODO(lukas): Accept other collections in `irops::make`.
            auto new_ = irops::make< irops::VerifyInst >(
                irb, std::vector< llvm::Value * >(operands.begin(), operands.end()));
            new_->copyMetadata(*current);
            current->replaceAllUsesWith(new_);
            current->eraseFromParent();
            current = new_;
            return current;
        }
    };

    template< typename H, typename ...Args >
    auto construct(llvm::IRBuilder<> &ir) {
        auto self = std::make_tuple(H::construct(ir));
        if constexpr (sizeof...(Args) == 0) return self;
        else return std::tuple_cat(self, construct< Args ... >(ir));
    }

    template< typename ... Args>
    auto construct(llvm::BasicBlock *bb) {
        llvm::IRBuilder<> irb(bb);
        return construct< Args ... >(irb);
    }

} // namespace circ::component
