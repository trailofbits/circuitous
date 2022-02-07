/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */
#pragma once

#include <circuitous/Fuzz/InstructionFuzzer.hpp>

#include <circuitous/Lifter/Context.hpp>

#include <circuitous/Util/Logging.hpp>
#include <circuitous/Util/InstructionBytes.hpp>

CIRCUITOUS_RELAX_WARNINGS
CIRCUITOUS_UNRELAX_WARNINGS

#include <optional>

namespace circ
{

    // TODO(lukas): Provide better api than `remill::Instruction`.
    struct Instruction
    {

    };

    // TODO(lukas): Just temporary struct to make refactoring easier.
    struct InstructionInfo
    {
        using rinst_t = remill::Instruction;
        using shadow_t = shadowinst::Instruction;
        using lifted_t = llvm::Function *;
        using enc_t = std::bitset< 15 * 8 >;

      private:
        std::optional< rinst_t >  _rinst;
        std::optional< enc_t >    _enc;
        std::optional< shadow_t > _shadow;
        std::optional< lifted_t > _lifted;

      public:
        InstructionInfo(remill::Instruction rinst)
            : _rinst(std::move(rinst)),
              _enc(InstBytes(rinst.bytes).to_enc< 15 * 8 >())
        {}

        bool has_shadow() const { return _shadow.has_value(); }
        bool has_lifted() const { return _lifted.has_value(); }

        rinst_t &rinst()   { check(_rinst); return *_rinst; }
        shadow_t &shadow() { check(_shadow); return *_shadow; }
        lifted_t &lifted() { check(_lifted); return *_lifted; }
        enc_t &enc()       { check(_enc); return *_enc; }

        const rinst_t &rinst()   const { check(_rinst); return *_rinst; }
        const shadow_t &shadow() const { check(_shadow); return *_shadow; }
        const lifted_t &lifted() const { check(_lifted); return *_lifted; }
        const enc_t &enc()       const { check(_enc); return *_enc; }

        void set(rinst_t r) { _rinst = std::make_optional( std::move(r) ); }
        void set(shadow_t s) { _shadow = std::make_optional( std::move(s) ); }
        void set(enc_t e) { _enc = std::make_optional( std::move(e) ); }
        void set(lifted_t l) { _lifted = std::make_optional( std::move(l) ); }

        void make_fuzz(CtxRef & ctx) { set(fuzz_operands(*ctx.arch(), rinst())); }

        template< typename ILifter >
        void make_lifted(CtxRef &ctx) {
            auto maybe_fn = ILifter(ctx).lift(*this);
            check(maybe_fn);
            set(*maybe_fn);
        }

        bool is_ready() const
        {
            check(_rinst && _enc) << "InstructionInfo is in inconsistent state!";
            return has_shadow() && has_lifted();
        }
    };

    struct InstructionBatch : has_ctx_ref
    {
        using parent_t = has_ctx_ref;
        using self_t = InstructionBatch;
        using insts_t = std::vector< InstructionInfo >;
        using raw_insts_t = std::vector< remill::Instruction >;

        insts_t insts;

        InstructionBatch(Ctx &ctx);
        InstructionBatch(Ctx &ctx, const std::vector< remill::Instruction > &rinsts);
        InstructionBatch(Ctx &ctx, const std::vector< InstBytes > &inst_bytes);
        InstructionBatch(Ctx &ctx, const std::string &raw_bytes);

        insts_t *operator->() { return &insts; }
        const insts_t *operator->() const { return &insts; }

        // Order of entries is not guaranteed.
        self_t &add(InstructionBatch &&other);
        self_t &add(InstructionInfo &&inst_info);
        self_t &add(raw_insts_t &&rinsts);

        self_t &fuzz();

        template< typename ILifter >
        self_t &lift()
        {
            for (auto &info : insts)
                if (!info.has_lifted())
                    info.make_lifted< ILifter >(this->ctx);
            return *this;
        }

        insts_t take() { return std::move(insts); }
    };

} // namespace circ
