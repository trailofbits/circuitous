/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <cstdint>
#include <deque>
#include <map>
#include <optional>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <circuitous/Support/Check.hpp>

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Lifter/Shadows.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#pragma clang diagnostic pop

namespace circ::shadowinst
{
    using values_t = std::vector<llvm::Value *>;
    using annotated_values = std::unordered_map<std::string, values_t>;

    static inline annotated_values collect_annotated(llvm::Value *from, llvm::Value *to)
    {
        annotated_values out = {
            { Names::meta::dst_reg, {} },
            { Names::meta::verify_args, {} },
        };

        using bb_it_t = llvm::BasicBlock::iterator;
        auto begin = bb_it_t{llvm::cast<llvm::Instruction>(from)};
        auto end = bb_it_t{llvm::cast<llvm::Instruction>(to)};
        for (auto it = begin; it != end; ++it) {
            auto &inst = *it;
            if (auto dst_alloca = GetMetadata(&inst, Names::meta::dst_reg))
                out[Names::meta::dst_reg].push_back(&inst);

            if (auto verify_arg = GetMetadata(&inst, Names::meta::verify_args))
                out[Names::meta::verify_args].push_back(&inst);
        }
        return out;
    }

    struct SelectMaker
    {
        llvm::Value *head = nullptr;
        std::vector<llvm::Value *> conditions;
        llvm::IRBuilder<> &ir;

        SelectMaker(llvm::IRBuilder<> &ir_) : ir(ir_) {}

        llvm::Value *initial_head(llvm::Value *val)
        {
            return llvm::UndefValue::get(val->getType());
        }

        bool is_ptr(llvm::Value *val) const
        {
            return llvm::isa<llvm::PointerType>(val->getType());
        }
        bool is_ival(llvm::Value *val) const
        {
            return llvm::isa<llvm::IntegerType>(val->getType());
        }

        llvm::Value *coerce(llvm::Value *val)
        {
            check(is_ptr(head) == is_ptr(val));
            check(is_ival(head) == is_ival(val));

            if (is_ival(head))
                return ir.CreateSExtOrTrunc(val, head->getType());
            if (is_ptr(head))
                return ir.CreateBitCast(val, head->getType());

            unreachable() << "Unreachable";
        }

        llvm::Value *chain(llvm::Value *condition, llvm::Value *on_true)
        {
            if (!head)
                head = initial_head(on_true);

            conditions.push_back(condition);
            head = ir.CreateSelect(condition, coerce(on_true), head);
            return head;
        }

        llvm::Value *chain(const std::map< llvm::Value *, llvm::Value * > &vals)
        {
            for (const auto &[c, v] : vals)
                chain(c, v);
            return get();
        }

        template<typename T=llvm::Value>
        llvm::Value *get() { return llvm::cast_or_null< T >(head); }
    };

    struct Materializer
    {
        using values_t = std::vector< llvm::Value *>;

        llvm::IRBuilder<> &irb;
        const Reg &s_reg;

        Materializer(llvm::IRBuilder<> &irb_, const Reg &s_reg_)
            : irb(irb_), s_reg(s_reg_)
        {}

      private:
        void append(auto &into, const auto &from)
        {
            into.insert(into.end(), from.begin(), from.end());
        }

      public:
        [[nodiscard]] llvm::Type *selector_type() const;

        [[nodiscard]] llvm::Value *region_selector() const;
        [[nodiscard]] llvm::Value *opaque_selector() const;

        llvm::Value *tie_opaque_selector() const;

        llvm::Value *was_decoded() const;

        template< typename Getter >
        llvm::Instruction *unguarded_decoder(llvm::Value *selector, Getter &get_reg) const
        {
            auto entries = s_reg.translation_entries_count();
            auto bits = s_reg.region_bitsize();
            check(entries <= (1 << bits))
                << "Translation entries count do not correspond to regions size "
                << entries << " > " << (1 << bits);

            std::vector<llvm::Value *> select_args((1 << bits) + 1, nullptr);
            select_args[0] = selector;

            llvm::Type *type = nullptr;
            for (const auto &[str, reg] : s_reg.translation_bytes_map()) {
                auto idx = llvm::APInt{static_cast< uint32_t >(bits), str, 2}.getLimitedValue();
                check(select_args.size() > idx + 1);
                select_args[idx + 1] = get_reg(irb, reg);

                if (!type)
                    type = select_args[idx + 1]->getType();
                check(type == select_args[idx + 1]->getType());
            }

            check(type);
            for (std::size_t i = 0; i < select_args.size(); ++i)
                if (!select_args[i])
                    select_args[i] = llvm::UndefValue::get(type);

            check(select_args.size() > 1);
            return irops::make< irops::Select >(irb, select_args);
        }

        template< typename Getter >
        llvm::Instruction *unguarded_decoder(Getter &get_reg) const
        {
            return unguarded_decoder(opaque_selector(), get_reg);
        }

        using mats_t = Reg::materializations_t;
        auto translation_entries(const mats_t &mats) const -> values_t;

        auto translation_entries_of(const std::string &name) -> values_t
        {
            auto it = s_reg.translation_map.find(name);
            check(it != s_reg.translation_map.end());
            return translation_entries(it->second);
        }

        template< typename Arch >
        auto translation_map(const Arch &arch)
        -> std::map< Reg::reg_t, std::vector< llvm::Value * > >
        {
            std::map< Reg::reg_t, std::vector< llvm::Value * > > out;
            for (const auto &[reg, _] : s_reg.translation_map)
            {
                auto &out_bucket = out[enclosing_reg(arch, reg)->name];
                append(out_bucket, translation_entries_of(reg));
            }
            return out;
        }
    };
} // namespace circ::shadowinst
