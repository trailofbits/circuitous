/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Support/Check.hpp>
#include <circuitous/Lifter/Shadows.hpp>
#include <circuitous/Lifter/ShadowMat.hpp>

namespace circ::shadowinst
{

    // Returns mapping { offset -> [ bitstring identifying the regs ] }
    using bstr_t = std::string;
    using bstrs_t = std::vector< bstr_t >;

    template< typename Arch >
    std::map< uint64_t, bstrs_t > shift_coerce_info(const Reg &s_reg, const Arch &arch)
    {
        std::map< uint64_t, bstrs_t > out;

        auto fetch_offset = [&](auto reg) -> uint64_t {
            if (auto orig = arch.RegisterByName(reg)) {
                auto big = orig->EnclosingRegister();
                return orig->offset - big->offset;
            }
            return 0ul;
        };

        for (auto &[reg, bits] : s_reg.translation_map) {
            auto key = fetch_offset(reg);

            for (auto &bstr : bits)
                out[key].push_back(s_reg.make_bitstring(bstr));
        }
        return out;
    }

    // Some regsiters may require additional masks (most notably smaller version
    // of its widest version - e.g. edi needs mask to be extracted from rdi)
    // Returns `[original size, enclosing size]`.
    template< typename Arch >
    auto mask_coerce_info(const Reg &s_reg, const Arch &arch)
    -> std::tuple< uint64_t, uint64_t >
    {
        std::map< std::tuple< uint64_t, uint64_t >, bstrs_t > out;
        std::vector< std::string > defaulted;

        auto defaults = [&](auto reg) -> bool { return !arch.RegisterByName(reg); };

        auto fetch_info = [&](auto reg) -> std::tuple< uint32_t, uint32_t > {
            if (auto orig = arch.RegisterByName(reg)) {
                auto big = orig->EnclosingRegister();
                return std::make_tuple(orig->size, big->size);
            }
            unreachable() << "Cannot fetch info for reg that is not in arch.";
        };

        for (auto &[reg, bits] : s_reg.translation_map) {
            if (defaults(reg)) {
                defaulted.push_back(reg);
                continue;
            }

            auto key = fetch_info(reg);
            for (auto &bstr : bits)
                out[key].push_back(s_reg.make_bitstring(bstr));
        }

        check(defaulted.size() <= 1) << s_reg.to_string();
        check(!out.empty()) << s_reg.to_string();
        // Check with extra dbg message
        if (out.size() != 1) {
            // Build dbg message
            std::stringstream ss;
            for (auto &[key, _] : out) {
                auto [x, y] = key;
                ss << "[ " << std::to_string(x) << " , " << std::to_string(y) << " ]";
            }
            unreachable() << "out.size() != 1\n" << ss.str() << " in:\n" << s_reg.to_string();
        }

        auto &[key, _] = *(out.begin());
        for (auto reg : defaulted)
            for (auto &bstr : s_reg.translation_map.find(reg)->second)
                out[key].push_back(s_reg.make_bitstring(bstr));

        return out.begin()->first;
    }

    llvm::Value *mask_coerce(llvm::Value *what, auto &irb,
                             const Reg &s_reg, const auto &arch)
    {
        auto [size, total_size] = mask_coerce_info(s_reg, arch);
        std::string mask_bits(size * 8, '1');
        llvm::APInt mask{ static_cast<uint32_t>(total_size * 8), mask_bits, 2};
        check(irb.getInt(mask)->getType() == what->getType());
        return irb.CreateAnd(what, irb.getInt(mask));
    }

    llvm::Value *shift_coerce(llvm::Value *what, auto &irb,
                              const Reg &s_reg, const auto &arch)
    {
        shadowinst::SelectMaker selects{ irb };
        for (auto &[shift_val, conds] : shift_coerce_info(s_reg, arch)) {
            std::vector<llvm::Value *> args;
            auto reg_selector = shadowinst::region_selector(irb, s_reg);
            for (auto &bstr : conds) {
                auto constant = llvm::APInt{static_cast<uint32_t>(bstr.size()), bstr, 2};
                args.push_back(irb.CreateICmpEQ(reg_selector, irb.getInt(constant)));
            }
            auto cond = make_or(irb, args);
            selects.chain(cond, irb.getIntN(arch.address_size, shift_val * 8));
        }
        return irb.CreateLShr(what, selects.get());
    }


    template< typename ...Args >
    llvm::Value *mask_shift_coerce(llvm::Value *what, Args && ...args)
    {
        return mask_coerce( shift_coerce( what, std::forward< Args >(args) ...),
                            std::forward< Args >(args) ...);
    }

    auto store_fragment(auto what, auto full, auto &irb,
                        const Reg &s_reg, const auto &arch)
    // [condition to be upheld by context, selector of value]
    -> std::tuple< llvm::Value *, llvm::Value * >
    {
        std::map< uint64_t, llvm::Value * > shifts;
        for (auto &[shift_val, conds] : shift_coerce_info(s_reg, arch)) {
            auto reg_selector = shadowinst::region_selector(irb, s_reg);
            shifts[shift_val * 8] = irops::is_one_of(irb, reg_selector, conds);
        }

        auto [size, ts] = mask_coerce_info(s_reg, arch);

        // TODO(lukas): Makes sense to check that size of `what` is size.

        std::map< llvm::Value *, llvm::Value * > cond_to_glued;
        for (auto &[shift_val, cond] : shifts) {
            std::vector< llvm::Value * > chunks;
            if (shift_val != 0)
                chunks.push_back(irops::make< irops::ExtractRaw >(irb, full,  0, shift_val));
            chunks.push_back(what);
            auto pos = shift_val + size * 8;
            if (ts * 8 - pos != 0)
                chunks.push_back(irops::make< irops::ExtractRaw >(irb, full, pos,
                                                                  ts * 8 - pos));
            cond_to_glued[cond] = irops::make< irops::Concat >(irb, chunks);
        }
        // TODO(lukas): Generalize.
        check(cond_to_glued.size() <= 2 && cond_to_glued.size() >= 1);

        auto &front = *cond_to_glued.begin();
        llvm::Value *init_cond = std::get< 0 >(front);
        llvm::Value *init_val = std::get< 1 >(front);
        cond_to_glued.erase(init_cond);

        for (auto &[c, v] : cond_to_glued) {
            init_cond = irb.CreateOr(c, init_cond);
            init_val = irb.CreateSelect(c, v, init_val);
        }
        return std::make_tuple(init_cond, init_val);
    }

} // namespace cir::shadowinst
