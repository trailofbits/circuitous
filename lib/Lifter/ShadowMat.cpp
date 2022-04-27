/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/Lifter/ShadowMat.hpp>
#include <circuitous/Lifter/Context.hpp>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Util/Logging.hpp>

namespace circ::shadowinst
{

    llvm::Type *Materializer::selector_type() const
    {
        return irb.getIntNTy(static_cast< uint32_t >(s_reg.region_bitsize()));
    }

    llvm::Value *Materializer::region_selector() const
    {
        std::vector<llvm::Value *> input_fragments;

        for (auto &[from, size] : s_reg.regions) {
            auto extract = irops::make_leaf< irops::Extract >(irb, from, size);
            input_fragments.push_back(extract);
        }
        // We need to match the order of the entry in `translation_map`
        std::reverse(input_fragments.begin(), input_fragments.end());
        return irops::make< irops::Concat >(irb, input_fragments);
    }


    llvm::Value *Materializer::opaque_selector() const
    {
        return irops::make_leaf< irops::RegSelector >(irb, *s_reg.selector, selector_type());
    }

    llvm::Value *Materializer::tie_opaque_selector() const
    {
        return irops::make< irops::AdviceConstraint >(irb, { region_selector(),
                                                             opaque_selector() } );
    }

    llvm::Value *Materializer::was_decoded() const
    {
        check(s_reg.selector);
        return irops::make_leaf< irops::WasDecoded >(irb, *s_reg.selector,
                                                     irb.getTrue()->getType());
    }

    auto Materializer::translation_entries(const mats_t &mats) const
    -> values_t
    {
        std::vector<llvm::Value *> translation_checks;
        auto selector = opaque_selector();
        for (auto &mat : mats)
        {
            auto expected_value = irb.getInt(make_APInt(mat, 0, s_reg.region_bitsize()));
            translation_checks.push_back(irb.CreateICmpEQ(selector, expected_value));
        }
        return translation_checks;
    }

} // namespace circ::shadowinst
