/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Logging.hpp>
#include <circuitous/Support/Check.hpp>

#include <remill/BC/Compat/CallSite.h>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Verifier.h>
#include <llvm/CodeGen/IntrinsicLowering.h>
#include <llvm/Transforms/Utils/Cloning.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <circuitous/Lifter/Components/Decoder.hpp>

#include <sstream>

namespace circ::build
{
    std::string Decoder::generate_raw_bytes(const std::string &full, uint64_t from, uint64_t to)
    {
        auto n = full.substr(from, to - from);
        std::reverse(n.begin(), n.end());
        return n;
    }

    llvm::Value *Decoder::create_bit_check(uint64_t from, uint64_t to)
    {
        auto encoding = convert_encoding(isel.encoding);
        std::string expected = generate_raw_bytes(encoding, from, to);

        auto size = static_cast< uint32_t >(expected.size());
        check(size == to - from) << size << " != " << to - from;

        auto expected_v = ir.getInt(llvm::APInt(size, expected, 2));
        auto extracted = irops::make_leaf< irops::ExtractRaw >(ir, from, to - from);
        return irops::make< irops::DecodeCondition >(ir, {expected_v, extracted}, size);
    }

    auto Decoder::byte_fragments() -> values_t
    {
        values_t out;

        auto unknown_regions = isel.shadow.UnknownRegions(rinst_size());
        // `unknown_regions` are in `[ from, size ]` format.
        for (auto [from, to] : shadowinst::FromToFormat(unknown_regions))
            out.push_back(create_bit_check(from, to));

        // TODO(lukas): Now we need to check the tail.
        //              Try to lift `6689d8` and `89d8` to demonstrate the issue.
        // TODO(lukas): For now we assume it is padded with 0s.
        auto tail_size = static_cast< uint32_t >(kMaxNumInstBits - rinst_size());
        auto tail = ir.getInt(llvm::APInt(tail_size, 0, false));

        auto extracted = irops::make_leaf< irops::ExtractRaw >(ir, rinst_size(), tail_size);
        auto compare = irops::make< irops::DecodeCondition >(ir, {tail, extracted}, tail_size);
        out.push_back(compare);
        return out;
    }

    auto Decoder::get_decoder_tree()
    -> std::tuple< llvm::Value *, std::vector< llvm::Value * > >
    {
        auto all_fragments = byte_fragments();
        if (auto trees = emit_translation_trees(); !trees.empty())
        {
            auto translations = irops::make< irops::And >(ir,trees);
            all_fragments.push_back(translations);
        }
        return { irops::make< irops::DecoderResult >(ir, all_fragments), to_verify };
    }

    auto Decoder::emit_translation_trees() -> values_t
    {
        values_t out;
        for (const auto &s_op : isel.shadow.operands)
        {
            if (s_op.reg())
                out.push_back(emit_translation_tree(*s_op.reg()));
            if (s_op.address())
            {
                if (s_op.address()->base_reg())
                    out.push_back(emit_translation_tree(*s_op.address()->base_reg()));
                if (s_op.address()->index_reg())
                    out.push_back(emit_translation_tree(*s_op.address()->index_reg()));
                if (s_op.address()->segment_reg())
                    out.push_back(emit_translation_tree(*s_op.address()->segment_reg()));
            }
        }
        return out;
    }

    llvm::Value *Decoder::emit_translation_tree(const shadowinst::Reg &sreg)
    {
        if (sreg.regions.marked_size() == 0)
            return ir.getTrue();

        auto materializer = shadowinst::Materializer(ir, sreg);
        auto selector = materializer.opaque_selector();
        to_verify.push_back(materializer.tie_opaque_selector());

        if (sreg.tm().is_saturated())
            return ir.getTrue();


        values_t conds;
        for (auto &val : sreg.tm().complement())
            conds.push_back(ir.CreateICmpEQ(selector, ir.getInt(val)));
        return ir.CreateNot(irops::make< irops::And >(ir, conds));
    }

}  // namespace circ::build
