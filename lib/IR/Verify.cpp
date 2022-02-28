/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/IR/Verify.hpp>

#include <circuitous/IR/Visitors.hpp>

#include <vector>
#include <optional>



namespace circ
{

    using bits_t = std::vector< std::optional< int > >;

    struct EncCollector : Visitor< EncCollector, true >
    {
        using parent_t = Visitor< EncCollector, true >;
        using self_t = EncCollector;

        bits_t bits;

        bits_t take() { return std::move(bits); }

        self_t &Visit(const Operation *op)
        {
            unreachable() << "EncCollector encountered unknown op:\n"
                          << pretty_print< true >(op);
        }

        self_t &Visit(const DecoderResult *op)
        {
            for (auto c : op->operands)
                this->Dispatch(c);
            return *this;
        }

        self_t &Visit(const DecodeCondition *op)
        {
            auto expected = op->operands[0];
            auto extracted = op->operands[1];

            check(is_of< Constant >(expected) && is_of< Extract >(extracted));
            return *this;
        }

    };

    Operation *get_decoder_result(Operation *root)
    {
        Operation *out = nullptr;
        for (auto op : root->operands)
        {
            if (is_of< DecoderResult >(op))
            {
                check(!out);
                out = op;
            }
        }
        return out;
    }

    bool verify_context_uniqueness(Circuit *circuit)
    {
        std::unordered_map< Operation *, bits_t > all;
        for (auto ctx : circuit->Attr< VerifyInstruction >())
        {
            all[ctx] = EncCollector().Dispatch(get_decoder_result(ctx)).take();
        }
        return all.size() == 3;
    }

}  // namespace circ
