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



    std::string log_src_dump(Operation *op, const std::string &gprefix = "")
    {
        std::stringstream ss;
        auto print_op = [&](auto root, const auto &prefix)
        {
            ss << prefix;
            if (auto src = op->get_meta(circir_llvm_meta::llvm_source_dump))
                ss << *src;
            else
                ss << "( no source meta )";
            ss << "\n";
            return ss.str();
        };

        std::string out = print_op(op, gprefix);

        for (auto c : op->operands)
            out += print_op(c, gprefix + " |- ");
        return out;
    }


    struct OwnsResult
    {
        VerifierResult res;
        OwnsResult() = default;

        VerifierResult take() { return std::move(res); }
    };

    struct ArityVerifier : OwnsResult
    {
        using self_t = ArityVerifier;

        using OwnsResult::OwnsResult;

        void log_operand_mismatch(Operation *op)
        {
            res.add_error() << to_string(op->op_code) << " has "
                            << op->operands.Size() << " operands which is invalid.\n"
                            << log_src_dump(op, "\t")
                            << "Error entry end.\n";
        }

        template< typename Fn >
        void verify_op_count(uint64_t count, Operation *op, Fn fn)
        {
            if (!fn(count, op->operands.Size()))
                log_operand_mismatch(op);
        }

        auto exactly(uint64_t count, Operation *op)
        {
            return verify_op_count(count, op, [](auto ex, auto ac){ return  ex == ac; });
        }

        auto not_exactly(uint64_t count, Operation *op)
        {
            return verify_op_count(count, op, [](auto ex, auto ac){ return  ex != ac; });
        }

        auto more_than(uint64_t count, Operation *op)
        {
            return verify_op_count(count, op, [](auto ex, auto ac){ return  ex <= ac; });
        }


        void verify_arity(Operation *op)
        {
            dcheck(op, [](){ return "Expected valid pointer."; });
            switch (op->op_code)
            {
                case Constant::kind:
                case Undefined::kind:
                case InputRegister::kind:
                case OutputRegister::kind:
                case InputInstructionBits::kind:
                case Advice::kind:
                case InputErrorFlag::kind:
                case OutputErrorFlag::kind:
                    return exactly(0, op);
                case InputImmediate::kind:
                case Not::kind:
                case PopulationCount::kind:
                case Parity::kind:
                case CountLeadingZeroes::kind:
                case CountTrailingZeroes::kind:
                case Extract::kind:
                case UnusedConstraint::kind:
                    return exactly(1, op);
                case RegConstraint::kind:
                case PreservedConstraint::kind:
                case CopyConstraint::kind:
                case DecodeCondition::kind:
                case AdviceConstraint::kind:
                    return exactly(2, op);
                case ReadConstraint::kind:
                    return exactly(4, op);
                case WriteConstraint::kind:
                    return exactly(5, op);
                case Select::kind:
                    return exactly((1 << op->operands[0]->size) + 1, op);
                case Concat::kind:
                    return more_than(1, op);
                case VerifyInstruction::kind:
                case OnlyOneCondition::kind:
                case Circuit::kind:
                case Or::kind:
                case And::kind:
                case DecoderResult::kind:
                    return not_exactly(0, op);
            }

            if (is_specialization< LeafValue >(op->op_code))
                return exactly(0, op);

            if (is_specialization<Computational>(op->op_code)) {
                switch (op->op_code) {
                    case Trunc::kind:
                    case ZExt::kind:
                    case SExt::kind:
                        return exactly(1, op);
                    case BSelect::kind:
                        return exactly(3, op);
                    default:
                        return exactly(2, op);
                }
            }
            unreachable() << "Cannot verify kind: " << to_string(op->op_code);
        }

        self_t &verify_arity(Operation *op, bool recursive)
        {
            dcheck(op, [](){ return "Expected valid pointer."; });
            verify_arity(op);
            if (!recursive)
                return *this;

            for (auto o : op->operands)
                verify_arity(o, recursive);

            return *this;
        }
    };

    struct AdviceVerifier : OwnsResult
    {
        using self_t = AdviceVerifier;

        using OwnsResult::OwnsResult;

        self_t &verify(Circuit *circuit)
        {
            verify_constraints(circuit);
            verify_users(circuit);
            return *this;
        }

        // We try to check if there is not a ADVICE with more than one constraint
        // in the same context, since that is an error.
        void verify_constraints(Circuit *circuit)
        {
            using verifies_t = std::unordered_set< VerifyInstruction * >;
            std::unordered_map< Operation *, verifies_t > hint_to_ctxs;
            std::unordered_map< Operation *, VerifyInstruction * > ctx;

            for (auto verif : circuit->Attr<VerifyInstruction>()) {
                for (auto op : verif->operands) {
                    if (op->op_code == AdviceConstraint::kind)
                    {
                        ctx[op] = verif;

                        if (op->operands[AdviceConstraint::kFixed]->op_code != Advice::kind)
                            res.add_error() << "ADVICE_CONSTRAINT hint operand is not ADVICE.";

                        bool found_hint = false;
                        for (auto hint : op->operands)
                        {
                            if (hint->op_code == Advice::kind)
                            {
                                if (found_hint)
                                    res.add_warning() << "ADVICE_CONSTRAINT has at least"
                                                      << "two direct ADVICE operands!";

                                found_hint = true;
                                if (hint_to_ctxs[hint].count(verif))
                                {
                                    res.add_error() << "Advice is under two hint checks"
                                                    << "in the same context!";
                                }
                            }
                        }
                        if (!found_hint)
                            res.add_error() << "ADVICE_CONSTRAINT does not have ADVICE"
                                            << "as direct operand!\n";
                    }
                }
            }
        }


        void verify_users(Operation *circuit)
        {
            Verifier::advice_users_t collected;
            Verifier::CollectAdviceUsers(circuit, collected);

            auto advice_to_str = [](auto op) {
                return "ADVICE [ " + std::to_string(op->id()) + " ]";
            };

            for (auto &[a, users] : collected)
            {
                if (users.size() == 0)
                    res.add_error() << advice_to_str(a) << " has no users.";

                if (users.size() == 1)
                {
                    if ((*users.begin())->op_code == AdviceConstraint::kind)
                        res.add_error() << advice_to_str(a)
                                        << " has only one user but it is a constraint.";
                    else
                        res.add_error() << advice_to_str(a)
                                        << " has only one user that is not a constraint.";
                }
            }
        }


    };

    struct DecoderResultVerifier : OwnsResult
    {
        using self_t = DecoderResultVerifier;
        using OwnsResult::OwnsResult;

        self_t &verify(Circuit *circuit)
        {
            verify_decoder_result(circuit);
            verify_decoder_result_presence(circuit);
            return *this;
        }

        void verify_decoder_result_presence(Circuit *circuit)
        {
            for (auto ctx : circuit->Attr< VerifyInstruction >())
            {
                std::vector< Operation * > decoder_results;
                for (auto op : ctx->operands)
                    if (is_of< DecoderResult >(op))
                        decoder_results.push_back(op);
                if (decoder_results.size() == 0)
                {
                    res.add_error() << "Conext:\n" << pretty_print< false >(ctx)
                                    << "has no DecoderResult operand";
                } else if (decoder_results.size() > 1) {
                    res.add_error() << "Context:\n" << pretty_print< false >(ctx)
                                    << "has " << decoder_results.size()
                                    << " DecoderResult operand";
                }
            }
        }

        // `root` is passed only for error messages.
        void verify_constrained_subtree(Operation *root, Operation *current,
                                        const std::unordered_set< Operation::kind_t > &allowed)
        {
            if (!allowed.count(current->op_code))
            {
                res.add_error() << pretty_print< true >(root)
                                << "\t -- has in its subtree operation that "
                                << "is not allowed:\n\t" << pretty_print< true >(current);
            }

            for (auto op : current->operands)
                verify_constrained_subtree(root, op, allowed);
        }

        void verify_decoder_result(Circuit *circuit)
        {
            auto allowed_nodes = collect_kinds< DecodeCondition, Constant,
                                                DecoderResult, Extract,
                                                InputInstructionBits >();
            for (auto root : circuit->Attr< DecoderResult >())
                verify_constrained_subtree(root, root, allowed_nodes);
        }
    };

    struct IDVerifier : OwnsResult
    {
        using self_t = IDVerifier;
        using OwnsResult::OwnsResult;

        self_t &verify(Circuit *circuit)
        {
            verify_ids(circuit);
            return *this;
        }

        void verify_ids(Circuit *circuit)
        {
            std::unordered_set< Operation * > seen;
            std::unordered_map< uint64_t, uint64_t > ids;
            collect_ids(circuit, seen, ids);

            for (auto &[id, count] : ids)
            {
                if (count != 1)
                    res.add_error() << "ID: " << id << " is present " << count << " times.";
            }
        }

        void collect_ids(Operation *op,
                         std::unordered_set< Operation * > &seen,
                         std::unordered_map< uint64_t, uint64_t > &ids)
        {
            if (seen.count(op))
                return;
            seen.insert(op);

            if (ids.count(op->id()))
                ids[op->id()] += 1;
            else
                ids[op->id()] = 1;

            for (auto o : op->operands)
                collect_ids(o, seen, ids);
        }
    };


    VerifierResult verify_arity(Operation *op, bool recursive)
    {
        return ArityVerifier().verify_arity(op, recursive).take();
    }

    VerifierResult verify_advices(Circuit *circuit)
    {
        return AdviceVerifier().verify(circuit).take();
    }

    VerifierResult verify_decoder_result(Circuit *circuit)
    {
        return DecoderResultVerifier().verify(circuit).take();
    }

    VerifierResult verify_ids(Circuit *circuit)
    {
        return IDVerifier().verify(circuit).take();
    }

}  // namespace circ
