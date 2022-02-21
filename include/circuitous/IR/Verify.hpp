/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <cstdint>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Metadata.hpp>
#include <circuitous/IR/Shapes.hpp>
#include <circuitous/Support/Check.hpp>

namespace circ
{

    struct VerifierResult
    {
        using report_t = std::string;
        using reports_t = std::vector< report_t >;

        reports_t errors;
        reports_t warnings;

        struct Message
        {
            std::stringstream ss;
            reports_t sink;

            Message(reports_t &sink_) : sink(sink_) {}
            Message(const Message &) = delete;
            Message(Message &&) = delete;
            Message &operator=(Message) = delete;
            ~Message() { sink.push_back(ss.str() + "\n"); }

            template< typename T >
            Message &operator<<(T &&t)
            {
                ss << std::forward< T >(t);
                return *this;
            }
        };

        auto add_warning() { return Message(warnings); }
        auto add_error() { return Message(errors); }

        bool has_errors() { return !errors.empty(); }
        bool has_warnings() { return !warnings.empty(); }

        void merge(VerifierResult &&other)
        {
            auto do_move = [](auto &to, auto &from)
            {
                to.insert(to.end(),
                          std::make_move_iterator(from.begin()),
                          std::make_move_iterator(from.end()));
            };
            do_move(errors, other.errors);
            do_move(warnings, other.warnings);
        }

        friend auto operator<<(std::ostream &os, const VerifierResult &self)
        -> decltype( os << "" )
        {
            auto print_results = [&](const auto &what, const auto &results)
            {
                if (results.empty())
                {
                    os << "No " << what << " found." << std::endl;
                    return;
                }

                os << what << ":" << std::endl;
                for (const auto &result : results)
                    os << " * " << result << std::endl;
            };

            print_results("errors", self.errors);
            print_results("warnings", self.warnings);
            return os;
        }
    };


    // Really simple structural verifier
    struct Verifier
    {
        std::stringstream ss;
        std::stringstream _warnings;

        bool status = true;

        std::string Report() { return ss.str(); }

        // TODO(lukas): Just rework whole verifier to use `VerifierResult`.
        void account(VerifierResult &&result)
        {
            if (result.has_errors())
                status &= false;
            for (auto &&err : result.errors)
                ss << std::move(err);
            for (auto &&warn : result.warnings)
                _warnings << std::move(warn);
        }

        void log_src_dump(Operation *op, const std::string &gprefix = "")
        {
            auto print_op = [&](auto root, const auto &prefix)
            {
                ss << prefix;
                if (auto src = op->get_meta(circir_llvm_meta::llvm_source_dump))
                    ss << *src;
                else
                    ss << "( no source meta )";
                ss << std::endl;
            };

            print_op(op, gprefix);

            for (auto c : op->operands)
                print_op(c, gprefix + " |- ");
        }

        bool log_operand_mismatch(Operation *op)
        {
            ss << to_string(op->op_code) << " has "
               << op->operands.Size() << " operands which is invalid.\n";
            log_src_dump(op, "\t");
            ss << "Error entry end.\n";
            return false;
        }

        bool Undef(Operation *op)
        {
            ss << op->Name() << " is not supported.\n";
            return false;
        }

        template< typename Fn >
        bool verify_op_count(uint64_t count, Operation *op, Fn fn)
        {
            if (!fn(count, op->operands.Size()))
                return log_operand_mismatch(op);
            return true;
        }

        bool exactly(uint64_t count, Operation *op)
        {
            return verify_op_count(count, op, [](auto ex, auto ac){ return  ex == ac; });
        }

        bool not_exactly(uint64_t count, Operation *op)
        {
            return verify_op_count(count, op, [](auto ex, auto ac){ return  ex != ac; });
        }

        bool more_than(uint64_t count, Operation *op)
        {
            return verify_op_count(count, op, [](auto ex, auto ac){ return  ex <= ac; });
        }


        bool VerifyArity(Operation *op)
        {
            check(op);
            switch (op->op_code) {
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

            if (is_specialization<LeafValue>(op->op_code))
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

        bool Verify(Operation *op)
        {
            check(op);
            status &= VerifyArity(op);
            for (auto o : op->operands)
                status &= Verify(o);
            return status;
        }

        void VerifyAdvices(Circuit *circuit)
        {
            status &= VerifyAdviceChecks(circuit);
            status &= VerifyAdviceUsers(circuit);
            status &= VerifyAdviceCtxs(circuit);
        }

        bool VerifyAdviceCtxs(Circuit *circuit)
        {
            CtxCollector collector;
            collector.Run(circuit);

            bool out = true;
            for (auto hint_check : circuit->Attr<AdviceConstraint>()) {
                if (collector.op_to_ctxs[hint_check].size() != 1)
                {
                    _warnings << "ADVICE_CONSTRAINT is member of multiple contexts.\n";
                    out &= true;
                }
            }
            return out;
        }

        // We try to check if there is not a ADVICE with more than one constraint
        // in the same context, since that is an error.
        bool VerifyAdviceChecks(Circuit *circuit)
        {
            bool out = true;
            std::unordered_map< Operation *,
                                std::unordered_set<VerifyInstruction * > > hint_to_ctxs;

            std::unordered_map<Operation *, VerifyInstruction *> ctx;
            for (auto verif : circuit->Attr<VerifyInstruction>()) {
                for (auto op : verif->operands) {
                    if (op->op_code == AdviceConstraint::kind)
                    {
                        ctx[op] = verif;

                        if (op->operands[AdviceConstraint::kFixed]->op_code != Advice::kind)
                        {
                            ss << "ADVICE_CONSTRAINT hint operand is not ADVICE\n";
                            status = false;
                        }

                        bool found_hint = false;
                        for (auto hint : op->operands)
                        {
                            if (hint->op_code == Advice::kind)
                            {
                                if (found_hint)
                                {
                                    _warnings << "ADVICE_CONSTRAINT has at least"
                                              << "two direct ADVICE operands!\n";
                                }

                                found_hint = true;
                                if (hint_to_ctxs[hint].count(verif))
                                {
                                    ss << "Advice is under two hint checks"
                                       << "in the same context!\n";
                                    status = false;
                                }
                            }
                        }
                        if (!found_hint)
                        {
                            ss << "ADVICE_CONSTRAINT does not have ADVICE as direct operand!\n";
                            status = false;
                        }
                    }
                }
            }
            return out;
        }

        using advice_users_t = std::unordered_map< Operation *,
                                                   std::unordered_set< Operation * > >;
        static void CollectAdviceUsers(Operation *op, advice_users_t &collected )
        {
            for (auto child : op->operands) {
                if (child->op_code == Advice::kind)
                    collected[child].insert(op);
                else
                    CollectAdviceUsers(child, collected);
            }
        }

        bool VerifyAdviceUsers(Operation *circuit)
        {
            advice_users_t collected;
            CollectAdviceUsers(circuit, collected);

            auto advice_to_str = [](auto op) {
                return "ADVICE [ " + std::to_string(op->id()) + " ]";
            };

            bool out = true;
            for (auto &[a, users] : collected) {
                if (users.size() == 0) {
                    ss << advice_to_str(a) << " has no users.";
                    out &= false;
                }
                if (users.size() == 1) {
                    if ((*users.begin())->op_code == AdviceConstraint::kind)
                        ss << advice_to_str(a) << " has only one user but it is a constraint.";
                    else
                        ss << advice_to_str(a)
                           << " has only one user that is not a constraint.";
                    out &= false;
                }
            }
            return out;
        }


        void VerifyIDs(Circuit *circuit)
        {
            std::unordered_set<Operation *> seen;
            std::unordered_map<uint64_t, uint64_t> ids;
            CollectIDs(circuit, seen, ids);
            for (auto &[id, count] : ids)
            {
                if (count != 1) {
                    status &= false;
                    ss << "ID: " << id << " is present " << count << " times.\n";
                }
            }
        }

        void CollectIDs(Operation *op,
                        std::unordered_set<Operation *> &seen,
                        std::unordered_map<uint64_t, uint64_t> &ids)
        {
            if (seen.count(op))
                return;
            seen.insert(op);

            if (ids.count(op->id()))
                ids[op->id()] += 1;
            else
                ids[op->id()] = 1;

            for (auto o : op->operands)
                CollectIDs(o, seen, ids);
        }

        // `root` is passed only for error messages.
        void verify_constrained_subtree(Operation *root, Operation *current,
                                        const std::unordered_set< Operation::kind_t > &allowed)
        {
            if (!allowed.count(current->op_code))
            {
                ss << pretty_print< true >(root) << "\t -- has in its subtree operation that "
                   << "is not allowed:\n\t" << pretty_print< true >(current);
                status &= false;
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

    // Check if circuit has some really basic structural integrity, and return
    // a `( result, error messages )`.
    static inline std::tuple<bool, std::string, std::string> VerifyCircuit(Circuit *circuit)
    {
        Verifier verifier;
        verifier.Verify(circuit);
        verifier.VerifyAdvices(circuit);
        verifier.verify_decoder_result(circuit);
        verifier.VerifyIDs(circuit);
        return {verifier.status, verifier.Report(), verifier._warnings.str() };
    }

    template< bool PrintWarnings=false >
    static inline void VerifyCircuit(const std::string &prefix,
                                     Circuit *circuit,
                                     const std::string &suffix="Done.")
    {
        log_info() << prefix;
        const auto &[status, msg, warnings] = VerifyCircuit(circuit);
        if (!status)
        {
            log_kill() << "WARNINGS:\n" << warnings << "\n"
                          << "FATAL ERRORS:\n" << msg << "\n"
                          << "Circuit is invalid";
        }
        if (PrintWarnings)
            log_info() << warnings;;
        log_info() << suffix;
    }

} // namespave circuitous
