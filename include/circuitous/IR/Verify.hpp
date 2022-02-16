/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <cstdint>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Shapes.hpp>
#include <circuitous/Support/Check.hpp>

namespace circ
{

    // Really simple structural verifier
    struct Verifier
    {
        std::stringstream ss;
        std::stringstream _warnings;

        bool status = true;

        std::string Report() { return ss.str(); }

        bool LogError(Operation *op)
        {
            ss << to_string(op->op_code) << " has "
               << op->operands.Size() << " operands which is invalid.\n";
            return false;
        }

        bool Undef(Operation *op)
        {
            ss << op->Name() << " is not supported.\n";
            return false;
        }

        template< typename Fn >
        bool Verify(uint64_t count, Operation *op, Fn fn)
        {
            if (!fn(count, op->operands.Size()))
                return LogError(op);
            return true;
        }

        bool Exactly(uint64_t count, Operation *op)
        {
            return Verify(count, op, [](auto ex, auto ac){ return  ex == ac; });
        }

        bool Not(uint64_t count, Operation *op)
        {
            return Verify(count, op, [](auto ex, auto ac){ return  ex != ac; });
        }

        bool MoreThan(uint64_t count, Operation *op)
        {
            return Verify(count, op, [](auto ex, auto ac){ return  ex <= ac; });
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
                    return Exactly(0, op);
                case InputImmediate::kind:
                case Not::kind:
                case PopulationCount::kind:
                case Parity::kind:
                case CountLeadingZeroes::kind:
                case CountTrailingZeroes::kind:
                case Extract::kind:
                case UnusedConstraint::kind:
                    return Exactly(1, op);
                case RegConstraint::kind:
                case PreservedConstraint::kind:
                case CopyConstraint::kind:
                case DecodeCondition::kind:
                case AdviceConstraint::kind:
                    return Exactly(2, op);
                case ReadConstraint::kind:
                    return Exactly(4, op);
                case WriteConstraint::kind:
                    return Exactly(5, op);
                case Select::kind:
                    return Exactly((1 << op->operands[0]->size) + 1, op);
                case Concat::kind:
                    return MoreThan(1, op);
                case VerifyInstruction::kind:
                case OnlyOneCondition::kind:
                case Circuit::kind:
                case Or::kind:
                case And::kind:
                    return Not(0, op);
            }

            if (is_specialization<LeafValue>(op->op_code))
                return Exactly(0, op);

            if (is_specialization<Computational>(op->op_code)) {
                switch (op->op_code) {
                    case Trunc::kind:
                    case ZExt::kind:
                    case SExt::kind:
                        return Exactly(1, op);
                    case BSelect::kind:
                        return Exactly(3, op);
                    default:
                        return Exactly(2, op);
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

    };

    // Check if circuit has some really basic structural integrity, and return
    // a `( result, error messages )`.
    static inline std::tuple<bool, std::string, std::string> VerifyCircuit(Circuit *circuit)
    {
        Verifier verifier;
        verifier.Verify(circuit);
        verifier.VerifyAdvices(circuit);
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
