/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#include <circuitous/IR/Verify.hpp>

#include <circuitous/IR/Visitors.hpp>
#include <circuitous/Support/Check.hpp>

#include <algorithm>
#include <optional>
#include <vector>

namespace circ
{

    struct PartialEnc
    {
        using self_t = PartialEnc;
        using bits_t = std::vector< std::optional< int > >;

        bits_t bits;

        PartialEnc(std::size_t size) : bits(size, std::nullopt) {}

        auto &operator[](std::size_t idx) { return bits[idx]; }
        const auto &operator[](std::size_t idx) const { return bits[idx]; }
        auto size() const { return bits.size(); }

        self_t &define(std::size_t from_inc, std::size_t to_inc, std::vector< int > vals)
        {
            dcheck(vals.size() == to_inc - from_inc + 1, [](){
                    return "Inconsistent arguments to PartialEnc::define()!"; });

            for (std::size_t i = 0; from_inc + i <= to_inc; ++i)
            {
                dcheck(!bits[from_inc + i], [](){ return "Cannot re-define bits."; });
                bits[from_inc + i] = vals[i];
            }
            return *this;
        }

        std::string to_string() const
        {
            std::ostringstream ss;
            ss << *this;
            return ss.str();
        }

        friend auto operator<<(std::ostream &os, const self_t &self) -> decltype( os << "" )
        {
            std::size_t idx = 0;
            for (const auto &b : self.bits)
            {
                if (b)
                    os << *b;
                else
                    os << "-";

                if (++idx % 8 == 0)
                    os << " ";
            }
            return os;
        }
    };

    struct EncCollector : Visitor< EncCollector, true >
    {
        using parent_t = Visitor< EncCollector, true >;
        using self_t = EncCollector;

        PartialEnc enc;

        EncCollector(std::size_t size) : enc(size) {}

        PartialEnc take() { return std::move(enc); }

        self_t &visit(const Operation *op)
        {
            unreachable() << "EncCollector encountered unknown op:\n"
                          << pretty_print< true >(op);
        }

        self_t &visit(const DecoderResult *op)
        {
            for (auto c : op->operands())
                this->dispatch(c);
            return *this;
        }

        self_t &visit(const DecodeCondition *op)
        {
            auto convert = [](const auto &str) {
                std::vector< int > out;
                for (auto c : str)
                {
                    dcheck(c == '0' || c == '1', [](){ return "Unknown value in bits."; });
                    out.push_back((c == '0') ? 0 : 1);
                }
                return out;
            };

            auto expected = op->operand(0);
            auto extracted = op->operand(1);

            check(isa< Constant >(expected) && isa< Extract >(extracted));
            const auto &constant = static_cast< const Constant & >(*expected);
            const auto &extract = static_cast< const Extract & >(*extracted);
            enc.define(extract.low_bit_inc, extract.high_bit_exc - 1, convert(constant.bits));
            return *this;
        }

    };

    struct UniqnuessVerifier
    {
        using unproved_t = std::unordered_map< Operation *, std::unordered_set< Operation * > >;
        using encs_t = std::unordered_map< Operation *, PartialEnc >;

        encs_t encs;
        unproved_t unproved;


        UniqnuessVerifier(std::unordered_map< Operation *, PartialEnc > encs_)
            : encs(std::move(encs_))
        {
            populate_unproved();
        }

        void populate_unproved()
        {
            for (const auto &[ctx, enc] : encs)
            {
                for (auto &[op, y] : unproved)
                    y.insert(ctx);
                unproved[ctx] = {};
            }
        }

        bool join(const std::optional< int > &a, const std::optional< int > &b)
        {
            if (!a || !b)
                return false;

            return *a != *b;
        }

        void try_prove(std::size_t i)
        {
            auto prove = [&](auto a, auto b)
            {
                unproved[a].erase(b);
                unproved[b].erase(a);
            };

            for (const auto &[a_ctx, a_enc] : encs)
                for (const auto &[b_ctx, b_enc] : encs)
                {
                    if (join(a_enc[i], b_enc[i]))
                        prove(a_ctx, b_ctx);
                }
        }

        VerifierResult run()
        {
            dcheck(!encs.empty(), []() {
                    return "Need at least one context to verify uniqnuess."; });
            for (std::size_t i = 0; i < encs.begin()->second.size(); ++i)
                try_prove(i);
            auto x = collect_unproved();
            log_dbg() << stats();
            return x;
        }

        VerifierResult collect_unproved() const
        {
            VerifierResult out;
            for (const auto &[ctx, others] : unproved)
            {
                if (!others.empty())
                {
                    std::stringstream ss;
                    ss << pretty_print(ctx) << " has uniqnuess collisions with:\n";
                    for (auto o : others)
                        ss << "\t" << pretty_print(o);

                    ss << "Encodings:\n";
                    ss << encs.find(ctx)->second << "\n";
                    for (auto o : others)
                        ss << encs.find(o)->second << "\n";
                    out.add_error() << ss.str();
                }
            }
            return out;
        }

        std::string stats() const
        {
            std::stringstream ss;
            ss << "Encoding:\n";
            for (const auto &[ctx, enc] : encs)
            {
                ss << pretty_print(ctx) << "\n";
                ss << enc << "\n";
            }

            ss << "Unproved:\n";
            for (const auto &[ctx, others] : unproved)
            {
                ss << pretty_print(ctx) << "\n";
                for (auto o : others)
                    ss << "\t" << pretty_print< false >(o) << "\n";
            }
            return ss.str();
        }
    };

    Operation *get_decoder_result(VerifyInstruction *root)
    {
        auto out = root->decoder();
        check( out && *out );
        return *out;
    }

    VerifierResult verify_ctxs_uniqueness(Circuit *circuit)
    {
        std::unordered_map< Operation *, PartialEnc > all;
        for (auto ctx : circuit->attr< VerifyInstruction >())
        {
            all.emplace(ctx, EncCollector(15 * 8).dispatch(get_decoder_result(ctx)).take());
        }
        return UniqnuessVerifier(std::move(all)).run();
    }



    std::string log_src_dump(Operation *op, const std::string &gprefix = "")
    {
        std::stringstream ss;
        auto print_op = [&](auto root, const auto &prefix)
        {
            ss << prefix;
            if (auto src = root->get_meta(circir_llvm_meta::llvm_source_dump))
                ss << *src;
            else
                ss << "( no source meta )";
            ss << "\n";
        };

        print_op(op, gprefix);

        for (auto c : op->operands())
            print_op(c, gprefix + " |- ");

        return ss.str();
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
            res.add_error() << op_code_str(op->op_code)
                            << " has "
                            << op->operands_size() << " operands which is invalid.\n"
                            << log_src_dump(op, "\t")
                            << "Error entry end.\n";
        }

        template< typename Fn >
        void verify_op_count(uint64_t count, Operation *op, Fn fn)
        {
            if (!fn(count, op->operands_size()))
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


        void verify_single(Operation *op)
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
                case DecodeCondition::kind:
                case AdviceConstraint::kind:
                    return exactly(2, op);
                case ReadConstraint::kind:
                    return exactly(4, op);
                case WriteConstraint::kind:
                    return exactly(5, op);
                case Select::kind:
                    return exactly((1 << op->operand(0)->size) + 1, op);
                case Concat::kind:
                    return more_than(1, op);
                case VerifyInstruction::kind:
                case OnlyOneCondition::kind:
                case Circuit::kind:
                case Or::kind:
                case And::kind:
                case DecoderResult::kind:
                    return not_exactly(0, op);
                default:
                    break;
            }

            if (isa< leaf_values_ts >(op->op_code))
                return exactly(0, op);

            if (isa< llvm_ops_t >(op->op_code)) {
                switch (op->op_code) {
                    case Trunc::kind:
                    case ZExt::kind:
                    case SExt::kind:
                        return exactly(1, op);
                    default:
                        return exactly(2, op);
                }
            }
            unreachable() << "Cannot verify kind: " << op_code_str(op->op_code);
        }

        self_t &verify_arity(Circuit *circuit)
        {
            auto wrap = [&]( auto op ) { this->verify_single( op ); };
            circuit->for_each_operation( wrap );
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
            auto handle = [ & ]( Operation *ctx, AdviceConstraint *ac,
                                 std::unordered_set< Operation * > &already_constrained )
            {
                auto advice = ac->advice();
                if ( !advice )
                {
                    res.add_error() << "AdviceConstraint advice operand is not an advice."
                                    << " Got " << pretty_print< false >( ac->fixed() )
                                    << " instead.";
                    return;
                }

                if ( already_constrained.count( advice ) )
                {
                    res.add_error() << "Advice" << pretty_print< false >( advice )
                                    << " is constrained more than once in the same context!\n"
                                    << advice->id() << " in ctx " << ctx->id();
                }
                already_constrained.emplace( advice );
            };

            for ( auto verif : circuit->attr< VerifyInstruction >() )
            {
                std::unordered_set< Operation * > already_constrained;
                for ( auto ac : filter< AdviceConstraint >( verif->operands() ) )
                    handle( verif, ac, already_constrained );
            }

            for ( auto ac : circuit->attr< AdviceConstraint >() )
                for ( auto user : ac->users() )
                    if ( !isa< VerifyInstruction >( user ) )
                        res.add_error() << "AdviceConstraint has user that is "
                                        << "not top-level context!";
        }


        void verify_users( Circuit *circuit )
        {
            auto advice_to_str = [](auto op) {
                return pretty_print(op);
            };

            for ( auto a : circuit->attr< Advice >() )
            {
                auto users = freeze< std::vector >( a->users() );
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

                auto only_acs = [&]()
                {
                    for ( auto user : users )
                    {
                        auto ac = dyn_cast< AdviceConstraint >( user );
                        if ( !ac || ac->advice() != a )
                            return false;
                    }
                    return true;
                }();

                if ( only_acs )
                {
                    res.add_error() << advice_to_str(a)
                                    << " has only advice constraints as users";
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
            for (auto ctx : circuit->attr< VerifyInstruction >())
            {
                std::vector< Operation * > decoder_results;
                for (auto op : ctx->operands())
                    if (isa< DecoderResult >(op))
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

            for (auto op : current->operands())
                verify_constrained_subtree(root, op, allowed);
        }

        void verify_decoder_result(Circuit *circuit)
        {
            auto allowed_nodes = collect_kinds< DecodeCondition, Constant,
                                                DecoderResult, Extract,
                                                InputInstructionBits, And, Advice, Icmp_eq,
                                                Or, Xor >();
            for (auto root : circuit->attr< DecoderResult >())
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
            std::unordered_map< uint64_t, uint64_t > ids;

            auto collect_ids = [ & ]( auto op )
            {
                if ( !ids.count( op->id() ) )
                    ids[ op->id() ] = 0;
                ids[ op->id() ] += 1;
            };

            circuit->for_each_operation( collect_ids );


            for (auto &[id, count] : ids)
                if (count != 1)
                    res.add_error() << "ID: " << id << " is present " << count << " times.";
        }
    };

    struct DAGVerifier : OwnsResult
    {
        using self_t = DAGVerifier;
        using OwnsResult::OwnsResult;

        self_t &verify( Circuit *circuit )
        {
            if ( !circuit->root )
            {
                res.add_error() << "Circuit does not have root!";
                return *this;
            }
            loop_dfs( circuit->root );
            return *this;
        }

        void loop_dfs( Operation *root )
        {
            std::unordered_map< Operation *, bool > seen;
            using entry_t = std::tuple< Operation *, std::size_t >;
            std::vector< entry_t > stack;
            stack.emplace_back( root, 0 );


            auto err_with_bt = [ & ]( auto next )
            {
                std::stringstream ss;
                ss << "Was reachable: " << pretty_print( next ) << "\n";
                for ( auto &[ op, op_idx ] : stack )
                    ss << "\t" << pretty_print( op ) << "\n \t * " << op_idx - 1 << "\n";
                res.add_error() << "Cycle was found!\n" << ss.str();
            };

            while ( !stack.empty() )
            {
                auto [ op, idx ] = stack.back();
                stack.pop_back();

                if ( idx != op->operands_size() )
                {
                    stack.emplace_back( op, idx + 1 );
                    auto next = op->operand( idx );
                    auto it = seen.find( next );
                    if ( it == seen.end() )
                    {
                        seen[ op ] = true;
                        stack.emplace_back( next, 0 );
                    } else if ( it->second == true ) {
                        err_with_bt( next );
                    }
                } else {
                    seen[ op ] = false;
                }

            }

        }

        void dfs( Operation *op, std::unordered_map< Operation *, bool > &seen )
        {
            seen[ op ] = true;

            for ( auto o : op->operands() )
            {
                auto it = seen.find( o );
                if ( it == seen.end() )
                    dfs( o, seen );
                else if ( it->second == true )
                    res.add_error() << "Cycle found: "
                                    << pretty_print( op ) << " was able to reach back at "
                                    << pretty_print( o );
            }

            seen[ op ] = false;

        }
    };


    VerifierResult verify_arity( Circuit *circuit )
    {
        return ArityVerifier().verify_arity( circuit ).take();
    }

    VerifierResult verify_advices( Circuit *circuit )
    {
        return AdviceVerifier().verify( circuit ).take();
    }

    VerifierResult verify_decoder_result( Circuit *circuit )
    {
        return DecoderResultVerifier().verify( circuit ).take();
    }

    VerifierResult verify_ids( Circuit *circuit )
    {
        return IDVerifier().verify( circuit ).take();
    }

    VerifierResult verify_dag( Circuit *circuit )
    {
        return DAGVerifier().verify( circuit ).take();
    }

}  // namespace circ
