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

        private:
            static void print_results(std::ostream &os,
                                      const std::string &what,
                                      const reports_t &results)
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

            static std::string print_results(const std::string &what,
                                             const reports_t &results)
            {
                std::ostringstream os;
                print_results(os, what, results);
                return os.str();
            }

        public:

        std::string get_warnings() const { return print_results("warnings", warnings); }
        std::string get_errors() const { return print_results("errors", errors); }

        friend auto operator<<(std::ostream &os, const VerifierResult &self)
        -> decltype( os << "" )
        {
            self.print_results(os, "errors", self.errors);
            self.print_results(os, "warnings", self.warnings);
            return os;
        }
    };

    VerifierResult verify_ctxs_uniqueness(Circuit *circuit);

    VerifierResult verify_arity(Operation *circuit, bool recursive = true);
    VerifierResult verify_advices(Circuit *circuit);
    VerifierResult verify_decoder_result(Circuit *circuit);
    VerifierResult verify_ids(Circuit *circuit);

    // Really simple structural verifier
    struct Verifier
    {
        using self_t = Verifier;

        VerifierResult res;

        VerifierResult take() { return std::move(res); }

        self_t &run_all(Circuit *circuit)
        {
            run(verify_arity, circuit, true);
            run(verify_advices, circuit);
            run(verify_decoder_result, circuit);
            run(verify_ids, circuit);
            run(verify_ctxs_uniqueness, circuit);
            return *this;
        }

        template< typename F, typename ... Args >
        self_t &run(F &&f, Args && ... args)
        {
            res.merge(f(std::forward< Args >(args) ...));
            return *this;
        }

        using advice_users_t = std::unordered_map< Operation *,
                                                   std::unordered_set< Operation * > >;
        // TODO(lukas): Move out.
        static void CollectAdviceUsers(Operation *op, advice_users_t &collected )
        {
            for (auto child : op->operands) {
                if (child->op_code == Advice::kind)
                    collected[child].insert(op);
                else
                    CollectAdviceUsers(child, collected);
            }
        }

    };

    static inline VerifierResult verify_circuit(Circuit *circuit)
    {
        return Verifier().run_all(circuit).take();
    }

    template< bool PrintWarnings=false >
    static inline void VerifyCircuit(const std::string &prefix,
                                     Circuit *circuit,
                                     const std::string &suffix="Done.")
    {
        log_info() << prefix;
        auto res = Verifier().run_all(circuit).take();
        if (res.has_errors())
        {
            log_kill() << res;
        }
        if (PrintWarnings && res.has_warnings())
            log_info() << res;
        log_info() << suffix;
    }

} // namespave circuitous
