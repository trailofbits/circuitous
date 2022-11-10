/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Memory.hpp>
#include <circuitous/IR/Trace.hpp>
#include <circuitous/IR/Visitors.hpp>

#include <circuitous/Support/Check.hpp>

#include <gap/core/ranges.hpp>

#include <cmath>
#include <deque>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>

namespace circ::print::verilog
{

    namespace impl
    {
        template< typename I > requires std::is_integral_v< I >
        std::string wire_size(I size)
        {
            std::stringstream ss;
            ss << "[" << size - 1 << ":0]";
            return ss.str();
        }

        template< typename I > requires std::is_integral_v< I >
        std::string get_bit(const std::string &op, I idx)
        {
            std::stringstream ss;
            ss << op << "[" << idx << ":" << idx << "]";
            return ss.str();
        }

        static inline std::string to_verilog(auto size, auto value)
        {
            std::stringstream ss;
            ss << size << "'d" << value;
            return ss.str();
        }

        static inline std::string bin_zero(auto size)
        {
            return std::to_string(size) + "'b" + std::string(size, '0');
        }

        static inline std::string bin_one(auto size)
        {
            return std::to_string(size) + "'b" + std::string(size, '1');
        }

        static inline std::string true_val() { return "1'b1"; }
        static inline std::string false_val() { return "1'b0"; }

        static inline std::string wire_name(Operation *op)
        {
            std::stringstream ss;
            ss << std::hex << "v" << op->id();
            return ss.str();
        }

        static inline std::string wire_decl(const std::string &name, std::string lhs, auto size)
        {
            std::stringstream ss;
            ss << "wire ";
            if (size != 1)
                ss << impl::wire_size(size) << " ";
            ss << name + " = " + lhs + ";\n";
            return ss.str();
        }

        static inline std::string to_signed(const std::string &what)
        {
            return "$signed(" + what + ")";
        }

    } // namespace impl

    struct dbg_verbose
    {
        std::stringstream _dbg;
    };

    struct ToStream : dbg_verbose
    {
        std::ostream &_os;
        Circuit *circuit;
        std::unordered_map< Operation *, std::string > op_names;

        ToStream(std::ostream &os_, Circuit *circuit_) : _os(os_), circuit(circuit_) {}

        auto &os() { return _os; }
        auto &dbg() { return _dbg; }

        std::string &give_name(Operation *op, std::string name)
        {
            auto [it, flag] = op_names.emplace(op, std::move(name));
            check(flag, [&](){ return "\n" + dbg().str(); });
            return it->second;
        }

        bool has_name(Operation *op) { return op_names.count(op); }

        std::optional< std::string > get_name(Operation *op)
        {
            auto it = op_names.find(op);
            if (it != op_names.end())
                return { it->second };
            return {};
        }
    };

    template< typename Ctx >
    struct OpFmt : Visitor< OpFmt< Ctx > >
    {
        Ctx &ctx;

        OpFmt(Ctx &ctx_) : ctx(ctx_) {}

        std::string get(Operation *op)
        {
            if (auto name = ctx.get_name(op))
                return *name;
            // `op` does not have a name -> create it and name it
            return ctx.give_name(op, this->dispatch(op));
        }

        template< typename I > requires std::is_integral_v< I >
        std::string get_bit(Operation * op, I idx)
        {
            return impl::get_bit(get(op), idx);
        }

        std::string make_wire(const std::string name, std::string lhs, auto size)
        {
            ctx.os() << impl::wire_decl(name, std::move(lhs), size);
            return name;
        }

        std::string make_wire(Operation *op, std::string lhs)
        {
            return make_wire(impl::wire_name(op), std::move(lhs), op->size);
        }

        std::string concat(const std::vector< std::string > &ops)
        {
            std::stringstream ss;
            ss << "{ " << ops[0];
            for (std::size_t i = 1; i < ops.size(); ++i)
                ss << ", " << ops[i];
            ss << " }";
            return ss.str();
        }

        std::string bin_apply(std::string f, auto &&ops,
                              bool signed_op=false)
        {
            auto get_ = [&](auto op) {
                auto out = get(op);
                if (signed_op)
                    return impl::to_signed(out);
                return out;
            };

            // Lifetime since `ops` can be generator.
            auto it = ops.begin();
            check(it != ops.end());

            std::stringstream ss;
            ss << get_(*it);
            ++it;
            for (; it != ops.end(); ++it)
                ss << " " << f << " " << get_(*it);
            return ss.str();
        }

        std::string ternary_stmt(Operation *cond, Operation *true_v, Operation *false_v)
        {
            std::stringstream ss;
            ss << get(cond) << " ? " << get(true_v) << " : " << get(false_v);
            return ss.str();
        }

        std::string mux(Select *op)
        {
            auto selector = get(op->selector());
            auto name = impl::wire_name(op);

            std::stringstream ss;
            // `next` is a recursion call - unfortunately in c++ lambda itself
            // cannot be invoked inside its body, so it needs to be passed as
            // extra argument.
            auto make_case = [&](std::size_t idx, auto next) -> void {
                ss << "( " << selector << " == "
                   << op->selector()->size << "'d" << idx - 1
                   << ") ? "
                   << get(op->operand(idx)) << " : ";
                if (idx == op->operands_size() - 2)
                    ss << get(op->operand(op->operands_size() - 1));
                else {
                    ss << std::endl << "\t";
                    next(idx + 1, next);
                }
            };

            make_case(1, make_case);
            return make_wire(op, ss.str());
        }

        auto zip()
        {
            return [=](auto &&... args) -> std::string {
                return this->bin_apply(std::forward<decltype(args)>(args)...);
            };
        }

        auto szip()
        {
            return [=](auto &&... args) -> std::string {
                return this->bin_apply(std::forward<decltype(args)>(args)..., true);
            };
        }

        // TODO(lukas): For dbg purposes.
        auto unary()
        {
            return [=](std::string str, const std::vector< Operation * > &ops) -> std::string {
                check(ops.size() == 1);
                std::stringstream ss;
                ss << str << " " << get(ops.front());
                return ss.str();
            };
        }

        auto wrap_zip(auto begin, auto end)
        {
            return [=](auto &&... args) -> std::string {
                std::stringstream ss;
                ss << begin;
                ss << zip()(std::forward< decltype(args) >(args)...);
                ss << end;
                return ss.str();
            };
        }

        std::string rmake(auto F, Operation *op)
        {
            std::vector< Operation * > reversed;
            for (auto x : op->operands())
                reversed.push_back(x);

            std::reverse(reversed.begin(), reversed.end());
            return make_wire(op, F(get_symbol(op), reversed));
        }

        std::string make(auto F, Operation *op)
        {
            return make_wire(op, F(get_symbol(op), op->operands()));
        }

        std::string make(auto F, Operation *op, std::string f)
        {
            return make_wire(op, F(f, op->operands()));
        }

        /* Operation mapping to symbols */
        std::string get_symbol(Operation *op)
        {
            switch (op->op_code) {
                case AdviceConstraint::kind:
                case RegConstraint::kind:
                case DecodeCondition::kind:
                    return "==";
                case Add::kind: return "+";
                case Sub::kind: return "-";
                case Mul::kind: return "*";
                // TODO(lukas): Verify this is correct.
                case SDiv::kind: return "/";
                case UDiv::kind: return "/";

                case URem::kind: return "%";
                case SRem::kind: return "%";

                case Shl::kind: return "<<";
                case LShr::kind: return ">>";
                case AShr::kind: return ">>>";

                case Icmp_eq::kind:  return "==";
                case Icmp_ne::kind:  return "!=";
                case Icmp_uge::kind: return ">=";
                case Icmp_sge::kind: return ">=";
                case Icmp_ugt::kind: return ">";
                case Icmp_sgt::kind: return ">";
                case Icmp_slt::kind: return "<";
                case Icmp_ult::kind: return "<";
                case Icmp_ule::kind: return "<=";
                case Icmp_sle::kind: return "<=";

                case Or::kind:
                    return "|";
                case VerifyInstruction::kind:
                case And::kind:
                case DecoderResult::kind:
                    return "&";
                case Xor::kind: return "^";
                case Concat::kind:
                    return ",";
                default:
                    log_kill() << "Unsupported op_code: " << pretty_print(op);
            }
        }

        std::string visit(Operation *op)
        {
            log_kill() << "Cannot print in verilog: " << pretty_print< true >(op)
                       << "\n" << ctx.dbg().str();
        }

        // Each extra operand V(n + 1) introduces
        // R(n) - result of computation - true if at least one `1` is present
        // O(n) - result of overflow flag - true if at least two `1` were present
        // check_overflow(O, R, V) : O || (R && V)
        //   overflow is saturared and it is raised if both current value and previous
        //   are set to `1` (i.e. there are at least two `1`s in inputs)
        // R(n + 1) := R(n) || V(n + 1)
        // O(n + 1) := check_overflow(O(n), R(n), V(n + 1))
        // Final value is then computed as
        // !O(m) && R(m)
        //   i.e. overflow did not happen, and at least one `1` was found.
        std::string visit(OnlyOneCondition *op)
        {
            auto base = [&](std::string s) { return s + "nx" + std::to_string(op->id()); };
            auto rn = [&](auto i) { return base("r") + "x" + std::to_string(i); };
            auto on = [&](auto i) { return base("o") + "x" + std::to_string(i); };

            // `( rn, on )`.
            using step_t = std::tuple< std::string, std::string >;
            std::vector< step_t > steps = { { impl::false_val(), impl::false_val() } };

            for (std::size_t i = 0; i < op->operands_size(); ++i)
            {
                const auto &[prev_rn, prev_on] = steps.back();
                auto rn_next = prev_rn + " || " + get(op->operand(i));
                auto on_next = prev_on + " || ( " + prev_rn + " && "
                                       + get(op->operand(i)) + ")";
                steps.emplace_back(make_wire(rn(i), rn_next, 1), make_wire(on(i), on_next, 1));
            }

            const auto &[last_rn, last_on] = steps.back();
            std::stringstream ss;
            ss << "(!" << last_on << ") && " << last_rn;
            return make_wire(op, ss.str());
        }

        std::string visit(VerifyInstruction *op) { return make(zip(), op); }

        /* Constraints */

        std::string visit(AdviceConstraint *op)    { return make(zip(), op); }
        std::string visit(RegConstraint *op)       { return make(zip(), op); }

        std::string make_extract(const std::string &from, uint64_t high_inc, uint64_t low_inc)
        {
            std::stringstream ss;
            ss << from << "[" << high_inc << ": " << low_inc << "]";
            return ss.str();
        }

        using parsed_mem_t = irops::memory::Parsed< std::string >;
        parsed_mem_t parse_mem(Operation *op)
        {
            auto extract = [&](auto from, auto current, auto size) {
                return make_extract(from, current + size - 1, current);
            };
            return irops::memory::parse(get(op), extract, ctx.circuit->ptr_size);
        }

        std::string make_memory_constraint(MemoryConstraint *op, bool is_write)
        {
            auto hint = parse_mem(op->hint_arg());

            std::stringstream ss;
            auto apply = [&](const std::string &lhs, const std::string &rhs,
                             std::string prefix="")
            {
                ss << prefix << " " << lhs << " == " << rhs;
            };

            auto add = [&](auto &&... args) { return apply(args ..., " &&"); };

            // TODO(lukas): It would be maybe better to instead concat the right side
            //              and compare with whole hint?
            std::string mode = (is_write) ? "1" : "0";

            apply(get(op->size_arg())    , hint.size());
            add(  get(op->addr_arg())    , hint.addr());
            add(  get(op->ts_arg())      , hint.timestamp());
            // In reads `val_arg()` is not present
            if (is_write)
                add(  get(op->val_arg()) , hint.value());
            add(  impl::to_verilog(4, op->mem_idx()) , hint.id());
            add(  "1'b1"                 , hint.used());
            add(  impl::bin_zero(6u)     , hint.reserved());
            add(  "1'b" + mode           , hint.mode());
            return make_wire(op, ss.str());
        }
        std::string visit(ReadConstraint *op) { return make_memory_constraint(op, false); }
        std::string visit(WriteConstraint *op) { return make_memory_constraint(op, true); }

        std::string visit(UnusedConstraint *op)
        {
            auto hint = get(op->operand(0));
            auto zero = impl::bin_zero(op->operand(0)->size);
            return make_wire(op, hint + " == " + zero);
        }

        /* Decode condition */
        std::string visit(DecodeCondition *op) { return make(zip(), op); }

        /* Mux */
        std::string visit(Select *op) { return mux(op); }

        /* BitManip */
        std::string visit(Concat *op)
        {
            return rmake(wrap_zip("{ ", " }"), op);
        }

        std::string visit(Extract *op)
        {
            auto from = get(op->operand(0));
            return make_wire(op, make_extract(from, op->high_bit_exc - 1, op->low_bit_inc));
        }

        /* Helpers */
        // TOOD(lukas): Double check.
        std::string visit(InputImmediate *op) { return get(op->operand(0)); }

        /* Nary helpers */
        std::string visit(And *op) { return make(zip(), op); }
        std::string visit(Or *op)  { return make(zip(), op); }

        /* Decoder result wrapper */
        std::string visit(DecoderResult *op) { return make(zip(), op); }

        /* LLVM ops */
        std::string visit(Add *op) { return make(zip(), op); }
        std::string visit(Sub *op) { return make(zip(), op); }
        std::string visit(Mul *op) { return make(zip(), op); }

        std::string visit(UDiv *op) { return make(zip(), op); }
        std::string visit(SDiv *op) { return make(szip(), op); }

        std::string visit(URem *op) { return make(zip(), op); }
        std::string visit(SRem *op) { return make(szip(), op); }

        std::string visit(Shl *op)  { return make(zip(), op); }
        std::string visit(LShr *op) { return make(zip(), op); }
        std::string visit(AShr *op) { return make(zip(), op); }

        std::string visit(Trunc *op)
        {
            auto trg_size = op->size - 1;
            std::stringstream ss;
            ss << get(op->operand(0)) << "[" << trg_size << ":0)";
            return make_wire(op, ss.str());
        }

        std::string visit(ZExt *op)
        {
            auto prefix = impl::bin_zero(op->size - op->operand(0)->size);
            return make_wire(op, concat({prefix, get(op->operand(0))}));
        }

        std::string visit(SExt *op)
        {
            auto pos_prefix = impl::bin_zero(op->size - op->operand(0)->size);
            auto neg_prefix = impl::bin_one(op->size - op->operand(0)->size);

            std::stringstream selector_ss;
            auto operand = op->operand(0);
            auto last = operand->size - 1;
            selector_ss << "(" << get(operand) << "[" << last << ":" << last << "] == "
                        << impl::bin_one(1u)
                        << ") ?" << neg_prefix << " : " << pos_prefix;
            auto padding =
                make_wire("pad_" + std::to_string(op->id()), selector_ss.str(), last + 1);
            return make_wire(op, concat({padding, get(op->operand(0))}));
        }

        std::string visit(Icmp_ult *op) { return make(zip(), op); }
        std::string visit(Icmp_slt *op) { return make(szip(), op); }
        std::string visit(Icmp_ugt *op) { return make(zip(), op); }
        std::string visit(Icmp_eq  *op) { return make(zip(), op); }
        std::string visit(Icmp_ne  *op) { return make(zip(), op); }
        std::string visit(Icmp_uge *op) { return make(zip(), op); }
        std::string visit(Icmp_ule *op) { return make(zip(), op); }
        std::string visit(Icmp_sgt *op) { return make(szip(), op); }
        std::string visit(Icmp_sge *op) { return make(szip(), op); }
        std::string visit(Icmp_sle *op) { return make(szip(), op); }

        std::string visit(Xor *op) { return make(zip(), op); }

        /* Leaves */
        std::string visit(Constant *op)
        {
            return make_wire(op, std::to_string(op->size) + "'b" + op->bits);
        }

        std::string visit(Undefined *op)
        {
            return make_wire(op, impl::bin_zero(op->size));
        }

        /* High level */
        std::string visit(PopulationCount *op)
        {
            uint32_t operand_size = op->operand(0)->size;
            uint32_t rsize = static_cast< uint32_t >(std::ceil(std::log2(operand_size)));
            uint32_t pad_size = operand_size - rsize;

            std::stringstream ss;
            auto from = get(op->operand(0));
            for (std::size_t i = 0; i < operand_size; ++i) {
                ss << from << "[" << i << "]";
                if (i != operand_size - 1)
                    ss << " + ";
            }
            auto name = impl::wire_name(op);
            auto aux = name + "_aux";

            ctx.os() << "wire " << impl::wire_size(rsize) << " " << aux << " = "
                     << ss.str() << ";\n";
            ctx.os() << "wire " << name << " = "
                     << concat({impl::bin_zero(pad_size), aux}) << ";\n";
            return name;
        }

        std::string visit(CountLeadingZeroes *op)
        {
            auto get_bit_ = [&](auto op, auto i) {
                return this->get_bit(op, op->size - i - 1);
            };
            return count_zeroes(op, get_bit_);
        }
        std::string visit(CountTrailingZeroes *op)
        {
            return count_zeroes(op, [&](auto op, auto i){ return this->get_bit(op, i); });
        }

        std::string count_zeroes(Operation *op, auto next_bit)
        {
            auto base = [&](std::string s) { return s + "nx" + std::to_string(op->id()); };
            auto fn = [&](auto i) { return base("f") + "x" + std::to_string(i); };
            auto tn = [&](auto i) { return base("t") + "x" + std::to_string(i); };

            uint32_t operand_size = op->operand(0)->size;
            auto operand = op->operand(0);
            uint32_t rsize = static_cast< uint32_t >(std::ceil(std::log2(operand_size)));
            auto padding = impl::bin_zero(operand_size - rsize);

            // `( fn, tn )`.
            using step_t = std::tuple< std::string, std::string >;
            std::vector< step_t > steps = { { impl::true_val(), impl::bin_zero(rsize) } };

            for (std::size_t i = 0; i < operand->size; ++i)
            {
                const auto &[prev_fn, prev_tn] = steps.back();
                auto fn_next = prev_fn + " && (!" + next_bit(operand, i) + ")";
                auto tn_next = prev_tn + " + { " + impl::bin_zero(rsize - 1)
                                       + ", " + fn_next + "}";
                steps.emplace_back(make_wire(fn(i), fn_next, 1), make_wire(tn(i),
                                   tn_next, rsize));
            }

            const auto &[_, last_tn] = steps.back();
            return make_wire(op, concat({padding, last_tn}));
        }

        std::string write(Operation *op) { return get(op); }
    };

    namespace iarg_fmt
    {
        struct UseName
        {
            std::string operator()(Operation *op) {
                // `.` is not a valid character in token name in verilog
                std::string out = "";
                for (auto c : op->name())
                    out += (c == '.') ? '_' : c;
                return out;
            }
        };

        struct Simple
        {
            uint32_t idx = 0;

            static inline std::vector< std::string > avail =
            {
                "A", "B", "C", "D", "E", "F", "G", "H"
            };

            std::string operator()(Operation *)
            {
                check(idx < avail.size());
                return avail[idx++];
            }
        };
    } // namespace iarg_fmt

    template< typename Next >
    struct WithNames : Next
    {
        using Next::Next;
        std::unordered_map< Operation *, std::string > args;
    };

    using ctx_t = WithNames< ToStream >;

    template< typename Ctx >
    struct ModuleHeaderBase
    {
        Ctx &ctx;

        // name -> whether something was stored to it.
        std::map< std::string, bool > result_args;

        ModuleHeaderBase(Ctx &ctx_) : ctx(ctx_) {}


        void declare_out_arg(const std::string &name, uint32_t size, bool is_last = false)
        {
            ctx.os() << "output " << impl::wire_size(size) << " " << name;
            if (!is_last)
                ctx.os() << ",";
            ctx.os() << std::endl;
            result_args.emplace(name, false);
        }

        void declare_in_arg(const std::string &name, uint32_t size)
        {
            // Appending `,` since this cannot be last one - output argument is expected
            // to be last
            ctx.os() << "input " << impl::wire_size(size) << " " << name << "," << std::endl;
        }

        void assign_out_arg(const std::string &name, const std::string &what)
        {
            auto it = result_args.find(name);
            // For now we cannot recover from this.
            check(it != result_args.end())
                << "Trying to use output wire that is not present:" << name;

            check(!it->second) << "Output wire" << name << "was already assigned!";
            it->second = true;
            ctx.os() << "assign " << name << " = " << what << ";\n";
        }

        void finalize(Operation *) {}
    };


    template< typename IArgFmt, typename Ctx >
    struct CoreModuleHeader : ModuleHeaderBase< Ctx >
    {
        using parent_t = ModuleHeaderBase< Ctx >;

        // Expects to be default constructed.
        IArgFmt iarg_fmt;

        using parent_t::parent_t;

        template< typename Fmt >
        void declare_in_arg(Operation *op, Fmt &&fmt)
        {
            auto get_name = [&](auto op) -> std::string {
                if (auto name = this->ctx.get_name(op))
                    return *name;
                // fmt may be stateful, so extra invocation is not desired.
                return this->ctx.give_name(op, fmt(op));
            };
            auto name = get_name(op);
            this->parent_t::declare_in_arg(name, op->size);
        }

        template< typename O, typename ... Ts, typename Fmt >
        void declare_in_args(Fmt &&fmt)
        {
            for (auto op : this->ctx.circuit->template Attr< O >())
                declare_in_arg(op, fmt);

            if constexpr (sizeof...(Ts) != 0)
                return declare_in_args< Ts ... >(std::forward< Fmt >(fmt));
            else
                return;
        }


        void declare_in_args(Operation *op)
        {
            if (isa< Circuit >(op))
            {
                declare_in_args< OutputRegister, InputRegister,
                                  InputErrorFlag, OutputErrorFlag,
                                  InputTimestamp, OutputTimestamp,
                                  Memory,
                                  InputInstructionBits,
                                  Advice >( iarg_fmt );
            } else {
                for (auto operand : op->operands())
                    declare_in_arg(operand, iarg_fmt );
            }
        }

        void declare_out_args() { this->declare_out_arg("result", 1, true); }
    };


    template< typename Impl >
    struct ModuleHeader : Impl
    {
        using Impl::Impl;

        void declare_module(const std::string &name, Operation *op)
        {
            this->ctx.os() << "module " << name << "(" << std::endl;
            this->declare_in_args(op);
            this->ctx.os() << std::endl;
            this->declare_out_args();
            this->ctx.os() << ");\n";

            this->finalize(op);
        }

        void end_module() { this->ctx.os() << "endmodule" << std::endl; }
    };

    template< typename Ctx >
    struct CheckerModuleHeader : ModuleHeaderBase< Ctx >
    {
        using parent_t = ModuleHeaderBase< Ctx >;
        using parent_t::parent_t;

        // TODO(lukas): This is optional only so that we can initiliaze it in later
        //              calls which is not a desirable design.
        std::optional< Trace > trace;

        void declare_in_args(Operation *op)
        {
            trace = Trace::make(this->ctx.circuit);
            this->declare_in_arg(current_trace_name(), trace->total_size);
            this->declare_in_arg(next_trace_name(), trace->total_size);
        }

        void declare_out_args()
        {
            this->declare_out_arg("result", 1);
            this->declare_out_arg("dummy", 1, true);
        }

        static std::string current_trace_name() { return "current"; }
        static std::string next_trace_name() { return "next"; }

        std::string get_trace_name(Operation *op)
        {
            if (is_one_of(op, output_leaves_ts{}))
                return next_trace_name();
            if (is_one_of(op, input_leaves_ts{}))
                return current_trace_name();
            unreachable() << "Trying to get trace name for unexpected op.";
        }

        void finalize(Operation *)
        {
            for (auto &[op, field] : trace->parse_map)
            {
                const auto &[ start, size, _ ] = *field;
                OpFmt< Ctx > builder(this->ctx);
                auto name = this->ctx.give_name(op, iarg_fmt::UseName()(op));
                auto body = builder.make_extract(get_trace_name(op), start + size - 1, start);
                builder.make_wire(name, body, size);
            }
        }
    };

    template< typename IArgFmt, typename Ctx >
    using core_module_header = ModuleHeader< CoreModuleHeader< IArgFmt, Ctx > >;

    template< typename Ctx >
    using checker_module_header = ModuleHeader< CheckerModuleHeader< Ctx > >;

    static inline std::string get_module_name( std::optional< Operation * > maybe_op = {} )
    {
        if ( !maybe_op )
            return "full_circuit";
        auto op = *maybe_op;
        check(!isa< leaf_values_ts >((op)));
        return op_code_str(op->op_code) + "_" + std::to_string(op->size);
    }

    static inline void print(std::ostream &os, const std::string &module_name, Circuit *c)
    {
        ctx_t ctx{ os, c };
        // TODO(lukas): Add some mechanism to choose - as there will most likely be many
        //              options, this may need to be more complex than a simple flag.
        //core_module_header< iarg_fmt::UseName, ctx_t > header(ctx);
        checker_module_header< ctx_t > header(ctx);

        header.declare_module(module_name, c->root);
        auto ret = OpFmt< ctx_t >(ctx).write(c->root);
        header.assign_out_arg("result", ret);
        header.assign_out_arg("dummy", impl::bin_zero(1u));
        header.end_module();
    }

} // namespace circ::print::verilog
