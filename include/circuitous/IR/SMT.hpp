/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <functional>
#include <memory>
#include <fstream>
#include <iostream>
#include <numeric>
#include <sstream>
#include <string>
#include <unordered_map>

#include <circuitous/Util/Logging.hpp>
#include <circuitous/Support/Check.hpp>
#include <circuitous/Support/Log.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/InstrTypes.h>
#include <z3++.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Storage.hpp>
#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Intrinsics.hpp>
#include <circuitous/IR/Visitors.hpp>

namespace circ
{

  template< typename Derived >
  struct BaseSMTVisitor : Visitor< Derived >
  {
    using Base = Visitor< Derived >;
    using Base::Dispatch;

    BaseSMTVisitor(auto ptr_size_) : ptr_size(ptr_size_), consts(ctx) {}

    z3::expr uninterpreted(Operation *op, const std::string &name, z3::sort result_sort)
    {
      z3::expr_vector args(ctx);
      z3::sort_vector args_sorts(ctx);
      for (const auto &arg : op->operands) {
        args.push_back(Dispatch(arg));
        args_sorts.push_back(args.back().get_sort());
      }

      auto decl = z3::function(name.c_str(), args_sorts, result_sort);
      return record(op, decl(args));
    }

    z3::expr uninterpreted(Operation *op, const std::string &name)
    {
      return uninterpreted(op, name, ctx.bv_sort(op->size));
    }

    z3::expr constant(Operation *op, const std::string &name)
    {
      auto e = ctx.bv_const(name.c_str(), op->size);
      if (!seen.count(op)) {
        consts.push_back(e);
      }
      return record(op, e);
    }

    z3::expr constant(Operation *op)
    {
      return constant(op, op->Name());
    }

    z3::expr lhs(Operation *op) { return Dispatch(op->operands[0]); };
    z3::expr rhs(Operation *op) { return Dispatch(op->operands[1]); };

    z3::expr accumulate(const auto &operands, const auto &fn)
    {
      auto dispatched = [&, fn] (const auto &lhs, auto rhs) { return fn(lhs, Dispatch(rhs)); };
      auto init = Dispatch(operands[0]);
      return std::accumulate(std::next(operands.begin()), operands.end(), init, dispatched);
    }

    z3::expr Visit(Operation *op)
    {
      log_kill() << "Unhandled operation: " << op->Name();
    }

    z3::expr& record(Operation *op, z3::expr e)
    {
      auto [it, _] = seen.try_emplace(op, e);
      return it->second;
    }

    uint32_t ptr_size = 0;
    z3::context ctx;
    z3::expr_vector consts;
    std::map< Operation *, z3::expr > seen;
  };

  template< typename Derived >
  struct IRToSMTConstantsVisitor : BaseSMTVisitor< Derived >
  {
    using Base = BaseSMTVisitor< Derived >;
    using Base::Dispatch;
    using Base::Visit;
    using Base::constant;
    using Base::ctx;
    using Base::record;

    using Base::Base;

    z3::expr Visit(InputInstructionBits *op) { return constant(op, "InputBits"); }
    z3::expr Visit(InputRegister *op) { return constant(op); }
    z3::expr Visit(OutputRegister *op) { return constant(op); }

    z3::expr Visit(Advice *op) { return constant(op, op->Name()); }

    z3::expr Visit(PopulationCount *op)     { return constant(op, "Population"); }
    z3::expr Visit(CountLeadingZeroes *op)  { return constant(op, "LeadingZeros"); }
    z3::expr Visit(CountTrailingZeroes *op) { return constant(op, "TrailingZeros"); }

    z3::expr Visit(InputTimestamp *op)  { return constant(op); }
    z3::expr Visit(OutputTimestamp *op) { return constant(op); }
    z3::expr Visit(InputErrorFlag *op)  { return constant(op); }
    z3::expr Visit(OutputErrorFlag *op) { return constant(op); }

    z3::expr Visit(Memory *op)  { return constant(op, "Memory"); }

    z3::expr Visit(Undefined *op) { return constant(op); }

    z3::expr Visit(Constant *op)
    {
      auto bits = std::make_unique<bool[]>(op->size);
      std::size_t idx = 0;
      for (auto bit : op->bits)
        bits[idx++] = bit != '0';
      return record(op, ctx.bv_val(op->size, bits.get()));
    }

  };

  template< typename Derived >
  struct IRToSMTOpsVisitor : IRToSMTConstantsVisitor< Derived >
  {
    using Base = IRToSMTConstantsVisitor< Derived >;
    using Base::Dispatch;
    using Base::Visit;
    using Base::ctx;
    using Base::lhs;
    using Base::rhs;
    using Base::record;

    using Base::Base;

    z3::expr to_bv(z3::expr expr)
    {
      return z3::ite(expr, true_bv(), false_bv());
    }

    z3::expr true_bv() { return ctx.bv_val(1, 1); }
    z3::expr false_bv() { return ctx.bv_val(0, 1); }

    z3::expr Visit(Add *op) { return record(op, lhs(op) + rhs(op)); }
    z3::expr Visit(Sub *op) { return record(op, lhs(op) - rhs(op)); }
    z3::expr Visit(Mul *op) { return record(op, lhs(op) * rhs(op)); }

    z3::expr Visit(UDiv *op) { return record(op, z3::udiv(lhs(op), rhs(op))); }
    z3::expr Visit(SDiv *op) { return record(op, lhs(op) / rhs(op)); }

    z3::expr Visit(CAnd *op) { return record(op, lhs(op) & rhs(op)); }
    z3::expr Visit(COr *op)  { return record(op, lhs(op) | rhs(op)); }
    z3::expr Visit(CXor *op) { return record(op, lhs(op) ^ rhs(op)); }

    z3::expr Visit(And *op)
    {
      return record(op, this->accumulate(op->operands, std::bit_and()));
    }
    z3::expr Visit(Or *op)
    {
      return record(op, this->accumulate(op->operands, std::bit_or()));
    }

    z3::expr Visit(Shl *op)  { return record(op, z3::shl(lhs(op), rhs(op))); }
    z3::expr Visit(LShr *op) { return record(op, z3::lshr(lhs(op), rhs(op))); }
    z3::expr Visit(AShr *op) { return record(op, z3::ashr(lhs(op), rhs(op))); }

    z3::expr Visit(Trunc *op) { return record(op, lhs(op).extract(op->size - 1, 0)); }

    z3::expr Visit(ZExt *op)
    {
      auto diff = op->size - op->operands[0]->size;
      return record(op, z3::zext(lhs(op), diff));
    }

    z3::expr Visit(SExt *op)
    {
      auto diff = op->size - op->operands[0]->size;
      return record(op, z3::sext(lhs(op), diff));
    }

    z3::expr Visit(Icmp_ult *op) { return record(op, to_bv(z3::ult(lhs(op), rhs(op)))); }
    z3::expr Visit(Icmp_slt *op) { return record(op, to_bv(z3::slt(lhs(op), rhs(op)))); }
    z3::expr Visit(Icmp_ugt *op) { return record(op, to_bv(z3::ugt(lhs(op), rhs(op)))); }
    z3::expr Visit(Icmp_eq *op)  { return record(op, to_bv(lhs(op) == rhs(op))); }
    z3::expr Visit(Icmp_ne *op)  { return record(op, to_bv(lhs(op) != rhs(op))); }
    z3::expr Visit(Icmp_uge *op) { return record(op, to_bv(z3::uge(lhs(op), rhs(op)))); }
    z3::expr Visit(Icmp_ule *op) { return record(op, to_bv(z3::ule(lhs(op), rhs(op)))); }
    z3::expr Visit(Icmp_sgt *op) { return record(op, to_bv(lhs(op) > rhs(op))); }
    z3::expr Visit(Icmp_sge *op) { return record(op, to_bv(lhs(op) >= rhs(op))); }
    z3::expr Visit(Icmp_sle *op) { return record(op, to_bv(z3::sle(lhs(op), rhs(op)))); }

    z3::expr Visit(Extract *op)
    {
      auto val = Dispatch(op->operands[0]);
      auto hi = op->high_bit_exc - 1;
      auto lo = op->low_bit_inc;
      return record(op, val.extract(hi, lo));
    }

    z3::expr Visit(InputImmediate *op)
    {
      return record(op, Dispatch(op->operands[0]));
    }
  };

  struct IRToSMTVisitor : IRToSMTConstantsVisitor<IRToSMTVisitor>
  {
    using Base = IRToSMTConstantsVisitor<IRToSMTVisitor>;
    using Base::Dispatch;
    using Base::Visit;

    using Base::Base;

    z3::expr Visit(Add *op) { return uninterpreted(op, "add"); }
    z3::expr Visit(Sub *op) { return uninterpreted(op, "sub"); }
    z3::expr Visit(Mul *op) { return uninterpreted(op, "mul"); }

    z3::expr Visit(UDiv *op) { return uninterpreted(op, "udiv"); }
    z3::expr Visit(SDiv *op) { return uninterpreted(op, "sdiv"); }

    z3::expr Visit(CAnd *op) { return uninterpreted(op, "cand"); }
    z3::expr Visit(COr *op)  { return uninterpreted(op, "cor"); }
    z3::expr Visit(CXor *op) { return uninterpreted(op, "cxor"); }

    z3::expr Visit(And *op) { return uninterpreted(op, "and"); }
    z3::expr Visit(Or *op)  { return uninterpreted(op, "or"); }

    z3::expr Visit(Shl *op)  { return uninterpreted(op, "shl"); }
    z3::expr Visit(LShr *op) { return uninterpreted(op, "lshr"); }
    z3::expr Visit(AShr *op) { return uninterpreted(op, "ashr"); }

    // z3::expr Visit(Trunc *op) { return uninterpreted(op, "trunc"); }
    z3::expr Visit(ZExt *op)  { return uninterpreted(op, "zext"); }
    z3::expr Visit(SExt *op)  { return uninterpreted(op, "sext"); }

    z3::expr Visit(Icmp_ult *op) { return uninterpreted(op, "ult"); }
    z3::expr Visit(Icmp_slt *op) { return uninterpreted(op, "slt"); }
    z3::expr Visit(Icmp_ugt *op) { return uninterpreted(op, "ugt"); }
    z3::expr Visit(Icmp_eq *op)  { return uninterpreted(op, "eq"); }
    z3::expr Visit(Icmp_ne *op)  { return uninterpreted(op, "ne"); }
    z3::expr Visit(Icmp_uge *op) { return uninterpreted(op, "uge"); }
    z3::expr Visit(Icmp_ule *op) { return uninterpreted(op, "ule"); }
    z3::expr Visit(Icmp_sgt *op) { return uninterpreted(op, "sgt"); }
    z3::expr Visit(Icmp_sge *op) { return uninterpreted(op, "sge"); }
    z3::expr Visit(Icmp_sle *op) { return uninterpreted(op, "sle"); }

    z3::expr Visit(Not *op) { return uninterpreted(op, "not"); }

    z3::expr Visit(Extract *op)
    {
      return uninterpreted(op, "Extract." + std::to_string(op->low_bit_inc) + "." + std::to_string(op->high_bit_exc));
    }

    z3::expr Visit(Concat *op) { return uninterpreted(op, "Concat"); }
    z3::expr Visit(Select *op) { return uninterpreted(op, "Select." + std::to_string(op->bits)); }
    z3::expr Visit(Parity *op) { return uninterpreted(op, "Parity"); }

    z3::expr Visit(BSelect *op) { return uninterpreted(op, "BSelect"); }

    z3::expr Visit(RegConstraint *op)       { return uninterpreted(op, "RegisterConstraint"); }
    z3::expr Visit(PreservedConstraint *op) { return uninterpreted(op, "PreservedConstraint"); }
    z3::expr Visit(CopyConstraint *op)      { return uninterpreted(op, "CopyConstraint"); }
    z3::expr Visit(AdviceConstraint *op)    { return uninterpreted(op, "AdviceConstraint"); }
    z3::expr Visit(OnlyOneCondition *op)    { return uninterpreted(op, "OnlyOne"); }
    z3::expr Visit(DecodeCondition *op)     { return uninterpreted(op, "Decode"); }
    z3::expr Visit(VerifyInstruction *op)   { return uninterpreted(op, "Verify"); }

    z3::expr Visit(ReadConstraint *op)      { return uninterpreted(op, "ReadConstraint"); }
    z3::expr Visit(WriteConstraint *op)     { return uninterpreted(op, "WriteConstraint"); }
    z3::expr Visit(UnusedConstraint *op)    { return uninterpreted(op, "UnusedConstraint"); }

    z3::expr Visit(InputImmediate *op) { return uninterpreted(op, "InputImmediate"); }

    z3::expr Visit(Circuit *op)
    {
      return uninterpreted(op, "Circuit", ctx.bool_sort());
    }
  };

  struct IRToBitBlastableSMTVisitor : IRToSMTOpsVisitor< IRToBitBlastableSMTVisitor >
  {
    using Base = IRToSMTOpsVisitor<  IRToBitBlastableSMTVisitor >;
    using Base::Dispatch;
    using Base::Visit;

    using Base::Base;

    z3::expr Visit(Concat *op)
    {
      z3::expr_vector vec(ctx);
      for (auto o : op->operands) {
        vec.push_back(Dispatch(o));
      }
      return record(op, z3::concat(vec));
    }

    z3::expr Visit(Select *op)
    {
      auto index = Dispatch(op->operands[0]);
      auto value = Dispatch(op->operands[1]);

      auto bw = index.get_sort().bv_size();
      auto current = ctx.bv_val(0, bw);

      auto undef = ctx.bv_val(0, value.get_sort().bv_size());

      z3::expr result = z3::ite(current == index, value, undef);
      for (auto i = 2U; i < op->operands.size(); ++i) {
        value = Dispatch(op->operands[i]);
        current = ctx.bv_val(i - 1, bw);
        result = z3::ite(current == index, value, result);
      }

      return record(op, result);
    }

    z3::expr Visit(BSelect *op)
    {
      auto cond = Dispatch(op->operands[0]);
      auto first = Dispatch(op->operands[1]);
      auto second = Dispatch(op->operands[2]);
      return record(op, z3::ite(cond == true_bv(), first, second));
    }

    z3::expr Visit(Parity *op)
    {
      auto operand = Dispatch(op->operands[0]);
      auto operand_size = operand.get_sort().bv_size();
      auto sum = operand.extract(0, 0);
      for (auto i = 1U; i < operand_size; ++i) {
        sum = sum ^ operand.extract(i, i);
      }
      return record(op, sum);
    }

    z3::expr Visit(RegConstraint *op)       { return record(op, to_bv(lhs(op) == rhs(op))); }
    z3::expr Visit(PreservedConstraint *op) { return record(op, to_bv(lhs(op) == rhs(op))); }
    z3::expr Visit(CopyConstraint *op)      { return record(op, to_bv(lhs(op) == rhs(op))); }
    z3::expr Visit(AdviceConstraint *op)    { return record(op, to_bv(lhs(op) == rhs(op))); }
    z3::expr Visit(DecodeCondition *op)     { return record(op, to_bv(lhs(op) == rhs(op))); }

    auto deconstruct_memory(const z3::expr &memory)
    {
      auto extractor = [&](auto thing, auto from, auto size) -> z3::expr {
        return thing.extract(from + size - 1, from);
      };
      check(memory.get_sort().bv_size() == irops::memory::size(ptr_size));
      return irops::memory::parse< z3::expr >(memory, extractor, ptr_size);
    }

    z3::expr Visit(ReadConstraint *op) {
      auto parsed = deconstruct_memory(Dispatch(op->hint_arg()));
      auto used =  parsed.used() == true_bv();
      auto mode =  parsed.mode() == false_bv();
      auto id =    parsed.id() == ctx.bv_val(static_cast< unsigned int >(op->id()), 4);
      auto size =  parsed.size() == Dispatch(op->size_arg());
      auto addr =  parsed.addr() == Dispatch(op->addr_arg());
      auto value = parsed.value() == Dispatch(op->val_arg());
      auto ts =    parsed.timestamp() == Dispatch(op->ts_arg());
      return record(op, to_bv(used && mode && id && size && addr && value && ts));
    }
    z3::expr Visit(WriteConstraint *op) {
      auto parsed = deconstruct_memory(Dispatch(op->hint_arg()));
      auto used = parsed.used() == true_bv();
      auto mode = parsed.mode() == true_bv();
      auto id =   parsed.id() == ctx.bv_val(static_cast< unsigned int >(op->id()), 4);
      auto size = parsed.size() == Dispatch(op->size_arg());
      auto addr = parsed.addr() == Dispatch(op->addr_arg());
      auto ts =   parsed.timestamp() == Dispatch(op->ts_arg());
      return record(op, to_bv(used && mode && id && size & addr && ts));
    }
    z3::expr Visit(UnusedConstraint *op) { not_implemented(); }

    z3::expr Visit(OnlyOneCondition *op)
    {
      auto size = static_cast< unsigned int >((op->operands.size() / 2) + 1);
      auto total = ctx.bv_val(0, size);
      for (auto x : op->operands)
        total = total + z3::zext(Dispatch(x), size - x->size);
      return record(op, to_bv(total == ctx.bv_val(1, size)));
    }

    z3::expr Visit(VerifyInstruction *op)
    {
      return record(op, accumulate(op->operands, std::bit_and()));
    }

    z3::expr Visit(Circuit *op)
    {
      auto expr = Dispatch(op->operands[0]);

      z3::sort_vector args_sorts(ctx);
      for (const auto &con : consts) {
        args_sorts.push_back(con.get_sort());
      }

      auto sem = z3::function("sem", args_sorts, ctx.bool_sort());
      return implies(sem(consts), expr == true_bv());
    }
  };

  template< typename derived >
  struct optimization
  {
    using expr_cache = std::unordered_map< unsigned, z3::expr >;

    derived& next() { return static_cast< derived& >(*this); }
    derived const& next() const { return static_cast< const derived & >(*this); }

    optimization(z3::context &ctx) : ctx(ctx), evec(ctx) {}

    z3::expr run(const z3::expr &e)
    {
      cache.clear();
      return optimize(e);
    }

    z3::expr optimize(const z3::expr &e)
    {
      if (cached(e))
        return cache.at(e.id());
      return next().optimize(e);
    }

    bool cached(const z3::expr &e) const { return cache.count(e.id()); }

    z3::expr store(const z3::expr &from, z3::expr to)
    {
      cache.emplace( from, to );
      return to;
    }

    z3::context &ctx;
    z3::expr_vector evec;
    expr_cache cache;
  };

  template< typename derived >
  struct app_optimization : optimization< app_optimization< derived > >
  {
    using base = optimization< app_optimization< derived > >;
    using base::base;

    derived& next() { return static_cast< derived& >(*this); }
    derived const& next() const { return static_cast< const derived & >(*this); }

    z3::expr optimize(const z3::expr &e)
    {
      return e.is_app() ? next().optimize(e) : e;
    }
  };

  struct or_to_and : app_optimization< or_to_and >
  {
    using base = app_optimization< or_to_and >::base;
    using app_optimization::app_optimization;

    z3::expr demorgan(const z3::expr_vector &args)
    {
      auto second = ++(args.begin());
      return !std::accumulate(second, args.end(), args[0], std::logical_and<>());
    }

    z3::expr optimize(const z3::expr &e)
    {
      z3::expr_vector args(ctx);
      for (auto i = 0U; i < e.num_args(); ++i) {
        auto arg = base::optimize(e.arg(i));
        args.push_back(e.is_or() ? !(arg) : arg);
      }

      return store( e, e.is_or() ? demorgan(args) : e.decl()(args) );
    }
  };

  struct eq_to_xnor : app_optimization< eq_to_xnor >
  {
    using base = app_optimization< eq_to_xnor >::base;
    using app_optimization::app_optimization;

    z3::expr optimize(const z3::expr &e)
    {
      z3::expr_vector args(ctx);
      for (auto i = 0U; i < e.num_args(); ++i)
        args.push_back(base::optimize(e.arg(i)));

      return store( e, e.is_eq() ? !(args[0] ^ args[1]) : e.decl()(args));
    }
  };

  struct elim_not : app_optimization< elim_not >
  {
    using base = app_optimization< elim_not >::base;
    using app_optimization::app_optimization;

    z3::expr optimize(const z3::expr &e)
    {
      z3::expr_vector args(ctx);
      for (auto i = 0U; i < e.num_args(); ++i)
        args.push_back(base::optimize(e.arg(i)));

      return store( e, e.is_not() && args[0].is_not() ? args[0].arg(0) : e.decl()(args) );
    }
  };

  static z3::solver bitblast(z3::expr expr, z3::context &ctx)
  {
    z3::solver solver(ctx);

    z3::tactic tactic(
      z3::tactic(ctx, "ctx-simplify") &
      z3::tactic(ctx, "propagate-bv-bounds") &
      z3::tactic(ctx, "solve-eqs") &
      z3::tactic(ctx, "simplify") &
      z3::tactic(ctx, "bit-blast") &
      z3::tactic(ctx, "aig")
    );

    z3::goal goal(ctx);
    goal.add(expr);
    expr = tactic(goal)[0].as_expr();

    // FIXME: optimize eq_to_xnor pass,
    // at the moment it timeouts on simple cases
    // solver.add(eq_to_xnor(ctx).run(expr));

    solver.add(expr);

    return solver;
  }

  struct CircuitStats
  {
    unsigned and_gates = 0;
    unsigned or_gates = 0;
    unsigned xor_gates = 0;
    unsigned not_gates = 0;

    CircuitStats& operator +=(const CircuitStats &other)
    {
      and_gates += other.and_gates;
      or_gates += other.or_gates;
      xor_gates += other.xor_gates;
      not_gates += other.not_gates;
      return *this;
    }
  };

  template< typename stream >
  auto operator<<(stream &os, const CircuitStats &stats) -> decltype(os << "")
  {
    return os << "bit-blasting statistics:\n"
        << "and gates: " << std::to_string(stats.and_gates) << '\n'
        << "or gates: " << std::to_string(stats.or_gates) << '\n'
        << "xor gates: " << std::to_string(stats.xor_gates) << '\n'
        << "not gates: " << std::to_string(stats.not_gates) << '\n';
  }

  static inline CircuitStats get_stats(z3::expr e, auto optimizer, auto &cache)
  {
    CircuitStats stats;

    if (cache.count(e.id()))
      return stats;
    cache.insert(e.id());

    // FIXME: optimize optimizer passes,
    // at the moment it timeout on simple cases
    // e = optimizer(e);

    if (e.is_app()) {

      if (e.is_and())
        stats.and_gates += e.num_args() - 1;
      else if (e.is_or())
        stats.or_gates += e.num_args() - 1;
      else if (e.is_xor())
        stats.xor_gates += e.num_args() - 1;
      else if (e.is_not())
        stats.not_gates++;

      auto args = e.num_args();
      for (unsigned i = 0; i < args; ++i)
        stats += get_stats(e.arg(i), optimizer, cache);

    } else {
      log_kill() << "unknown operation " << e << '\n';
    }

    return stats;
  }

  static inline CircuitStats get_stats(Circuit *circuit)
  {
      auto visitor = IRToBitBlastableSMTVisitor(circuit->ptr_size);
      auto expr = visitor.Visit(circuit);
      auto &ctx = visitor.ctx;

      CircuitStats stats;
      auto assertions = bitblast(expr, ctx).assertions();

      auto elimnot = [&ctx] (const z3::expr &e) { return elim_not(ctx).run(e); };
      auto ortoand = [&ctx] (const z3::expr &e) { return or_to_and(ctx).run(e); };

      auto optimizer = [&] (const z3::expr &e)
      {
        return elimnot( ortoand(e) );
      };

      std::unordered_set< unsigned > cache;

      for (const auto &e : assertions)
        stats += get_stats(e, optimizer, cache);

      return stats;
  }

  namespace smt
  {
    std::unique_ptr<Circuit> deserialize(const std::string &path);
  } // namespace smt

} // namespace circ
