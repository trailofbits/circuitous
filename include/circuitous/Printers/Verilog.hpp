/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma pragma once

#include <circuitous/IR/Circuit.hpp>

#include <deque>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>

namespace circ::print {

  template< typename Self >
  struct VerilogFmt : Visitor< Self > {

    using parent_t = Visitor< Self >;
    using parent_t::Dispatch;

    auto &self() { return static_cast< Self & >(*this); }
    auto get(Operation *op) {
      if (!self().op_names.count(op))
        self().op_names[op] = this->Dispatch(op);
      return self().op_names[op];
    }

    auto wire_name(Operation *op) {
      std::stringstream ss;
      ss << std::hex << "v" << op->id();
      return ss.str();
    }

    auto wire_decl(const std::string &name, std::string lhs) {
      return "wire " + name + " = " + lhs + ";\n";
    }

    std::string make_wire(Operation *op, std::string lhs) {
      auto name = wire_name(op);
      self().os << wire_decl(name, std::move(lhs));
      return name;
    }

    std::string concat(const std::vector< std::string > &ops) {
      std::stringstream ss;
      ss << "{ " << ops[0];
      for (std::size_t i = 1; i < ops.size(); ++i)
        ss << ", " << ops[i];
      ss << " }";
      return ss.str();
    }

    std::string bin_apply(std::string f, const std::vector< Operation * > &ops)
    {
      CHECK(ops.size() != 0);

      std::stringstream ss;
      ss << get(ops[0]);
      for (std::size_t i = 1; i < ops.size(); ++i)
        ss << " " << f << " " << get(ops[i]);
      LOG(INFO) << ss.str();
      return ss.str();
    }

    std::string ternary_stmt(Operation *cond, Operation *true_v, Operation *false_v) {
      std::stringstream ss;
      ss << cond << " ? " << true_v << " " << false_v;
      return ss.str();
    }

    std::string ternary(BSelect *op) {
      auto lhs = ternary_stmt(op->operands[0], op->operands[1], op->operands[2]);
      return make_wire(op, std::move(lhs));
    }

    std::string mux(Select *op) {
      auto selector = get(op->selector());
      auto name = wire_name(op);

      std::stringstream ss;
      // `next` is a recursion call - unfortunately in c++ lambda itself
      // cannot be invoked inside its body, so it needs to be passed as
      // extra argument.
      auto make_case = [&](std::size_t idx, auto next) -> void {
        ss << "( " << selector << " == "
           << op->selector()->size << "'d" << idx - 1
           << ") ? "
           << get(op->operands[idx]) << " : ";
        if (idx == op->operands.size() - 2)
          ss << get(op->operands.back());
        else {
          ss << std::endl << "\t";
          next(idx + 1, next);
        }
      };

      make_case(1, make_case);

      return make_wire(op, ss.str());
    }

    auto zip() {
      return [=](auto &&... args) -> std::string {
        return this->bin_apply(std::forward<decltype(args)>(args)...);
      };
    }

    // TODO(lukas): For dbg purposes.
    auto unary() {
      return [=](std::string str, const std::vector< Operation * > &ops) -> std::string {
        CHECK(ops.size() == 1);
        std::stringstream ss;
        ss << str << " " << get(ops.front());
        return ss.str();
      };
    }

    auto wrap_zip(auto begin, auto end) {
      return [=](auto &&... args) -> std::string {
        std::stringstream ss;
        ss << begin;
        ss << zip()(std::forward< decltype(args) >(args)...);
        ss << end;
        return ss.str();
      };
    }

    auto make(auto F, Operation *op) {
      return make_wire(op, F(get_symbol(op), op->operands));
    }

    auto make(auto F, Operation *op, std::string f) {
      return make_wire(op, F(f, op->operands));
    }

    /* Operation mapping to symbols */
    std::string get_symbol(Operation *op) {
      switch (op->op_code) {
        case AdviceConstraint::kind:
        case RegConstraint::kind:
        case PreservedConstraint::kind:
        case CopyConstraint::kind:
        case DecodeCondition::kind:
          return "==";
        case Add::kind: return "+";
        case Sub::kind: return "-";
        case Mul::kind: return "*";
        // TODO(lukas): Verify this is correct.
        case SDiv::kind: return "/";
        case UDiv::kind: return "/";

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

        case COr::kind:
        case Or::kind:
          return "|";
        case CAnd::kind:
        case VerifyInstruction::kind:
        case And::kind:
          return "&";
        case CXor::kind: return "^";
        case OnlyOneCondition::kind:
          return "TODO";
        case Concat::kind:
          return ",";
        default:
          LOG(FATAL) << "Unsupported op_code: " << pretty_print(op);
      }
    }

    auto bin_zero(auto size) -> std::string {
      return std::to_string(size) + "'b" + std::string(size, '0');
    }

    std::string Visit(Operation *op) {
      LOG(FATAL) << "Cannot print in verilog: " << pretty_print(op);
    }

    std::string Visit(OnlyOneCondition *op)  { return make(zip(), op); }
    std::string Visit(VerifyInstruction *op) { return make(zip(), op); }

    /* Constraints */

    std::string Visit(AdviceConstraint *op)    { return make(zip(), op); }
    std::string Visit(RegConstraint *op)       { return make(zip(), op); }
    std::string Visit(PreservedConstraint *op) { return make(zip(), op); }
    std::string Visit(CopyConstraint *op)      { return make(zip(), op); }

    std::string Visit(ReadConstraint *)  { LOG(FATAL) << "memop"; }
    std::string Visit(WriteConstraint *) { LOG(FATAL) << "memop"; }

    /* Decode condition */
    std::string Visit(DecodeCondition *op) { return make(zip(), op); }

    /* Mux */
    std::string Visit(Select *op) { return mux(op); }

    /* BitManip */
    std::string Visit(Concat *op) {
      return make(wrap_zip("{ ", " }"), op);
    }

    std::string Visit(Extract *op) {
      auto from = get(op->operands[0]);
      std::stringstream ss;
      ss << from << "[" << op->high_bit_exc - 1 << ": " << op->low_bit_inc << "]";
      return make_wire(op, ss.str());
    }

    /* Helpers */
    // TOOD(lukas): Double check.
    std::string Visit(InputImmediate *op) { return get(op->operands[0]); }

    /* Nary helpers */
    std::string Visit(And *op) { return make(zip(), op); }
    std::string Visit(Or *op)  { return make(zip(), op); }

    /* LLVM ops */
    std::string Visit(Add *op) { return make(zip(), op); }
    std::string Visit(Sub *op) { return make(zip(), op); }
    std::string Visit(Mul *op) { return make(zip(), op); }

    std::string Visit(UDiv *op) { return make(zip(), op); }
    std::string Visit(SDiv *op) { return make(zip(), op); }

    std::string Visit(Shl *op)  { return make(zip(), op); }
    std::string Visit(LShr *op) { return make(zip(), op); }
    std::string Visit(AShr *op) { return make(zip(), op); }

    std::string Visit(Trunc *op) { return make(unary(), op, std::string("todo.trunc")); }
    std::string Visit(ZExt *op)  {
      auto prefix = bin_zero(op->size - op->operands[0]->size);
      return make_wire(op, concat({prefix, get(op)}));
    }

    std::string Visit(SExt *op)  { return make(unary(), op, std::string("todo.sext")); }

    std::string Visit(Icmp_ult *op) { return make(zip(), op); }
    std::string Visit(Icmp_slt *op) { return make(zip(), op); }
    std::string Visit(Icmp_ugt *op) { return make(zip(), op); }
    std::string Visit(Icmp_eq  *op) { return make(zip(), op); }
    std::string Visit(Icmp_ne  *op) { return make(zip(), op); }
    std::string Visit(Icmp_uge *op) { return make(zip(), op); }
    std::string Visit(Icmp_ule *op) { return make(zip(), op); }
    std::string Visit(Icmp_sgt *op) { return make(zip(), op); }
    std::string Visit(Icmp_sge *op) { return make(zip(), op); }
    std::string Visit(Icmp_sle *op) { return make(zip(), op); }

    std::string Visit(BSelect *op) { return ternary(op); }

    std::string Visit(CAnd *op) { return make(zip(), op); }
    std::string Visit(COr *op)  { return make(zip(), op); }
    std::string Visit(CXor *op) { return make(zip(), op); }

    /* Leaves */
    std::string Visit(Constant *op) {
      return make_wire(op, std::to_string(op->size) + "'b" + op->bits);
    }

    std::string Visit(Undefined *op) {
      return make_wire(op, "xxxx");
    }

    /* High level */
    std::string Visit(PopulationCount *op) {
      return make(unary(), op, std::string("todo.pc"));
    }

    std::string Visit(CountLeadingZeroes *op) {
      return make(unary(), op, std::string("todo.clz"));
    }

    std::string Visit(CountTrailingZeroes *op) {
      return make(unary(), op, std::string("todo.ctz"));
    }

    std::string Visit(Circuit *op) { return get(op->operands[0]); }
  };

  struct dbg_verbose {
    std::stringstream dbg;
  };

  struct Verilog : dbg_verbose, VerilogFmt< Verilog > {
    std::ostream &os;
    Circuit *circuit;

    std::unordered_map< Operation *, std::string > op_names;
    std::string result;

    Verilog(std::ostream &os_, Circuit *circuit_) : os(os_), circuit(circuit_) {}

    Verilog(const Verilog &) = delete;
    Verilog(Verilog &&) = delete;

    Verilog &operator=(const Verilog &) = delete;
    Verilog &operator=(Verilog &&) = delete;

    void declare_module(const std::string &name)
    {
      os << "module " << name << "(" << std::endl;
      write_inputs();
      os << std::endl;
      write_output();
      os << "\n);\n";
    }

    template< typename I > requires std::is_integral_v< I >
    std::string wire_size(I size) {
      std::stringstream ss;
      ss << "[" << size - 1 << ": 0]";
      return ss.str();
    }

    auto &give_name(Operation *op, std::string name) {
      dbg << "Naming: " << pretty_print< false >(op) << " -> " << name << std::endl;
      auto [it, flag] = op_names.emplace(op, std::move(name));
      CHECK(flag) << std::endl << dbg.str();
      return it->second;
    }

    template< typename O, typename ... Ts, typename Fmt >
    void write_input(Fmt &&fmt) {
      auto get_name = [&](auto op) -> std::string {
        // fmt may be stateful, so extra invocation is not desired.
        CHECK(!op_names.count(op)) << std::endl << dbg.str();
        return give_name(op, fmt(op));
      };

      for (auto op : circuit->Attr< O >()) {
        // Appending `,` since this cannot be last one - output argument is expected
        // to be last
        os << "input " << wire_size(op->size) << " " << get_name(op) << ","<< std::endl;
      }

      if constexpr (sizeof...(Ts) != 0)
        return write_input< Ts ... >(std::forward< Fmt >(fmt));
      else
        return;
    }

    void write_inputs() {
      auto fmt_io = [](auto op) { return op->Name(); };

      uint32_t idx = 0;
      auto fmt_advice = [&](auto op) { return op->Name() + "." + std::to_string(++idx); };

      write_input< Advice >(fmt_advice);
      write_input< OutputRegister, InputRegister,
                   InputErrorFlag, OutputErrorFlag,
                   InputTimestamp, OutputTimestamp,
                   Memory,
                   InputInstructionBits >( fmt_io );
    }

    void write_output() {
      os << "output [0:0] result" << std::endl;
      result = "result";
    }

    void write_body() { this->Visit(circuit); }

    static void print(std::ostream &os, Circuit *c) {
      Verilog vprinter(os, c);
      vprinter.declare_module("circuit");
      vprinter.write_body();
    }
  };


} // namespace circ::print
