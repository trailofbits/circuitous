/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Memory.hpp>

#include <cmath>
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
    std::string get(Operation *op) {
      if (!self().op_names.count(op))
        self().op_names[op] = this->Dispatch(op);
      return self().op_names[op];
    }

    template< typename I > requires std::is_integral_v< I >
    std::string wire_size(I size) {
      std::stringstream ss;
      ss << "[" << size - 1 << ": 0]";
      return ss.str();
    }

    template< typename I > requires std::is_integral_v< I >
    std::string get_bit(const std::string &op, I idx) {
      std::stringstream ss;
      ss << op << "[" << idx << ":" << idx << "]";
      return ss.str();
    }

    template< typename I > requires std::is_integral_v< I >
    std::string get_bit(Operation * op, I idx) {
      return get_bit(get(op), idx);
    }

    std::string wire_name(Operation *op) {
      std::stringstream ss;
      ss << std::hex << "v" << op->id();
      return ss.str();
    }

    std::string wire_decl(const std::string &name, std::string lhs) {
      return "wire " + name + " = " + lhs + ";\n";
    }

    std::string make_wire(const std::string name, std::string lhs) {
      self().os << wire_decl(name, std::move(lhs));
      return name;
    }

    std::string make_wire(Operation *op, std::string lhs) {
      return make_wire(wire_name(op), std::move(lhs));
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
      return ss.str();
    }

    std::string ternary_stmt(Operation *cond, Operation *true_v, Operation *false_v) {
      std::stringstream ss;
      ss << get(cond) << " ? " << get(true_v) << " : " << get(false_v);
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

    std::string make(auto F, Operation *op) {
      return make_wire(op, F(get_symbol(op), op->operands));
    }

    std::string make(auto F, Operation *op, std::string f) {
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
        case Concat::kind:
          return ",";
        default:
          LOG(FATAL) << "Unsupported op_code: " << pretty_print(op);
      }
    }

    std::string cast(auto size, auto value) {
      std::stringstream ss;
      ss << size << "'d" << value;
      return ss.str();
    }

    std::string bin_zero(auto size) {
      return std::to_string(size) + "'b" + std::string(size, '0');
    }

    std::string bin_one(auto size) {
      return std::to_string(size) + "'b" + std::string(size, '1');
    }

    std::string true_val() { return "1'b1"; }
    std::string false_val() { return "1'b0"; }

    std::string Visit(Operation *op) {
      LOG(FATAL) << "Cannot print in verilog: " << pretty_print< true >(op)
                 << "\n" << self().dbg.str();
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
    std::string Visit(OnlyOneCondition *op)  {
      // TOOD(lukas): At this point probably a submodule is a better fit.
      //              Unique identifier used to produced unique names.
      ++only_one_cond;

      auto base = [&](std::string s) { return s + "nx" + std::to_string(only_one_cond); };
      auto rn = [&](auto i) { return base("r") + "x" + std::to_string(i); };
      auto on = [&](auto i) { return base("o") + "x" + std::to_string(i); };

      // `( rn, on )`.
      using step_t = std::tuple< std::string, std::string >;
      std::vector< step_t > steps = { { false_val(), false_val() } };

      for (std::size_t i = 0; i < op->operands.size(); ++i) {
        const auto &[prev_rn, prev_on] = steps.back();
        auto rn_next = prev_rn + " || " + get(op->operands[i]);
        auto on_next = prev_on + " || ( " + prev_rn + " && " + get(op->operands[i]) + ")";
        steps.emplace_back(make_wire(rn(i), rn_next), make_wire(on(i), on_next));
      }

      const auto &[last_rn, last_on] = steps.back();
      std::stringstream ss;
      ss << "(!" << last_on << ") && " << last_rn;
      return make_wire(op, ss.str());
    }
    std::string Visit(VerifyInstruction *op) { return make(zip(), op); }

    /* Constraints */

    std::string Visit(AdviceConstraint *op)    { return make(zip(), op); }
    std::string Visit(RegConstraint *op)       { return make(zip(), op); }
    std::string Visit(PreservedConstraint *op) { return make(zip(), op); }
    std::string Visit(CopyConstraint *op)      { return make(zip(), op); }

    std::string make_extract(const std::string &from, uint64_t high_inc, uint64_t low_inc)
    {
      std::stringstream ss;
      ss << from << "[" << high_inc << ": " << low_inc << "]";
      return ss.str();
    }

    using parsed_mem_t = irops::memory::Parsed< std::string >;
    parsed_mem_t parse_mem(Operation *op) {
      auto extract = [&](auto from, auto current, auto size) {
        return make_extract(from, current + size - 1, current);
      };
      return irops::memory::parse(get(op), extract, self().circuit->ptr_size);
    }

    std::string make_memory_constraint(MemoryConstraint *op, bool is_write)
    {
      auto hint = parse_mem(op->hint_arg());

      std::stringstream ss;
      auto apply = [&](const std::string &lhs, const std::string &rhs, std::string prefix="") {
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
        add(  get(op->val_arg())     , hint.value());
      add(  cast(4, op->mem_idx()) , hint.id());
      add(  "1'b1"                 , hint.used());
      add(  bin_zero(6u)           , hint.reserved());
      add(  "1'b" + mode           , hint.mode());
      return make_wire(op, ss.str());
    }
    std::string Visit(ReadConstraint *op) { return make_memory_constraint(op, false); }
    std::string Visit(WriteConstraint *op) { return make_memory_constraint(op, true); }

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
      return make_wire(op, make_extract(from, op->high_bit_exc - 1, op->low_bit_inc));
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

    std::string Visit(Trunc *op) {
      auto trg_size = op->size - 1;
      std::stringstream ss;
      ss << get(op->operands[0]) << "[" << trg_size << ":0]";
      return make_wire(op, ss.str());
    }
    std::string Visit(ZExt *op)  {
      auto prefix = bin_zero(op->size - op->operands[0]->size);
      return make_wire(op, concat({prefix, get(op->operands[0])}));
    }

    std::string Visit(SExt *op) {
      auto pos_prefix = bin_zero(op->size - op->operands[0]->size);
      auto neg_prefix = bin_one(op->size - op->operands[0]->size);

      std::stringstream selector_ss;
      auto operand = op->operands[0];
      auto last = operand->size - 1;
      selector_ss << "(" << get(operand) << "[" << last << ":" << last << "] == "
                  << bin_one(1u)
                  << ") ?" << neg_prefix << " : " << pos_prefix;
      auto padding = make_wire("pad." + std::to_string(op->id()), selector_ss.str());
      return make_wire(op, concat({padding, get(op->operands[0])}));
    }

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
      uint32_t operand_size = op->operands[0]->size;
      uint32_t rsize = static_cast< uint32_t >(std::ceil(std::log2(operand_size)));
      uint32_t pad_size = operand_size - rsize;

      std::stringstream ss;
      auto from = get(op->operands[0]);
      for (std::size_t i = 0; i < operand_size; ++i) {
        ss << from << "[" << i << "]";
        if (i != operand_size - 1)
          ss << " + ";
      }
      auto name = wire_name(op);
      auto aux = name + ".aux";

      self().os << "wire " << aux << " " << wire_size(rsize) << " = " << ss.str() << ";\n";
      self().os << "wire " << name << " = " << concat({bin_zero(pad_size), aux}) << ";\n";
      return name;
    }

    std::string Visit(CountLeadingZeroes *op) {
      auto get_bit_ = [&](auto op, auto i) {
        return this->get_bit(op, op->size - i);
      };
      return count_zeroes(op, get_bit_);
    }
    std::string Visit(CountTrailingZeroes *op) {
      return count_zeroes(op, [&](auto op, auto i){ return this->get_bit(op, i); });
    }

    std::string count_zeroes(Operation *op, auto next_bit) {
      auto base = [&](std::string s) { return s + "nx" + std::to_string(op->id()); };
      auto fn = [&](auto i) { return base("f") + "x" + std::to_string(i); };
      auto tn = [&](auto i) { return base("t") + "x" + std::to_string(i); };

      uint32_t operand_size = op->operands[0]->size;
      auto operand = op->operands[0];
      uint32_t rsize = static_cast< uint32_t >(std::ceil(std::log2(operand_size)));
      auto padding = bin_zero(operand_size - rsize);

      // `( fn, tn )`.
      using step_t = std::tuple< std::string, std::string >;
      std::vector< step_t > steps = { { false_val(), bin_zero(rsize) } };

      for (std::size_t i = 0; i < operand->size; ++i) {
        const auto &[prev_fn, prev_tn] = steps.back();
        auto fn_next = prev_fn + " || (!" + next_bit(operand, i) + ")";
        auto tn_next = prev_tn + " + { " + bin_zero(rsize - 1) + ", " + fn_next + "}";
        steps.emplace_back(make_wire(fn(i), fn_next), make_wire(tn(i), tn_next));
      }

      const auto &[_, last_tn] = steps.back();
      return make_wire(op, concat({padding, last_tn}));
    }

    std::string Visit(Circuit *op) { return get(op->operands[0]); }

    uint32_t only_one_cond = 0u;
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

    std::string &give_name(Operation *op, std::string name) {
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

      write_input< OutputRegister, InputRegister,
                   InputErrorFlag, OutputErrorFlag,
                   InputTimestamp, OutputTimestamp,
                   Memory,
                   InputInstructionBits,
                   Advice >( fmt_io );
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
