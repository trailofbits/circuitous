/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/Printers.h>

#include <algorithm>
#include <memory>
#include <string>

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/SMT.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/StringRef.h>
#include <z3++.h>
#include <z3_api.h>
CIRCUITOUS_UNRELAX_WARNINGS


namespace circ::smt
{
  // Keeps track of already assigned ids.
  struct IdxCache {
    uint32_t mem_idx = 0;
    uint32_t advice_idx = 0;
  };

  Operation* app(const z3::expr &e, Circuit *circuit, IdxCache &idxs)
  {
    auto decl = e.decl();
    auto name = decl.name().str();
    auto sort = e.get_sort();

    if (name == "Circuit")
      return circuit;

    check(sort.is_bv());
    auto bv = sort.bv_size();

    if (name.starts_with("Extract")) {
      auto view = llvm::StringRef(name);
      auto [pref, rest] = view.split('.');
      auto [lo, hi] = rest.split('.');
      return circuit->Create< Extract >(unsigned(std::stoi(lo.str())), unsigned(std::stoi(hi.str())));
    }
    else if (name == "InputBits")
      return circuit->Create< InputInstructionBits >(bv);

    else if (name.starts_with("In.register")) {
      auto reg = name.substr(strlen("In.register."));
      return circuit->Create< InputRegister >(reg, bv);
    }

    else if (name.starts_with("Out.register")) {
      auto reg = name.substr(strlen("Out.register."));
      return circuit->Create< OutputRegister >(reg, bv);
    }

    else if (name == "Advice") {
      return circuit->Create< Advice >(bv, ++idxs.advice_idx);
    }

    else if (name == "Population")
      return circuit->Create< PopulationCount >(bv);
    else if (name == "LeadingZeros")
      return circuit->Create< CountLeadingZeroes >(bv);
    else if (name == "TrailingZeros")
      return circuit->Create< CountTrailingZeroes >(bv);

    else if (name == "RegisterConstraint")
      return circuit->Create< RegConstraint >();
    else if (name == "PreservedConstraint")
      return circuit->Create< PreservedConstraint >();
    else if (name == "CopyConstraint")
      return circuit->Create< CopyConstraint >();
    else if (name == "AdviceConstraint")
      return circuit->Create< AdviceConstraint >();

    else if (name == "OnlyOne")
      return circuit->Create< OnlyOneCondition >();
    else if (name == "Decode")
      return circuit->Create< DecodeCondition >();
    else if (name == "Verify")
      return circuit->Create< VerifyInstruction >();

    else if (name == "add")
      return circuit->Create< Add >(bv);
    else if (name == "sub")
      return circuit->Create< Sub >(bv);
    else if (name == "mul")
      return circuit->Create< Mul >(bv);

    else if (name == "udiv")
      return circuit->Create< UDiv >(bv);
    else if (name == "sdiv")
      return circuit->Create< SDiv >(bv);

    else if (name == "shl")
      return circuit->Create< Shl >(bv);
    else if (name == "lshr")
      return circuit->Create< LShr >(bv);
    else if (name == "ashr")
      return circuit->Create< AShr >(bv);

    // else if (name == "trunc")
    //   return circuit->Create< Trunc >();
    else if (name == "zext")
      return circuit->Create< ZExt >(bv);
    else if (name == "sext")
      return circuit->Create< SExt >(bv);

    else if (name == "ult")
      return circuit->Create< Icmp_ult >(bv);
    else if (name == "slt")
      return circuit->Create< Icmp_slt >(bv);
    else if (name == "ugt")
      return circuit->Create< Icmp_ugt >(bv);
    else if (name == "eq")
      return circuit->Create< Icmp_eq >(bv);
    else if (name == "ne")
      return circuit->Create< Icmp_ne >(bv);
    else if (name == "uge")
      return circuit->Create< Icmp_uge >(bv);
    else if (name == "ule")
      return circuit->Create< Icmp_ule >(bv);
    else if (name == "sgt")
      return circuit->Create< Icmp_sgt >(bv);
    else if (name == "sge")
      return circuit->Create< Icmp_sge >(bv);
    else if (name == "sle")
      return circuit->Create< Icmp_sle >(bv);

    else if (name == "In.timestamp")
      return circuit->Create< InputTimestamp >(bv);
    else if (name == "Out.timestamp")
      return circuit->Create< OutputTimestamp >(bv);

    else if (name == "In.error_flag")
      return circuit->Create< InputErrorFlag >(bv);
    else if (name == "Out.error_flag")
      return circuit->Create< OutputErrorFlag >(bv);

    else if (name == "Concat")
      return circuit->Create< Concat >(bv);
    else if (name.starts_with("Select")) {
      auto bits = name.substr(strlen("Select."));
      return circuit->Create< Select >(unsigned(std::stoi(bits)), bv);
    }
    else if (name == "Parity")
      return circuit->Create< Parity >();

    else if (name == "ReadConstraint")
      return circuit->Create< ReadConstraint >();
    else if (name == "WriteConstraint")
      return circuit->Create< WriteConstraint >();
    else if (name == "UnusedConstraint")
      return circuit->Create< UnusedConstraint >();

    else if (name == "Memory")
      return circuit->Create< Memory >(irops::memory::size(circuit->ptr_size), ++idxs.mem_idx);

    else if (name == "InputImmediate")
      return circuit->Create< InputImmediate >(bv);

    log_kill() << "unknown operation" << e << name;
  }

  Operation* constant(const z3::expr &e, Circuit *circuit)
  {
    auto sort = e.get_sort();
    auto bv = sort.bv_size();

    auto str = Z3_get_numeral_string(e.ctx(), e);
    llvm::APInt num(bv, str, 10);

    llvm::SmallString< 128 > bits;
    bits.reserve(bv);

    num.toStringUnsigned(bits, 2);
    while (bits.size() < bv) {
      bits.insert(bits.begin(), '0');
    }

    std::reverse(bits.begin(), bits.end());
    return circuit->Create< Constant >( bits.str().str(), bv );
  }

  auto expr_hash  = [] (const z3::expr &e) { return e.hash(); };
  auto expr_equal = [] (const z3::expr &l, const z3::expr &r) { return l.to_string() == r.to_string(); };

  using expr_cache = std::unordered_map< z3::expr, Operation*, decltype(expr_hash), decltype(expr_equal) >;

  Operation* deserialize(const z3::expr &e, Circuit *circuit, expr_cache &seen, IdxCache &idxs)
  {
    if (e.is_numeral()) {
      return constant(e, circuit);
    }

    if (e.is_app()) {
      auto op = app(e, circuit, idxs);

      unsigned num = e.num_args();
      for (unsigned i = 0; i < num; i++) {
        auto arg = e.arg(i);
        if (auto it = seen.find(arg); it != seen.end()) {
          op->AddUse(it->second);
        } else {
          auto darg = deserialize(e.arg(i), circuit, seen, idxs);
          seen.emplace(arg, darg);
          op->AddUse(darg);
        }
      }

      return op;
    }

    unreachable() << "unknown expr " << e;
  }

  std::unique_ptr<Circuit> deserialize(const std::string &path)
  {
    z3::context ctx;
    auto exprs = ctx.parse_file(path.c_str());

    // TODO(lukas): Make configurable.
    auto circuit = std::make_unique< Circuit >(64u);
    check(exprs.size() == 1);

    expr_cache seen(10, expr_hash, expr_equal);
    IdxCache idxs;
    deserialize(exprs[0], circuit.get(), seen, idxs);
    return circuit;
  }

} // namespace circ::smt
