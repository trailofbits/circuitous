/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <algorithm>
#include <circuitous/IR/SMT.hpp>
#include <memory>
#include "circuitous/IR/Circuit.hpp"

#include <llvm/ADT/APInt.h>
#include <z3++.h>
#include <z3_api.h>

namespace circ::smt
{
  Operation* app(const z3::expr &e, Circuit *circuit)
  {
    auto decl = e.decl();
    auto name = decl.name().str();
    auto sort = e.get_sort();

    CHECK(sort.is_bool() || sort.is_bv());
    auto bv = sort.is_bv() ? sort.bv_size() : 1;

    if (name == "Circuit")
      return circuit;

    if (name == "extract")
      return circuit->Create< Extract >(e.lo(), e.hi() + 1);

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

    else if (name == "Advice")
      return circuit->Create< Advice >(bv);

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

    else if (name == "bvadd")
      return circuit->Create< Add >(bv);
    else if (name == "bvsub")
      return circuit->Create< Sub >(bv);
    else if (name == "bvmul")
      return circuit->Create< Mul >(bv);

    else if (name == "bvudiv")
      return circuit->Create< UDiv >(bv);
    else if (name == "bvsdiv")
      return circuit->Create< SDiv >(bv);

    else if (name == "bvand")
      return circuit->Create< CAnd >(bv);
    else if (name == "bvor")
      return circuit->Create< COr >(bv);
    else if (name == "bvxor")
      return circuit->Create< CXor >(bv);

    else if (name == "bvshl")
      return circuit->Create< Shl >(bv);
    else if (name == "bvlshr")
      return circuit->Create< LShr >(bv);
    else if (name == "bvashr")
      return circuit->Create< AShr >(bv);

    // else if (name == "bvadd")
    //  return circuit->Create< Trunc >();
    // else if (name == "bvadd")
    //   return circuit->Create< ZExt >();
    // else if (name == "bvadd")
    //   return circuit->Create< SExt >();

    else if (name == "bvult")
      return circuit->Create< Icmp_ult >(bv);
    else if (name == "bvslt")
      return circuit->Create< Icmp_slt >(bv);
    else if (name == "bvugt")
      return circuit->Create< Icmp_ugt >(bv);
    else if (name == "=")
      return circuit->Create< Icmp_eq >(bv);
    else if (name == "!=")
      return circuit->Create< Icmp_ne >(bv);
    else if (name == "bvuge")
      return circuit->Create< Icmp_uge >(bv);
    else if (name == "bvule")
      return circuit->Create< Icmp_ule >(bv);
    else if (name == "bvsgt")
      return circuit->Create< Icmp_sgt >(bv);
    else if (name == "bvsge")
      return circuit->Create< Icmp_sge >(bv);
    else if (name == "bvsle")
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

    else if (name == "BSelect")
      return circuit->Create< BSelect >(bv);

    LOG(FATAL) << "unknown operation " << e << " " << name;
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

  Operation* deserialize(const z3::expr &e, Circuit *circuit)
  {
    if (e.is_numeral()) {
      return constant(e, circuit);
    }

    if (e.is_ite()) {
      return deserialize(e.arg(0), circuit);
    }

    if (e.is_app()) {
      auto op = app(e, circuit);

      unsigned num = e.num_args();
      for (unsigned i = 0; i < num; i++)
        op->AddUse(deserialize(e.arg(i), circuit));

      return op;
    }

    LOG(FATAL) << "unknown expr " << e;
  }

  std::unique_ptr<Circuit> deserialize(const std::string &path)
  {
    z3::context ctx;
    auto exprs = ctx.parse_file(path.c_str());

    auto circuit = std::make_unique< Circuit >();
    CHECK(exprs.size() == 1);
    deserialize(exprs[0], circuit.get());
    return circuit;
  }

} // namespace circ::smt