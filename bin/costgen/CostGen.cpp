/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/IR.h>

#include <circuitous/Printers/Verilog.hpp>
#include <circuitous/Util/Logging.hpp>

#include <fstream>
#include <iostream>
#include <tuple>
#include <unordered_map>
#include <vector>

DEFINE_string(output, "", "File to store the generated code");

namespace circ {

  struct Ctx {
    Circuit circuit;
    using entry_t = std::tuple< Operation *, std::string >;
    std::vector< entry_t > todo;

    Ctx(uint32_t ptr_size): circuit(ptr_size) {}

    Operation *leaf(uint32_t size) {
      std::string bits(size, '1');
      return circuit.Create< Constant >(std::move(bits), size);
    }

    template< typename T, typename ... Args >
    void add(uint32_t operands_count,
             uint32_t operands_size, Args &&...args)
    {
      auto op = circuit.Create< T >( std::forward< Args >(args) ... );
      for (uint32_t i = 0; i < operands_count; ++i)
        op->AddUse(leaf(operands_size));
      todo.emplace_back(op, "");
    }

    template< typename T, typename ... Args >
    void add(const std::vector< Operation * > &ops, Args && ...args)
    {
     auto op = circuit.Create< T >( std::forward< Args >(args) ... );
     for (auto x : ops)
        op->AddUse(x);
      todo.emplace_back(op, "");
    }

    void annotate(std::string msg) {
      CHECK(todo.size() != 0);
      auto &[op, _msg] = todo.back();
      CHECK(_msg.empty());
      _msg = std::move(msg);
    }

    void write(const std::string &file_name) {
      CHECK(!file_name.empty());
      std::ofstream file(file_name);
      for (auto &[op, msg] : todo)
      {
        file << msg;
        circ::print::verilog::print_solo(file, &circuit, op);
        file << std::endl;
      }
    }
  };


  void generate() {
    Ctx ctx{32};

    ctx.add< Or >(5, 1);
    {
      ctx.annotate("// Logical or.\n");
    }
    ctx.add< And >(5, 1);
    {
      ctx.annotate("// Logical and.\n");
    }
    // TODELETE
    //ctx.add< Not >(1, 1);
    //ctx.add< Not >(1, 32);

    ctx.add< AdviceConstraint >(2, 32);
    {
      ctx.annotate("// Comparison.\n");
    }

    ctx.add< Extract >(1, 128, 0u, 32u);
    {
      ctx.annotate("// Extract [0, 32).\n");
    }
    ctx.add< Extract >(1, 128, 32u, 64u);
    {
      ctx.annotate("// Extract [32, 64).\n");
    }

    // LLVMOps
    ctx.add< Add >(2, 32, 32u);
    { ctx.annotate("// Addition.\n"); }
    ctx.add< Sub >(2, 32, 32u);
    { ctx.annotate("// Sub.\n"); }
    ctx.add< Mul >(2, 32, 32u);
    { ctx.annotate("// Multiplication.\n"); }
    ctx.add< UDiv >(2, 32, 32u);
    { ctx.annotate("// Unsigned division.\n"); }
    ctx.add< SDiv >(2, 32, 32u);
    { ctx.annotate("// Signed division.\n"); }
    ctx.add< Shl >(2, 32, 32u);
    { ctx.annotate("// First operand shifted to left by number specified by second..\n"); }
    ctx.add< LShr >(2, 32, 32u);
    { ctx.annotate("// Logical shift right.\n"); }
    ctx.add< AShr >(2, 32, 32u);
    { ctx.annotate("// Arithmetic shift right.\n"); }

    ctx.add< Trunc >(1, 32, 16u);
    { ctx.annotate("// Truncate to 16b.\n"); }
    ctx.add< ZExt >(1, 16, 32u);
    { ctx.annotate("// Zero extend to 32b.\n"); }
    ctx.add< SExt >(1, 16, 32u);
    { ctx.annotate("// Sign extend to 32b.\n"); }

    ctx.add< Icmp_ult >(2, 32, 1u);
    { ctx.annotate("// op1 is less than op2.\n"); }
    ctx.add< Icmp_slt >(2, 32, 1u);
    { ctx.annotate("// signed op1 is less than signed op2.\n"); }
    ctx.add< Icmp_ugt >(2, 32, 1u);
    { ctx.annotate("// op1 greater than op2..\n"); }
    ctx.add< Icmp_eq >(2, 32, 1u);
    { ctx.annotate("// Eq.\n"); }
    ctx.add< Icmp_ne >(2, 32, 1u);
    { ctx.annotate("// Ne.\n"); }
    ctx.add< Icmp_uge >(2, 32, 1u);
    { ctx.annotate("// op1 greater equal than op2.\n"); }
    ctx.add< Icmp_ule >(2, 32, 1u);
    { ctx.annotate("// op1 less equal than op2.\n"); }
    ctx.add< Icmp_sgt >(2, 32, 1u);
    { ctx.annotate("// signed op1 greater than signed op2.\n"); }
    ctx.add< Icmp_sge >(2, 32, 1u);
    { ctx.annotate("// signed op1 greater equal signed op2.\n"); }
    ctx.add< Icmp_sle >(2, 32, 1u);
    { ctx.annotate("// signed op1 less equal signed op2.\n"); }

    ctx.add< CAnd >(2, 32, 32u);
    { ctx.annotate("// And.\n"); }
    ctx.add< COr >(2, 32, 32u);
    { ctx.annotate("// Or.\n"); }
    ctx.add< CXor >(2, 32, 32u);
    { ctx.annotate("// Xor.\n"); }

    ctx.add< BSelect >({ctx.leaf(1), ctx.leaf(32), ctx.leaf(32)}, 32u);
    { ctx.annotate("// if (op1) ? op2 : op3.\n"); }
    std::vector< Operation * > tail{8, ctx.leaf(32)};
    tail.insert(tail.begin(), ctx.leaf(3));

    ctx.add< Select >(tail, 3u, 32u);
    {
      std::stringstream ss;
      ss << "// Basic mux." << std::endl;
      ss << "// First operand is a selector" << std::endl;
      ss << "// Returns operand that is on index that is value of selector." << std::endl;
      ss << "// E.g. if selector is 0, select first, if 1 select second etc." << std::endl;
      ctx.annotate(ss.str());
    }

    //ctx.add< ReadConstraint >(32, 2);
    //ctx.add< WriteConstraint >(32, 2);
    ctx.add< Concat >({ctx.leaf(32), ctx.leaf(16), ctx.leaf(8)}, 56u);
    {
      std::stringstream ss;
      ss << "// Concat." << std::endl;
      ss << "// Expected that concat( A, B ) will do AB," << std::endl;
      ss << "// e.g. extract on result [0:0] will return B[0:0]" << std::endl;
      ss << "// and trunc back to size of `B` will return `B`." << std::endl;
      ctx.annotate(ss.str());
    }
    ctx.add< VerifyInstruction >(6, 1);
    { ctx.annotate("// Each operand is true.\n"); }
    ctx.add< OnlyOneCondition >(6, 1);
    {
      std::stringstream ss;
      ss << "// Each extra operand V(n + 1) introduces.\n";
      ss << "// R(n) - result of computation - true if at least one `1` is present.\n";
      ss << "// O(n) - result of overflow flag - true if at least two `1` were present.\n";
      ss << "// check_overflow(O, R, V) : O || (R && V).\n";
      ss << "//   overflow is saturared and it is raised if both current value and previous.\n";
      ss << "//   are set to `1` (i.e. there are at least two `1`s in inputs).\n";
      ss << "// R(n + 1) := R(n) || V(n + 1).\n";
      ss << "// O(n + 1) := check_overflow(O(n), R(n), V(n + 1)).\n";
      ss << "// Final value is then computed as.\n";
      ss << "// !O(m) && R(m).\n";
      ss << "//   i.e. overflow did not happen, and at least one `1` was found..\n";
      ctx.annotate(ss.str());
    }

    ctx.add< PopulationCount >(1, 32, 32u);
    { ctx.annotate("// Population count.\n"); }
    ctx.add< CountLeadingZeroes >(1, 32, 32u);
    {
      std::stringstream ss;
      ss << "// Consider input is `A`" << std::endl;
      ss << "// `t` holds total count, `f` saturates to `0` if `1` was found." << std::endl;
      ss << "// f_(n + 1) = f_n && !A[n]" << std::endl;
      ss << "// t_(n + 1) = t_n + { 0 padding, f_(n + 1) }" << std::endl;
      ss << "// Final result is padded last value of t_n." << std::endl;
      ctx.annotate(ss.str());
    }
    ctx.add< CountTrailingZeroes >(1, 32, 32u);
    { ctx.annotate("// CountTrailingZeroes.\n"); }

    //ctx.add< ReadConstraint >(32, 2);
    //ctx.add< WriteConstraint >(32, 2);

    ctx.write(FLAGS_output);
  }

} // namespace circ

int main(int argc, char *argv[]) {
  google::ParseCommandLineFlags(&argc, &argv, true);
  google::InitGoogleLogging(argv[0]);

  circ::generate();
  return 0;
}
