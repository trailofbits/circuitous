#include <circuitous/Transforms/EqSat/Graph.hpp>

#include <circuitous/Util/Overloads.hpp>
#include <optional>

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/APSInt.h>
#include <llvm/ADT/Twine.h>

namespace circ::eqsat
{
  std::string node_name(const OpTemplate &op)
  {
    return std::visit( [] (const auto &o) { return o.op_code_name; }, op );
  }

  std::string to_string(const OpTemplate &op)
  {
    return std::visit( overloaded {
      [] (const OpCode    &o) { return o.op_code_name; },
      [] (const SizedOp   &o) { return o.op_code_name + "." + std::to_string(o.size.value()); },
      [] (const AdviceOp  &o) { return o.op_code_name + "." + std::to_string(o.size.value()) + "." + std::to_string(o.idx.value()); },
      [] (const RegOp     &o) { return o.op_code_name + "." + o.reg_name; },
      [] (const MemOp     &o) { return o.op_code_name + "." + std::to_string(o.mem_idx); },
      [] (const ExtractOp &o) { return o.op_code_name + "." + std::to_string(o.low_bit_inc) + "." + std::to_string(o.high_bit_exc); },
      [] (const SelectOp  &o) { return o.op_code_name + "." + std::to_string(o.size) + "." + std::to_string(o.bits); },
      [] (const ConstOp   &o) {
        llvm::SmallVector< char > str;
        llvm::APSInt(o.bits).toStringUnsigned(str);
        return llvm::Twine(str).str();
      }
    }, op );
  }

  std::string name(const CircuitENode *node) { return node_name( *node ); }

  using maybe_bitwidth = std::optional< uint32_t >;

  maybe_bitwidth bitwidth(const OpTemplate &op)
  {
    return std::visit( overloaded {
      [] (const OpCode    &o) -> maybe_bitwidth { return std::nullopt; },
      [] (const SizedOp   &o) -> maybe_bitwidth { return o.size; },
      [] (const AdviceOp  &o) -> maybe_bitwidth { return o.size; },
      [] (const RegOp     &o) -> maybe_bitwidth { return o.size; },
      [] (const MemOp     &o) -> maybe_bitwidth { return std::nullopt; },
      [] (const ExtractOp &o) -> maybe_bitwidth { return o.high_bit_exc - o.low_bit_inc; },
      [] (const SelectOp  &o) -> maybe_bitwidth { return o.size; },
      [] (const ConstOp   &o) -> maybe_bitwidth { return o.size; }
    }, op);
  }

  maybe_bitwidth bitwidth(const CircuitENode *node)
  {
    if (node->is_bond_node()) {
      return std::nullopt;
    }
    return bitwidth(node->data());
  }

  bool is_context_node(const CircuitENode *node)
  {
    return name(node) == "VerifyInstruction";
  }

  std::optional<llvm::APInt>extract_constant(const CircuitENode *node)
  {
    if (auto con = std::get_if< ConstOp >(&node->data())) {
      return llvm::APInt(con->size, con->bits, 10);
    }
    return std::nullopt;
  }

} // namespace cird::eqsat
