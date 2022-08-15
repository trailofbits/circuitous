#include <optional>

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/APSInt.h>
#include <llvm/ADT/Twine.h>

namespace circ
{
  // std::string node_name(const NodeTemplate &op)
  // {
  //   return std::visit( [] (const auto &o) { return o.op_code_name; }, op );
  // }

  // std::string to_string(const NodeTemplate &op)
  // {
  //   return std::visit( overloaded {
  //     [] (const OpCodeNode    &o) { return o.op_code_name; },
  //     [] (const SizedNode   &o) { return o.op_code_name + "." + std::to_string(o.size.value()); },
  //     [] (const AdviceNode  &o) { return o.op_code_name + "." + std::to_string(o.size.value()) + "." + std::to_string(o.idx.value()); },
  //     [] (const RegisterNode     &o) { return o.op_code_name + "." + o.reg_name; },
  //     [] (const MemoryNode     &o) { return o.op_code_name + "." + std::to_string(o.mem_idx); },
  //     [] (const ExtractNode &o) { return o.op_code_name + "." + std::to_string(o.low_bit_inc) + "." + std::to_string(o.high_bit_exc); },
  //     [] (const SelectNode  &o) { return o.op_code_name + "." + std::to_string(o.size) + "." + std::to_string(o.bits); },
  //     [] (const ConstantNode   &o) {
  //       llvm::SmallVector< char > str;
  //       llvm::APSInt(o.bits).toStringUnsigned(str);
  //       return llvm::Twine(str).str();
  //     }
  //   }, op );
  // }

  // maybe_bitwidth bitwidth(const NodeTemplate &op)
  // {
  //   return std::visit( overloaded {
  //     [] (const OpCodeNode    &o) -> maybe_bitwidth { return std::nullopt; },
  //     [] (const SizedNode   &o) -> maybe_bitwidth { return o.size; },
  //     [] (const AdviceNode  &o) -> maybe_bitwidth { return o.size; },
  //     [] (const RegisterNode     &o) -> maybe_bitwidth { return o.size; },
  //     [] (const MemoryNode     &o) -> maybe_bitwidth { return std::nullopt; },
  //     [] (const ExtractNode &o) -> maybe_bitwidth { return o.high_bit_exc - o.low_bit_inc; },
  //     [] (const SelectNode  &o) -> maybe_bitwidth { return o.size; },
  //     [] (const ConstantNode   &o) -> maybe_bitwidth { return o.size; }
  //   }, op);
  // }

  // std::string name(const circuit_enode *enode) { return node_name( enode->data ); }

  // maybe_bitwidth bitwidth(const CircuitENode *node)
  // {
  //   if (node->is_bond_node()) {
  //     return std::nullopt;
  //   }
  //   return bitwidth(node->data());
  // }

  // bool is_context_node(const CircuitENode *node)
  // {
  //   return name(node) == "VerifyInstruction";
  // }

  // std::optional<llvm::APInt>extract_constant(const CircuitENode *node)
  // {
  //   if (auto con = std::get_if< ConstantNode >(&node->data())) {
  //     return llvm::APInt(con->size, con->bits, 10);
  //   }
  //   return std::nullopt;
  // }

} // namespace cird::eqsat
