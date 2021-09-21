#include <circuitous/Transforms/EqSat/Graph.hpp>

#include <circuitous/Util/Overloads.hpp>

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/APSInt.h>
#include <llvm/ADT/Twine.h>

namespace circ::eqsat {

  std::string to_string(const OpTemplate &op)
  {
    return std::visit( overloaded {
      [] (const OpCode    &o) { return o.op_code_name; },
      [] (const SizedOp   &o) { return o.op_code_name + "." + std::to_string(o.size.value()); },
      [] (const AdviceOp  &o) { return o.op_code_name + "." + std::to_string(o.idx.value()); },
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

  std::string name(const CircuitENode *node)
  {
    return std::visit( [] (const auto &value) { return value.op_code_name; }, node->term );
  }

  bool is_context_node(const CircuitENode *node)
  {
    return name(node) == "VerifyInstruction";
  }

  std::optional<std::int64_t> extract_constant(const CircuitENode *node)
  {
    throw std::runtime_error("not implemented");
  }

} // namespace cird::eqsat
