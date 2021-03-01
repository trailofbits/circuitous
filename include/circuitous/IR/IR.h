/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Util/UseDef.h>

#include <bitset>
#include <iosfwd>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

namespace remill {
class Arch;
}  // namespace remill
namespace llvm {
class Constant;
class Function;
class Instruction;
class StringRef;
}  // namespace llvm
namespace circuitous {

// We can try to do some optimizations, but we want to be
// able to toggle them (if for nothing we want to be able to
// test them)
struct Optimizations {
  bool reduce_imms = false;
};

template <typename Derived>
class Visitor;

class Circuit;

// A general instruction.
class Operation : public User, public Def<Operation> {
 public:
  virtual ~Operation(void);

  virtual std::string Name(void) const = 0;
  virtual Operation *CloneWithoutOperands(Circuit *) const = 0;

  virtual bool Equals(const Operation *that) const;

  enum : unsigned {

    // Some sequence of bits.
    kConstant,

    // An undefined value, either an `llvm::Undef` or a `__remill_undefined_*`
    // value.
    kUndefined,

    // Every LLVM instruction kind. Things like Add, Sub, etc.
    kLLVMOperation,

    // Flip all bits.
    kNot,

    // Extract some selection of bits [low_inc, high_exc) from an operand.
    //
    // NOTE(pag): `low_inc` is the lowest order bit, `high_exc` is the highest
    //             order bit.
    kExtract,

    // Concatenate an N and an M bit value, producing an (N+M)-bit value.
    kConcat,

    // Compute the population count of some bit string.
    kPopulationCount,

    // Compute the parity of some bit string. This produces a single bit result.
    kParity,

    // Count the leading zeroes of a bit string.
    kCountLeadingZeroes,

    // Count the trailing zeros of a bit string.
    kCountTrailingZeroes,

    // Memory read condition. In bitcode, we might have the following:
    //
    //      v = __remill_read_memory_32(mem, addr)
    //
    // In the circuit, we actually want to have `v` be a hint value, and the
    // expected value of `addr` be a hint value, and then this condition
    // just checks the following:
    //
    //      computed_addr == Hint()
    //
    // It is up to the memory trace verifier to actually verify that `v` is the
    // right thing to read from `addr`.
    //
    // Produces a single bit as output.
    kReadMemoryCondition,

    // Named register that is an input to the circuit, representing the value
    // of the register before the instruction "executes".
    kInputRegister,

    kInputImmediate,

    // Named register that is a hint input to teh circuit, representing the
    // value of the register after the instruction "executes". We need to verify
    // that the computed value of the register after the instruction matches
    // this value, which is what `kRegisterCondition` is for.
    kOutputRegister,

    // Input to the circuit representing the bits of the instruction.
    kInputInstructionBits,

    // A condition generated as part of optimizations, where a hint is generated.
    kHintCondition,

    // Checks that a computed register's value matches some output value, e.g.:
    //
    //      new_EAX == OutputRegister(EAX)
    //
    // Produces a single bit as output.
    kRegisterCondition,

    // Checks that a register's value is preserved, e.g.:
    //
    //      InputRegister(EAX) == OutputRegister(EAX)
    //
    // Produces a single bit as output.
    kPreservedCondition,

    // Checks that a register's value is actually a copy of another register,
    // e.g.:
    //
    //      InputRegister(EBX) == OutputRegister(EAX)
    //
    // Produces a single bit as output.
    kCopyCondition,

    // Condition that checks if a proposed encoding matches some input
    // instruction bits.
    //
    // Produces a single bit as output.
    kDecodeCondition,

    // A condition representing the result of verifying one or more instruction
    // transitions. Initially, there is one verification per unique instruction
    // encoding. For example, if we have `mov eax, ebx` and `mov ebx, eax`,
    // then we will have two uses of `kVerifyInstruction`. Later, multiple
    // verifications might be merged onto one.
    //
    // This is semantically equivalent to an AND of N bits, i.e. `b1 & .. & bN`.
    // The semantic is that all of the inputs must be `1` in order for this
    // function to have "verified" some instruction.
    //
    // Produces a single bit as output.
    kVerifyInstruction,

    // An input hint value of some size.
    kHint,

    // A XOR of N bits, i.e. `b1 ^ ... ^ bN`. We construct the circuit such
    // that we know that only one of the bits will ever be `1`, and all others
    // will be zero.
    kOnlyOneCondition,

    // Output value (0 or 1) of the circuit. This is really just a specialized
    // equivalence class, where all the inputs are equivalent ways of expressing
    // the circuit, of which the minimum cost one is chosen.
    kCircuit
  };

  // The "opcode" of this.
  const unsigned op_code{0};

  // Size in bits of this instruction's "result" value. For example, a zero-
  // extension will represent the size of the output value.
  const unsigned size{0};

  // Ordered list of operands. For example, if we had `add a, b`, then
  // likely what we have is really an equivalence class pointing to two
  // instructions: `add a, b` and `add b, a`.
  UseList<Operation> operands;

  template <typename Vis>
  void Traverse(Vis &vis) {
    for (auto op : operands) {
      vis.Visit(op);
    }
  }

 protected:
  explicit Operation(unsigned op_code_, unsigned size_);
};

#define FORWARD_CONSTRUCTOR(base_class, derived_class) \
  inline explicit derived_class(unsigned size_) \
      : base_class(Operation::k##derived_class, size_) {}


#define CONDITION_CONSTRUCTOR(derived_class) \
  inline explicit derived_class(void) \
      : Condition(Operation::k##derived_class) {}


// Mirrors an instruction from LLVM. `op_code` is `inst->getOpcode()`.
class LLVMOperation final : public Operation {
 public:
  explicit LLVMOperation(unsigned llvm_opcode_, unsigned llvm_predicate_,
                         unsigned size_);

  explicit LLVMOperation(llvm::Instruction *inst_);
  virtual ~LLVMOperation(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  const unsigned llvm_op_code;
  const unsigned llvm_predicate;

  static const unsigned kInvalidLLVMPredicate;
};

// An undefined value.
class Undefined final : public Operation {
 public:
  inline explicit Undefined(unsigned size_)
      : Operation(Operation::kUndefined, size_) {}

  virtual ~Undefined(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;

 protected:
  using Operation::Operation;
};

class Constant final : public Operation {
 public:
  inline explicit Constant(std::string bits_, unsigned size_)
      : Operation(Operation::kConstant, size_),
        bits(bits_) {}

  virtual ~Constant(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  // Value of this constant. The least significant bit is stored in `bits[0]`,
  // and the most significant bit is stored in `bits[size - 1u]`.
  const std::string bits;
};

// Some bitwise operation; must be extended.
class BitOperation : public Operation {
 public:
  virtual ~BitOperation(void);

 protected:
  using Operation::Operation;
};

// Flip all bits in a bitvector.
class Not final : public BitOperation {
 public:
  FORWARD_CONSTRUCTOR(BitOperation, Not)

  virtual ~Not(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// Extract bits.
class Extract final : public BitOperation {
 public:
  virtual ~Extract(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  inline explicit Extract(unsigned low_bit_inc_, unsigned high_bit_exc_)
      : BitOperation(Operation::kExtract, high_bit_exc_ - low_bit_inc_),
        low_bit_inc(low_bit_inc_),
        high_bit_exc(high_bit_exc_) {}

  const unsigned low_bit_inc;
  const unsigned high_bit_exc;
};

// Concatenate two bitvectors.
class Concat final : public BitOperation {
 public:
  FORWARD_CONSTRUCTOR(BitOperation, Concat)

  virtual ~Concat(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// Population count of some bits. Needed for things like parity count
// calculation.
class PopulationCount final : public BitOperation {
 public:
  FORWARD_CONSTRUCTOR(BitOperation, PopulationCount)
  virtual ~PopulationCount(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// Count the leading zeroes of some bits.
class CountLeadingZeroes final : public BitOperation {
 public:
  FORWARD_CONSTRUCTOR(BitOperation, CountLeadingZeroes)
  virtual ~CountLeadingZeroes(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

class CountTrailingZeroes final : public BitOperation {
 public:
  FORWARD_CONSTRUCTOR(BitOperation, CountTrailingZeroes)
  virtual ~CountTrailingZeroes(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

class Condition : public Operation {
 public:
  virtual ~Condition(void);

 protected:
  inline explicit Condition(unsigned op_code_) : Operation(op_code_, 1u) {}
};

// Returns `0` if there are an even numbers of bits in a bit string, and `1`
// if there are an odd number of bits in a bit string.
class Parity final : public Condition {
 public:
  CONDITION_CONSTRUCTOR(Parity)
  virtual ~Parity(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// Condition that verifies that some computed address matches a hint address.
class ReadMemoryCondition final : public Condition {
 public:
  enum : unsigned {
    kDynamicAddress = 0u,
    kHintedAddress = 1u,
    kHintedValue = 2u
  };
  CONDITION_CONSTRUCTOR(ReadMemoryCondition)
  virtual ~ReadMemoryCondition(void);
  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// An input register.
class InputRegister : public Operation {
 public:
  virtual ~InputRegister(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  inline explicit InputRegister(unsigned size_, std::string reg_name_)
      : Operation(Operation::kInputRegister, size_),
        reg_name(std::move(reg_name_)) {}

  const std::string reg_name;
};

class InputImmediate : public Operation {
  public:
    virtual ~InputImmediate(void) = default;

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  explicit InputImmediate(unsigned size_)
      : Operation(Operation::kInputImmediate, size_) {}
};

// An output register.
class OutputRegister : public Operation {
 public:
  virtual ~OutputRegister(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  inline explicit OutputRegister(unsigned size_, std::string reg_name_)
      : Operation(Operation::kOutputRegister, size_),
        reg_name(std::move(reg_name_)) {}

  const std::string reg_name;
};

// A comparison between the proposed output value of a register, and the
// output register itself.
class RegisterCondition final : public Condition {
 public:
  enum : unsigned { kDynamicRegisterValue = 0u, kOutputRegister = 1u };

  CONDITION_CONSTRUCTOR(RegisterCondition)
  virtual ~RegisterCondition(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// A comparison between the proposed output value of a register, and the
// output register itself.
class HintCondition final : public Condition {
 public:
  enum : unsigned { kDynamicValue = 0u, kHint = 1u };

  CONDITION_CONSTRUCTOR(HintCondition)
  virtual ~HintCondition(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// Says that we are preserving the value of a register.
class PreservedCondition final : public Condition {
 public:
  enum : unsigned { kInputRegister = 0u, kOutputRegister = 1u };

  CONDITION_CONSTRUCTOR(PreservedCondition)
  virtual ~PreservedCondition(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// Says that we are moving one register to a different register.
class CopyCondition final : public Condition {
 public:
  enum : unsigned { kOtherInputRegister = 0u, kOutputRegister = 1u };

  CONDITION_CONSTRUCTOR(CopyCondition)
  virtual ~CopyCondition(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// Input bits that
class InputInstructionBits : public Operation {
 public:
  FORWARD_CONSTRUCTOR(Operation, InputInstructionBits)
  virtual ~InputInstructionBits(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// Checks whether or not some instruction bits (second operand) equals some
// proposed values.
class DecodeCondition final : public Condition {
 public:
  CONDITION_CONSTRUCTOR(DecodeCondition)
  virtual ~DecodeCondition(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// A hint value is an input to the circuit. It can serve several purposes,
// e.g.
class Hint final : public Operation {
 public:
  virtual ~Hint(void);

  inline explicit Hint(unsigned size_)
      : Operation(Operation::kHint, size_),
        weak_conditions(this, true) {}

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;

  // Weak list of conditions that compare this hint value against what it is
  // hinting.
  UseList<Operation> weak_conditions;
};

class VerifyInstruction final : public Condition {
 public:
  CONDITION_CONSTRUCTOR(VerifyInstruction)
  virtual ~VerifyInstruction(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

class OnlyOneCondition final : public Condition {
 public:
  CONDITION_CONSTRUCTOR(OnlyOneCondition)
  virtual ~OnlyOneCondition(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

#undef FORWARD_CONSTRUCTOR
#undef CONDITION_CONSTRUCTOR

// Represents the results of verifying a circuit.
class Circuit : public Condition {
 public:
  virtual ~Circuit(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;

  using CircuitPtr = std::unique_ptr<Circuit>;


  static CircuitPtr CreateFromInstructions(const std::string &arch_name,
                                           const std::string &os_name,
                                           const std::string &file_name,
                                           const Optimizations &opts={});

  static CircuitPtr CreateFromInstructions(const std::string &arch_name,
                                           const std::string &os_name,
                                           const llvm::StringRef &bytes,
                                           const Optimizations &opts={});

  static CircuitPtr CreateFromInstructions(const std::string &arch_name,
                                           const std::string &os_name,
                                           std::string_view bytes,
                                           const Optimizations &opts={});
 public:
  void Serialize(std::ostream &os);

  void Serialize(std::function<std::ostream &(const std::string &)> os_opener);

  static std::unique_ptr<Circuit> Deserialize(std::istream &is);

  void RemoveUnused(void);

  // clang-format off

#define FOR_EACH_OPERATION(cb) \
  cb(Constant, constants) \
  cb(Undefined, undefs) \
  cb(LLVMOperation, llvm_insts) \
  cb(Not, nots) \
  cb(Concat, concats) \
  cb(CountLeadingZeroes, clzs) \
  cb(CountTrailingZeroes, ctzs) \
  cb(Extract, extracts) \
  cb(PopulationCount, popcounts) \
  cb(Parity, parities) \
  cb(InputRegister, input_regs) \
  cb(InputImmediate, input_imms) \
  cb(OutputRegister, output_regs) \
  cb(InputInstructionBits, inst_bits) \
  cb(RegisterCondition, transitions) \
  cb(PreservedCondition, preserved_regs) \
  cb(CopyCondition, copied_regs) \
  cb(DecodeCondition, decode_conditions) \
  cb(VerifyInstruction, verifications) \
  cb(ReadMemoryCondition, memory_reads) \
  cb(OnlyOneCondition, xor_all) \
  cb(Hint, hints) \
  cb(HintCondition, hint_conds)

  // clang-format on

#define DECLARE_MEMBER(type, field) DefList<type> field;

  FOR_EACH_OPERATION(DECLARE_MEMBER)
#undef DECLARE_MEMBER

  template <typename CB>
  void ForEachOperation(CB cb) {
    std::vector<Operation *> ops;

#define PUSH_OP(type, field) \
  for (auto op : field) { \
    ops.push_back(op); \
  }

    FOR_EACH_OPERATION(PUSH_OP)
#undef PUSH_OP

    for (auto op : ops) {
      cb(op);
    }
  }

 private:
  Circuit(void);
};

// Visitor for visiting IR.
template <typename Derived>
class Visitor {
 public:
  void VisitOperation(Operation *op) {
    op->Traverse(*this);
  }

  void Visit(Operation *op) {
    auto self = static_cast<Derived *>(this);
    switch (const auto op_code = op->op_code; op_code) {
#define VISITOR_CASE(type, field) \
  case Operation::k##type: \
    if (auto typed_op = dynamic_cast<type *>(op); typed_op) { \
      self->Visit##type(typed_op); \
    } else { \
      LOG(FATAL) << "Node with op_code=" << op_code << " (k" << #type \
                 << ") has incorrect dynamic type: " << typeid(*op).name(); \
    } \
    break;

      FOR_EACH_OPERATION(VISITOR_CASE)
      VISITOR_CASE(Circuit, ...)
#undef VISITOR_CASE
      default: LOG(FATAL) << "Unhandled operation: " << op->Name();
    }
  }

#define DECLARE_VISITOR(type, field) \
  void Visit##type(type *op) { \
    auto self = static_cast<Derived *>(this); \
    self->VisitOperation(op); \
  }

  FOR_EACH_OPERATION(DECLARE_VISITOR)
  DECLARE_VISITOR(Circuit, ...)
#undef DECLARE_VISITOR
};

template <typename Derived>
class UniqueVisitor : public Visitor<Derived> {
 public:
  void Visit(Operation *op) {
    if (seen_ops.count(op)) {
      return;
    }
    seen_ops.insert(op);
    this->Visitor<Derived>::Visit(op);
  }
  void Reset(void) {
    seen_ops.clear();
  }

 private:
  std::unordered_set<Operation *> seen_ops;
};

}  // namespace circuitous
