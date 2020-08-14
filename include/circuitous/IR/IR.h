/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Util/UseDef.h>

#include <bitset>
#include <iosfwd>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

namespace remill {
class Arch;
}  // namespace remill
namespace llvm {
class Constant;
class Function;
class Instruction;
}  // namespace llvm
namespace circuitous {

class EquivalenceClass;

template <typename Derived>
class Visitor;

// A general instruction.
class Operation : public User, public Def<Operation> {
 public:
  virtual ~Operation(void);

  virtual std::string Name(void) const = 0;

  virtual bool Equals(const Operation *that) const;

  enum : unsigned {

    // Some sequence of bits.
    kConstant,

    // Every LLVM instruction kind. Things like Add, Sub, etc.
    kLLVMOperation,

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

    // Named register that is a hint input to teh circuit, representing the
    // value of the register after the instruction "executes". We need to verify
    // that the computed value of the register after the instruction matches
    // this value, which is what `kRegisterCondition` is for.
    kOutputRegister,

    // Input to the circuit representing the bits of the instruction.
    kInputInstructionBits,

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

    // A node whose operands are all equivalent. The idea is to choose the
    // minimum cost operand, and ignore the rest.
    kEquivalenceClass,

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

  // The equivalence class to which this operation belongs.
  //
  // NOTE(pag): This is a weak use.
  UseRef<Operation> eq_class;

  template <typename Vis>
  void Traverse(Vis &vis) {
    for (auto op : operands) {
      vis.Visit(op);
    }
  }

 protected:
  explicit Operation(unsigned op_code_, unsigned size_);
  explicit Operation(unsigned op_code_, unsigned size_, Operation *eq_class_);
};

#define FORWARD_CONSTRUCTOR(base_class, derived_class) \
  inline explicit derived_class(unsigned size_) \
      : base_class(Operation::k##derived_class, size_) {} \
\
  inline explicit derived_class(unsigned size_, Operation *eq_class_) \
      : base_class(Operation::k##derived_class, size_, eq_class_) {}


#define CONDITION_CONSTRUCTOR(derived_class) \
  inline explicit derived_class(void) \
      : Condition(Operation::k##derived_class) {} \
\
  inline explicit derived_class(Operation *eq_class_) \
      : Condition(Operation::k##derived_class, eq_class_) {}


// Mirrors an instruction from LLVM. `op_code` is `inst->getOpcode()`.
class LLVMOperation final : public Operation {
 public:
  explicit LLVMOperation(unsigned llvm_opcode_, unsigned llvm_predicate_,
                         unsigned size_);
  explicit LLVMOperation(unsigned llvm_opcode_, unsigned llvm_predicate_,
                         unsigned size_, Operation *eq_class_);

  explicit LLVMOperation(llvm::Instruction *inst_);
  explicit LLVMOperation(llvm::Instruction *inst_, Operation *eq_class_);
  virtual ~LLVMOperation(void);

  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  const unsigned llvm_op_code;
  const unsigned llvm_predicate;

  static const unsigned kInvalidLLVMPredicate;
};

class Constant final : public Operation {
 public:
  inline explicit Constant(std::string bits_, unsigned size_)
      : Operation(Operation::kConstant, size_),
        bits(bits_) {}

  inline explicit Constant(std::string bits_, unsigned size_,
                           Operation *eq_class_)
      : Operation(Operation::kConstant, size_, eq_class_),
        bits(bits_) {}

  virtual ~Constant(void);

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

// Extract bits.
class Extract final : public BitOperation {
 public:
  virtual ~Extract(void);
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  inline explicit Extract(unsigned low_bit_inc_, unsigned high_bit_exc_)
      : BitOperation(Operation::kExtract, high_bit_exc_ - low_bit_inc_),
        low_bit_inc(low_bit_inc_),
        high_bit_exc(high_bit_exc_) {}

  inline explicit Extract(unsigned low_bit_inc_, unsigned high_bit_exc_,
                          Operation *eq_class_)
      : BitOperation(Operation::kExtract, high_bit_exc_ - low_bit_inc_,
                     eq_class_),
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
  std::string Name(void) const override;
};

// Population count of some bits. Needed for things like parity count
// calculation.
class PopulationCount final : public BitOperation {
 public:
  FORWARD_CONSTRUCTOR(BitOperation, PopulationCount)
  virtual ~PopulationCount(void);
  std::string Name(void) const override;
};

// Count the leading zeroes of some bits.
class CountLeadingZeroes final : public BitOperation {
 public:
  FORWARD_CONSTRUCTOR(BitOperation, CountLeadingZeroes)
  virtual ~CountLeadingZeroes(void);
  std::string Name(void) const override;
};

class CountTrailingZeroes final : public BitOperation {
 public:
  FORWARD_CONSTRUCTOR(BitOperation, CountTrailingZeroes)
  virtual ~CountTrailingZeroes(void);
  std::string Name(void) const override;
};

class Condition : public Operation {
 public:
  virtual ~Condition(void);

 protected:
  inline explicit Condition(unsigned op_code_) : Operation(op_code_, 1u) {}

  inline explicit Condition(unsigned op_code_, Operation *eq_class_)
      : Operation(op_code_, 1u, eq_class_) {}
};

// Returns `0` if there are an even numbers of bits in a bit string, and `1`
// if there are an odd number of bits in a bit string.
class Parity final : public Condition {
 public:
  CONDITION_CONSTRUCTOR(Parity)
  virtual ~Parity(void);
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
  std::string Name(void) const override;
};

// An input register.
class InputRegister : public Operation {
 public:
  virtual ~InputRegister(void);
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  inline explicit InputRegister(unsigned size_, std::string reg_name_)
      : Operation(Operation::kInputRegister, size_),
        reg_name(std::move(reg_name_)) {}

  const std::string reg_name;
};

// An output register.
class OutputRegister : public Operation {
 public:
  virtual ~OutputRegister(void);
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
  std::string Name(void) const override;
};

// Says that we are preserving the value of a register.
class PreservedCondition final : public Condition {
 public:
  enum : unsigned { kInputRegister = 0u, kOutputRegister = 1u };

  CONDITION_CONSTRUCTOR(PreservedCondition)
  virtual ~PreservedCondition(void);
  std::string Name(void) const override;
};

// Says that we are moving one register to a different register.
class CopyCondition final : public Condition {
 public:
  enum : unsigned { kOtherInputRegister = 0u, kOutputRegister = 1u };

  CONDITION_CONSTRUCTOR(CopyCondition)
  virtual ~CopyCondition(void);
  std::string Name(void) const override;
};

// Input bits that
class InputInstructionBits : public Operation {
 public:
  FORWARD_CONSTRUCTOR(Operation, InputInstructionBits)
  virtual ~InputInstructionBits(void);
  std::string Name(void) const override;
};

// Checks whether or not some instruction bits (second operand) equals some
// proposed values.
class DecodeCondition final : public Condition {
 public:
  CONDITION_CONSTRUCTOR(DecodeCondition)
  virtual ~DecodeCondition(void);
  std::string Name(void) const override;
};

// A hint value is an input to the circuit. It can serve several purposes,
// e.g.
class Hint final : public Operation {
 public:
  virtual ~Hint(void);
  std::string Name(void) const override;

  inline explicit Hint(unsigned size_)
      : Operation(Operation::kHint, size_),
        weak_conditions(this, true) {}

  inline explicit Hint(unsigned size_, Operation *eq_class_)
      : Operation(Operation::kHint, size_, eq_class_),
        weak_conditions(this, true) {}

  // Weak list of conditions that compare this hint value against what it is
  // hinting.
  UseList<Operation> weak_conditions;
};

// An equivalence class on tree instructions. This is itself its own.
class EquivalenceClass final : public Operation {
 public:
  FORWARD_CONSTRUCTOR(Operation, EquivalenceClass)
  virtual ~EquivalenceClass(void);
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;
};

class VerifyInstruction final : public Condition {
 public:
  CONDITION_CONSTRUCTOR(VerifyInstruction)
  virtual ~VerifyInstruction(void);
  std::string Name(void) const override;
};

class OnlyOneCondition final : public Condition {
 public:
  CONDITION_CONSTRUCTOR(OnlyOneCondition)
  virtual ~OnlyOneCondition(void);
  std::string Name(void) const override;
};

#undef FORWARD_CONSTRUCTOR
#undef CONDITION_CONSTRUCTOR

// Represents the results of verifying a circuit.
class Circuit : public Condition {
 public:
  virtual ~Circuit(void);
  std::string Name(void) const override;

  static std::unique_ptr<Circuit>
  CreateFromInstructions(const std::string &arch_name,
                         const std::string &os_name,
                         const std::string &file_name);

  void Serialize(std::ostream &os);

  static std::unique_ptr<Circuit> Deserialize(std::istream &is);

  // clang-format off

#define FOR_EACH_OPERATION(cb) \
  cb(Constant, constants) \
  cb(LLVMOperation, llvm_insts) \
  cb(Concat, concats) \
  cb(CountLeadingZeroes, clzs) \
  cb(CountTrailingZeroes, ctzs) \
  cb(Extract, extracts) \
  cb(PopulationCount, popcounts) \
  cb(Parity, parities) \
  cb(InputRegister, input_regs) \
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
  cb(EquivalenceClass, eq_classes)

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
