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
class Operation : public Node<Operation> {
 public:
  virtual ~Operation(void) = default;

  virtual std::string Name(void) const;
  virtual Operation *CloneWithoutOperands(Circuit *) const = 0;

  virtual bool Equals(const Operation *that) const;

  enum : unsigned {

    // Some sequence of bits.
    kConstant = 0,

    // An undefined value, either an `llvm::Undef` or a `__remill_undefined_*`
    // value.
    kUndefined = 1,

    // Every LLVM instruction kind. Things like Add, Sub, etc.
    kLLVMOperation = 2,

    // Flip all bits.
    kNot = 3,

    // Extract some selection of bits [low_inc, high_exc) from an operand.
    //
    // NOTE(pag): `low_inc` is the lowest order bit, `high_exc` is the highest
    //             order bit.
    kExtract = 4,

    // Concatenate an N and an M bit value, producing an (N+M)-bit value.
    kConcat = 5,

    // Compute the population count of some bit string.
    kPopulationCount = 6,

    // Compute the parity of some bit string. This produces a single bit result.
    kParity = 7,

    // Count the leading zeroes of a bit string.
    kCountLeadingZeroes = 8,

    // Count the trailing zeros of a bit string.
    kCountTrailingZeroes = 9,

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
    kReadMemoryCondition = 10,

    // Named register that is an input to the circuit, representing the value
    // of the register before the instruction "executes".
    kInputRegister = 11,

    kInputImmediate = 12,

    // Named register that is a hint input to teh circuit, representing the
    // value of the register after the instruction "executes". We need to verify
    // that the computed value of the register after the instruction matches
    // this value, which is what `kRegisterCondition` is for.
    kOutputRegister = 13,

    // Input to the circuit representing the bits of the instruction.
    kInputInstructionBits = 14,

    // A condition generated as part of optimizations, where a hint is generated.
    kHintCondition = 15,

    // Checks that a computed register's value matches some output value, e.g.:
    //
    //      new_EAX == OutputRegister(EAX)
    //
    // Produces a single bit as output.
    kRegisterCondition = 16,

    // Checks that a register's value is preserved, e.g.:
    //
    //      InputRegister(EAX) == OutputRegister(EAX)
    //
    // Produces a single bit as output.
    kPreservedCondition = 17,

    // Checks that a register's value is actually a copy of another register,
    // e.g.:
    //
    //      InputRegister(EBX) == OutputRegister(EAX)
    //
    // Produces a single bit as output.
    kCopyCondition = 18,

    // Condition that checks if a proposed encoding matches some input
    // instruction bits.
    //
    // Produces a single bit as output.
    kDecodeCondition = 19,

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
    kVerifyInstruction = 20,

    // An input hint value of some size.
    kHint = 21,

    // A XOR of N bits, i.e. `b1 ^ ... ^ bN`. We construct the circuit such
    // that we know that only one of the bits will ever be `1`, and all others
    // will be zero.
    kOnlyOneCondition = 22,

    // Output value (0 or 1) of the circuit. This is really just a specialized
    // equivalence class, where all the inputs are equivalent ways of expressing
    // the circuit, of which the minimum cost one is chosen.
    kCircuit = 23,

    // Invalid -- you cannot have this tag present in you tree.
    kInvalid = 0xff
  };

  uint64_t id() const { return _id; }

  // The "opcode" of this.
  const unsigned op_code{0};
  uint64_t _id = 0;

  // Size in bits of this instruction's "result" value. For example, a zero-
  // extension will represent the size of the output value.
  const unsigned size{0};

  template <typename Vis>
  void Traverse(Vis &vis) {
    for (auto op : operands) {
      vis.Visit(op);
    }
  }

  static constexpr inline uint32_t kind = kInvalid;

 protected:
  explicit Operation(unsigned op_code_, unsigned size_);
};

template<typename Kind>
static inline std::string to_string(Kind kind) {
  switch (kind) {
    case Operation::kConstant : return "CONSTANT";
    case Operation::kUndefined : return "UNDEFINED";
    case Operation::kLLVMOperation : return "LLVMOPERATION";
    case Operation::kNot : return "NOT";
    case Operation::kExtract : return "EXTRACT";
    case Operation::kConcat : return "CONCAT";
    case Operation::kPopulationCount : return "POPULATION_COUNT";
    case Operation::kParity : return "PARITY";
    case Operation::kCountLeadingZeroes : return "COUNT_LEADING_ZEROES";
    case Operation::kCountTrailingZeroes : return "COUNT_TRAILING_ZEROES";
    case Operation::kReadMemoryCondition: return "READ_MEMORY_CONDITION";
    case Operation::kInputRegister: return "INPUT_REGISTER";
    case Operation::kInputImmediate : return "INPUT_IMMEDIATE";
    case Operation::kOutputRegister : return "OUTPUT_REGISTER";
    case Operation::kInputInstructionBits : return "INSTRUNCTION_BITS";
    case Operation::kHintCondition : return "HINT_CONDITION";
    case Operation::kRegisterCondition : return "OUTPUT_REGISTER_CHECK";
    case Operation::kPreservedCondition : return "PRESERVED_REGISTER_CHECK";
    case Operation::kCopyCondition : return "COPIED_REGISTER_CHECK";
    case Operation::kDecodeCondition : return "INSTRUCTION_BITS_CHECK";
    case Operation::kVerifyInstruction : return "ALL_OF";
    case Operation::kHint : return "HINT";
    case Operation::kOnlyOneCondition : return "ONE_OF";
    case Operation::kCircuit : return "RESULT";
    case Operation::kInvalid : return "INVALID";
    default : LOG(FATAL) << "Unknown kind " << kind; return "";
  }
}

template<typename Kind>
static inline Kind from_string(const std::string &name) {
  if (name == "CONSTANT") return Operation::kConstant;
  if (name == "UNDEFINED") return Operation::kUndefined;
  if (name == "LLVMOPERATION") return Operation::kLLVMOperation;
  if (name == "NOT") return Operation::kNot;
  if (name == "EXTRACT") return Operation::kExtract;
  if (name == "CONCAT") return Operation::kConcat;
  if (name == "POPULATION_COUNT") return Operation::kPopulationCount;
  if (name == "PARITY") return Operation::kParity;
  if (name == "COUNT_LEADING_ZEROES") return Operation::kCountLeadingZeroes;
  if (name == "COUNT_TRAILING_ZEROES") return Operation::kCountTrailingZeroes;
  if (name == "READ_MEMORY_CONDITION") return Operation::kReadMemoryCondition;
  if (name == "INPUT_REFGISTER") return Operation::kInputRegister;
  if (name == "OUTPOUT_REGISTER") return Operation::kOutputRegister;
  if (name == "INSTRUNCTION_BITS") return Operation::kInputInstructionBits;
  if (name == "HINT_CONDITION") return Operation::kHintCondition;
  if (name == "OUTPUT_REGISTER_CHECK") return Operation::kRegisterCondition;
  if (name == "PRESERVED_REGISTER_CHECK") return Operation::kPreservedCondition;
  if (name == "COPIED_REGISTER_CHECK") return Operation::kCopyCondition;
  if (name == "INSTRUCTION_BITS_CHECK") return Operation::kDecodeCondition;
  if (name == "ALL_OF") return Operation::kVerifyInstruction;
  if (name == "HINT") return Operation::kHint;
  if (name == "ONE_OF") return Operation::kOnlyOneCondition;
  if (name == "RESULT") return Operation::kCircuit;
  if (name == "INVALID") return Operation::kInvalid;
  LOG(FATAL) << "Unknown name " << name;
  return "";
}

#define FORWARD_CONSTRUCTOR(base_class, derived_class) \
  inline explicit derived_class(unsigned size_) \
      : base_class(derived_class::kind, size_) {}


#define CONDITION_CONSTRUCTOR(derived_class) \
  inline explicit derived_class(void) \
      : Condition(derived_class::kind) {}

// Some bitwise operation; must be extended.
class BitOperation : public Operation {
 public:
  static constexpr inline uint32_t kind = kInvalid;

  std::string Name(void) const override;

 protected:
  using Operation::Operation;
};

template<typename T>
struct ValueLeaf : public Operation {
  static constexpr inline uint32_t kind = kInvalid;

  std::string Name(void) const override;
  using Operation::Operation;

  T value;
};

// An input register.
class InputRegister : public Operation {
 public:
  static constexpr inline uint32_t kind = kInputRegister;

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
  static constexpr inline uint32_t kind = kInputImmediate;

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  explicit InputImmediate(unsigned size_)
      : Operation(Operation::kInputImmediate, size_) {}
};

// An output register.
class OutputRegister : public Operation {
 public:
  static constexpr inline uint32_t kind = kOutputRegister;

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  inline explicit OutputRegister(unsigned size_, std::string reg_name_)
      : Operation(Operation::kOutputRegister, size_),
        reg_name(std::move(reg_name_)) {}

  const std::string reg_name;
};


// Mirrors an instruction from LLVM. `op_code` is `inst->getOpcode()`.
class LLVMOperation final : public Operation {
 public:
  static constexpr inline uint32_t kind = kLLVMOperation;
  explicit LLVMOperation(unsigned llvm_opcode_, unsigned llvm_predicate_,
                         unsigned size_);

  explicit LLVMOperation(llvm::Instruction *inst_);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  const uint32_t llvm_op_code;
  const uint32_t llvm_predicate;

  static const uint32_t kInvalidLLVMPredicate;
};

// An undefined value.
class Undefined final : public Operation {
 public:
  static constexpr inline uint32_t kind = kUndefined;
  inline explicit Undefined(unsigned size_)
      : Operation(Operation::kUndefined, size_) {}

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;

 protected:
  using Operation::Operation;
};

class Constant final : public Operation {
 public:
  static constexpr inline uint32_t kind = kConstant;

  inline explicit Constant(std::string bits_, unsigned size_)
      : Operation(Operation::kConstant, size_),
        bits(bits_) {}


  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  // Value of this constant. The least significant bit is stored in `bits[0]`,
  // and the most significant bit is stored in `bits[size - 1u]`.
  const std::string bits;
};

// Flip all bits in a bitvector.
class Not final : public BitOperation {
 public:
  static constexpr inline uint32_t kind = kNot;
  FORWARD_CONSTRUCTOR(BitOperation, Not)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
};

// Extract bits.
class Extract final : public BitOperation {
 public:
  static constexpr inline uint32_t kind = kExtract;

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
  bool Equals(const Operation *that) const override;

  inline explicit Extract(unsigned low_bit_inc_, unsigned high_bit_exc_)
      : BitOperation(Operation::kExtract, high_bit_exc_ - low_bit_inc_),
        low_bit_inc(low_bit_inc_),
        high_bit_exc(high_bit_exc_) {}

  const uint32_t low_bit_inc;
  const uint32_t high_bit_exc;
};

// Concatenate two bitvectors.
class Concat final : public BitOperation {
 public:
  static constexpr inline uint32_t kind = kConcat;
  FORWARD_CONSTRUCTOR(BitOperation, Concat)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
};

// Population count of some bits. Needed for things like parity count
// calculation.
class PopulationCount final : public BitOperation {
 public:
  static constexpr inline uint32_t kind = kPopulationCount;
  FORWARD_CONSTRUCTOR(BitOperation, PopulationCount)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
};

// Count the leading zeroes of some bits.
class CountLeadingZeroes final : public BitOperation {
  static constexpr inline uint32_t kind = kCountLeadingZeroes;
 public:
  FORWARD_CONSTRUCTOR(BitOperation, CountLeadingZeroes)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
};

class CountTrailingZeroes final : public BitOperation {
 public:
  static constexpr inline uint32_t kind = kCountTrailingZeroes;
  FORWARD_CONSTRUCTOR(BitOperation, CountTrailingZeroes)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
};

class Condition : public Operation {
 public:

 protected:
  inline explicit Condition(unsigned op_code_) : Operation(op_code_, 1u) {}
};

// Returns `0` if there are an even numbers of bits in a bit string, and `1`
// if there are an odd number of bits in a bit string.
// TODO(lukas): Should this not inherit from BitOperation?
class Parity final : public Condition {
 public:
  static constexpr inline uint32_t kind = kParity;
  CONDITION_CONSTRUCTOR(Parity)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// Condition that verifies that some computed address matches a hint address.
class ReadMemoryCondition final : public Condition {
 public:
  static constexpr inline uint32_t kind = kReadMemoryCondition;
  enum : unsigned {
    kDynamicAddress = 0u,
    kHintedAddress = 1u,
    kHintedValue = 2u
  };
  CONDITION_CONSTRUCTOR(ReadMemoryCondition)
  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// A comparison between the proposed output value of a register, and the
// output register itself.
class RegisterCondition final : public Condition {
 public:
  static constexpr inline uint32_t kind = kRegisterCondition;
  enum : unsigned { kDynamicRegisterValue = 0u, kOutputRegister = 1u };

  CONDITION_CONSTRUCTOR(RegisterCondition)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// A comparison between the proposed output value of a register, and the
// output register itself.
class HintCondition final : public Condition {
 public:
  static constexpr inline uint32_t kind = kHintCondition;
  enum : unsigned { kDynamicValue = 0u, kHint = 1u };

  CONDITION_CONSTRUCTOR(HintCondition)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// Says that we are preserving the value of a register.
class PreservedCondition final : public Condition {
 public:
  static constexpr inline uint32_t kind = kPreservedCondition;
  enum : unsigned { kInputRegister = 0u, kOutputRegister = 1u };

  CONDITION_CONSTRUCTOR(PreservedCondition)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// Says that we are moving one register to a different register.
class CopyCondition final : public Condition {
 public:
  static constexpr inline uint32_t kind = kCopyCondition;
  enum : unsigned { kOtherInputRegister = 0u, kOutputRegister = 1u };

  CONDITION_CONSTRUCTOR(CopyCondition)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// Input bits that
class InputInstructionBits : public Operation {
 public:
  static constexpr inline uint32_t kind = kInputInstructionBits;
  FORWARD_CONSTRUCTOR(Operation, InputInstructionBits)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// Checks whether or not some instruction bits (second operand) equals some
// proposed values.
class DecodeCondition final : public Condition {
 public:
  static constexpr inline uint32_t kind = kDecodeCondition;
  CONDITION_CONSTRUCTOR(DecodeCondition)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

// A hint value is an input to the circuit. It can serve several purposes,
// e.g.
class Hint final : public Operation {
 public:
  static constexpr inline uint32_t kind = kHint;

  inline explicit Hint(unsigned size_)
      : Operation(Operation::kHint, size_) {}

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;

  // Weak list of conditions that compare this hint value against what it is
  // hinting.
  // TODO(lukas): ???
  //UseList<Operation> weak_conditions;
};

class VerifyInstruction final : public Condition {
 public:
  static constexpr inline uint32_t kind = kVerifyInstruction;
  CONDITION_CONSTRUCTOR(VerifyInstruction)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

class OnlyOneCondition final : public Condition {
 public:
  static constexpr inline uint32_t kind = kOnlyOneCondition;
  CONDITION_CONSTRUCTOR(OnlyOneCondition)

  Operation *CloneWithoutOperands(Circuit *circuit) const override;
  std::string Name(void) const override;
};

#undef FORWARD_CONSTRUCTOR
#undef CONDITION_CONSTRUCTOR

template<typename OP>
struct MaterializedDefList {
  DefList<OP> data;

  std::size_t RemoveUnused() {
    auto notify_operands = [](auto &&x) {
      for (auto op : x->operands) {
        op->RemoveUser(x.get());
      }
    };
    return data.RemoveUnused(notify_operands);
  }

  template<typename CB>
  void ForEachOperation(CB &&cb) {
    for (auto op : this->data) {
      cb(op);
    }
  }

  template<typename CB>
  void Apply(CB &&cb) {
    cb(data);
  }

  auto &Attr() { return data; }
};


template<typename ... Ops>
struct Attributes : MaterializedDefList<Ops> ... {

  template<typename T>
  using parent = MaterializedDefList<T>;

  template<typename T>
  auto &Attr() {
    return this->parent<T>::Attr();
  }

  template<typename CB>
  void ForEachOperation(CB cb) {
    (parent<Ops>::ForEachOperation(cb), ...);
  }

  void ClearWithoutErasure() {
    auto clear = [](auto &field) {
      for (auto op : field) {
        op->operands.ClearWithoutErasure();
      }
    };
    (parent<Ops>::Apply(clear), ...);
  }

  template<typename CB>
  void ForEachField(CB cb) {
    (parent<Ops>::Apply(cb), ...);
  }

  std::size_t RemoveUnused() {
    return (parent<Ops>::RemoveUnused() + ...);
  }
};

using AllAttributes =
  Attributes<Constant, Undefined, LLVMOperation, Not, Concat,
             CountLeadingZeroes, CountTrailingZeroes, Extract, PopulationCount,
             Parity, InputRegister, InputImmediate,  OutputRegister, InputInstructionBits,
             RegisterCondition, PreservedCondition, CopyCondition, DecodeCondition,
             VerifyInstruction, ReadMemoryCondition, OnlyOneCondition,
             Hint, HintCondition>;

// TODO(lukas): Get rid of this
#define FOR_EACH_OPERATION(what) \
  what(Constant) \
  what(Undefined) \
  what(LLVMOperation) \
  what(Not) \
  what(Concat) \
  what(CountLeadingZeroes) \
  what(CountTrailingZeroes) \
  what(Extract) \
  what(PopulationCount) \
  what(Parity) \
  what(InputRegister) \
  what(InputImmediate) \
  what(OutputRegister) \
  what(InputInstructionBits) \
  what(RegisterCondition) \
  what(PreservedCondition) \
  what(CopyCondition) \
  what(DecodeCondition) \
  what(VerifyInstruction) \
  what(ReadMemoryCondition) \
  what(OnlyOneCondition) \
  what(Hint) \
  what(HintCondition)


// Represents the results of verifying a circuit.
class Circuit : public Condition, AllAttributes {
 public:
  virtual ~Circuit(void);

  Operation *CloneWithoutOperands(Circuit *circuit) const override;

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

  void RemoveUnused(void) {
    while (this->AllAttributes::RemoveUnused()) {}
  }

  template<typename T>
  std::size_t RemoveUnused(void) {
    return this->AllAttributes::parent<T>::RemoveUnused();
  }
  // clang-format off

  using AllAttributes::ForEachOperation;

  uint64_t ids = 0;
  static constexpr inline uint64_t max_id = (1ull >> 60);

  template<typename T> auto &Attr() {
    static_assert(std::is_base_of_v<Operation, T>);
    return this->AllAttributes::Attr<T>();
  }

  template<typename T, typename ...Args>
  T* Create(Args &&...args) {
    auto op = Attr<T>().Create(std::forward<Args>(args)...);
    op->_id = ++ids;
    return op;
  }

  template<typename T, typename ...Args>
  T* Adopt(uint64_t id, Args &&...args) {
    auto op = Attr<T>().Create(std::forward<Args>(args)...);
    op->_id = id;
    ids = std::max(ids, id);
    return op;
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
#define VISITOR_CASE(type) \
  case Operation::k##type: \
    if (auto typed_op = dynamic_cast<type *>(op); typed_op) { \
      self->Visit##type(typed_op); \
    } else { \
      LOG(FATAL) << "Node with op_code=" << op_code << " (k" << #type \
                 << ") has incorrect dynamic type: " << typeid(*op).name(); \
    } \
    break;

      FOR_EACH_OPERATION(VISITOR_CASE)
      VISITOR_CASE(Circuit)
#undef VISITOR_CASE
      default: LOG(FATAL) << "Unhandled operation: " << op->Name();
    }
  }

#define DECLARE_VISITOR(type) \
  void Visit##type(type *op) { \
    auto self = static_cast<Derived *>(this); \
    self->VisitOperation(op); \
  }

  FOR_EACH_OPERATION(DECLARE_VISITOR)
  DECLARE_VISITOR(Circuit)
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
