/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Util/UseDef.h>
#include <circuitous/Util/TypeList.hpp>

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

class Circuit;

// A general instruction.
class Operation : public Node<Operation> {
 public:
  static constexpr uint32_t kind = 0;

  virtual ~Operation() = default;

  virtual std::string Name() const;
  virtual bool Equals(const Operation *that) const;

  auto &operator[](std::size_t idx) { return operands[idx]; }
  const auto &operator[](std::size_t idx) const { return operands[idx]; }

  uint64_t id() const { return _id; }


  // Size in bits of this instruction's "result" value. For example, a zero-
  // extension will represent the size of the output value.
  const unsigned size{0};

  // The "opcode" of this.
  const unsigned op_code{0};

  uint64_t _id = 0;


  template <typename Vis>
  void Traverse(Vis &vis) {
    for (auto op : operands) {
      vis.Dispatch(op);
    }
  }

  explicit Operation(unsigned size_, unsigned op_code_) : size(size_), op_code(op_code_) {}
};

static constexpr uint32_t build_kind(uint8_t type, uint16_t op_code, uint8_t meta) {
  return static_cast<uint32_t>((type << 24) + (op_code << 8) + meta);
}

static constexpr uint32_t add_type(uint32_t val, uint8_t type) {
  auto type_ = static_cast< uint32_t >(type);
  return static_cast<uint32_t>((type_ << 24) + val);
}

static constexpr uint32_t add_tag(uint32_t val, uint16_t tag) {
  auto tag_ = static_cast< uint32_t >(tag);
  return static_cast<uint32_t>((tag_ << 8) + val);
}

static constexpr uint32_t add_meta(uint32_t val, uint8_t meta) {
  return static_cast<uint32_t>(meta + val);
}

template< uint8_t type_ >
struct with_type { static constexpr uint8_t type = type_; };

using LeafValue = with_type< 0x1 >;
using HiddenValue = with_type< 0x2 >;
using BitManip = with_type< 0x3 >;
using BitOp = with_type< 0x4 >;

using Computational = with_type< 0x5 >;

using Constraint = with_type< 0x6 >;
using Uncat = with_type< 0x7 >;
using BoolOp = with_type< 0x9 >;

template< uint16_t tag_ >
struct Tag {
  static constexpr uint16_t tag = tag_;
};

template< typename Type, typename Tag >
struct with_type_tag : Type, Tag {

  static constexpr uint32_t compute_kind(uint32_t base) {
    return add_type(add_tag(base, Tag::tag), Type::type);
  }
};


template< typename T, uint8_t meta >
struct IO : T {
  static constexpr uint32_t kind = add_meta(T::kind, meta);

  template< typename ... Args >
  IO(Args &&...args) : T(std::forward<Args>(args)..., kind) {}
};


template< typename T >
struct Input : IO< T, 0 >  {
  using IO< T, 0 >::IO;
  std::string Name() const override { return "In." + T::Name(); }
};

template< typename T >
struct Output : IO< T, 1 > {
  using IO< T, 1 >::IO;
  std::string Name() const override { return "Out." + T::Name(); }
};

/* I/O & Leaf nodes */

struct Register : Operation, with_type_tag< LeafValue, Tag< 3 > >{
  static constexpr uint32_t kind = compute_kind(Operation::kind);

  template< typename ...As >
  Register(const std::string &rn_, As &&... as)
      : Operation(std::forward<As>(as)...), reg_name(rn_)
  {}

  std::string Name() const override { return "register." + reg_name; }
  bool Equals(const Operation *other) const override { LOG(FATAL) << "TODO"; }

  std::string reg_name;
};

using InputRegister = Input< Register >;
using OutputRegister = Output< Register >;

struct ErrorFlag : Operation, with_type_tag< LeafValue, Tag< 4 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);

  template< typename ...Args >
  ErrorFlag(Args &&... args) : Operation(1u, std::forward<Args>(args)...) {}

  std::string Name() const override { return "error_flag"; }
};

using InputErrorFlag = Input< ErrorFlag >;
using OutputErrorFlag = Output< ErrorFlag >;

// An undefined value.
struct Undefined final : Operation, with_type_tag< LeafValue, Tag< 2 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);

  explicit Undefined(unsigned size_) : Operation(size_, kind) {}
  std::string Name() const override { return "undefined"; }
};


struct Constant final : Operation, with_type_tag< LeafValue, Tag< 1 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);

  explicit Constant(std::string bits_, unsigned size_)
      : Operation(size_, kind),
        bits(std::move(bits_))
  {}

  bool Equals(const Operation *that) const override;
  std::string Name() const override;

  // Value of this constant. The least significant bit is stored in `bits[0]`,
  // and the most significant bit is stored in `bits[size - 1u]`.
  const std::string bits;
};

struct Hint final : Operation, with_type_tag< LeafValue, Tag< 5 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);

  inline explicit Hint(unsigned size_) : Operation(size_, kind) {}

  std::string Name() const override { return "hint"; }
};

// Input bits that
struct InputInstructionBits : Operation, with_type_tag< LeafValue, Tag< 6 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);
  explicit InputInstructionBits(unsigned size_) : Operation(size_, kind) {}

  std::string Name() const override { return "instruction_bits"; }
};

/* LLVMOP */


// Mirrors an instruction from LLVM. `op_code` is `inst->getOpcode()`.
struct LLVMOperation final : Operation, with_type_tag< Computational, Tag< 1 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);
  explicit LLVMOperation(unsigned llvm_opcode_, unsigned llvm_predicate_,
                         unsigned size_);

  explicit LLVMOperation(llvm::Instruction *inst_);

  std::string Name() const override;
  bool Equals(const Operation *that) const override;

  const uint32_t llvm_op_code;
  const uint32_t llvm_predicate;

  static const uint32_t kInvalidLLVMPredicate;
};


#define declare_llvm_op(cls, idx) \
struct cls final : Operation, with_type_tag< Computational, Tag< idx > > { \
  static constexpr uint32_t kind = compute_kind(Operation::kind); \
  cls(unsigned size_) : Operation(size_, kind) {} \
  std::string Name() const override { return #cls; } \
};

declare_llvm_op(Add, 0)
declare_llvm_op(Sub, 1)
declare_llvm_op(Mul, 2)

declare_llvm_op(UDiv, 3)
declare_llvm_op(SDiv, 4)

declare_llvm_op(Shl, 5)
declare_llvm_op(LShr, 6)
declare_llvm_op(AShr, 7)

declare_llvm_op(Trunc, 8)
declare_llvm_op(ZExt, 9)
declare_llvm_op(SExt, 10)

declare_llvm_op(Icmp_ult, 11)
declare_llvm_op(Icmp_slt, 12)
declare_llvm_op(Icmp_ugt, 13)
declare_llvm_op(Icmp_eq, 14)
declare_llvm_op(Icmp_ne, 15)

#undef declare_llvm_op

using llvm_ops_t = TL<
    Add, Sub, Mul, UDiv, SDiv, Shl, LShr, AShr, Trunc, ZExt, SExt,
    Icmp_ult, Icmp_slt, Icmp_ugt, Icmp_eq, Icmp_ne
>;


/* Hidden */

struct InputImmediate : HiddenValue, Operation {
  static constexpr uint16_t tag = 0x1;
  static constexpr uint32_t kind = add_type(add_tag(Operation::kind, tag), HiddenValue::type);

  std::string Name() const override { return "input_immediate"; }
  bool Equals(const Operation *that) const override;

  explicit InputImmediate(unsigned size_) : Operation(size_, kind) {}
};

/* BitManips */

struct Extract final : Operation, with_type_tag< BitManip, Tag< 0 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);

  std::string Name() const override {
    std::stringstream ss;
    ss << "extract." << high_bit_exc << "." << low_bit_inc;
    return ss.str();
  }

  bool Equals(const Operation *that) const override;

  inline explicit Extract(uint32_t low_bit_inc_, uint32_t high_bit_exc_)
      : Operation(high_bit_exc_ - low_bit_inc_, kind),
        low_bit_inc(low_bit_inc_),
        high_bit_exc(high_bit_exc_) {}

  const uint32_t low_bit_inc;
  const uint32_t high_bit_exc;
};


struct Concat final : Operation, with_type_tag< BitManip, Tag< 1 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);
  Concat(uint32_t size_) : Operation(size_, kind) {}
  std::string Name() const override { return "concat"; }
};

/* BitOps */

struct PopulationCount final : Operation, with_type_tag< BitOp, Tag< 0 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);
  explicit PopulationCount(unsigned size_) : Operation(size_, kind) {}
  std::string Name() const override { return "pop_count"; }
};

struct CountLeadingZeroes final : Operation, with_type_tag< BitOp, Tag< 1 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);
  explicit CountLeadingZeroes(unsigned size_) : Operation(size_, kind) {}
  std::string Name() const override { return "count_lead_zeroes"; }
};

struct CountTrailingZeroes final : Operation, with_type_tag< BitOp, Tag< 2 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);
  explicit CountTrailingZeroes(unsigned size_) : Operation(size_, kind) {}
  std::string Name() const override { return "count_trailing_zeroes"; }
};

struct Not final : Operation, with_type_tag< BitOp, Tag< 3 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);
  explicit Not(unsigned size_) : Operation(size_, kind) {}
  std::string Name() const override { return "not"; }
};

struct Parity final : Operation, with_type_tag< BitOp, Tag< 4 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);
  explicit Parity() : Operation(1, kind) {}

  std::string Name() const override { return "parity"; }
};

/* Without category */

struct Select : Operation, with_type_tag< Uncat, Tag< 0 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);

  explicit Select(uint32_t bits_, uint32_t size_)
      : Operation(size_, kind), bits(bits_)
  {}
  std::string Name() const override {
    std::stringstream ss;
    ss << "select." << bits;
    return ss.str();
  }

  // Return one of the `2 ^ bits` values. It is also expected that this node
  // has `2 ^ bits + 1` operands.
  uint32_t bits = 0;
};

/* Constaints */

struct EnforceCtx : Operation {
  using Operation::Operation;
  enum : uint8_t { kDynamic = 0u, kFixed = 1u };

  Operation *dynamic() { return operands[kDynamic]; }
  Operation *fixed() { return operands[kFixed]; }

  const Operation *dynamic() const { return operands[kDynamic]; }
  const Operation *fixed() const { return operands[kFixed]; }

  std::string suffix_() const {
    if (operands.size() != 2) {
      return "invalid.0";
    }
    return fixed()->Name() + "." + std::to_string(fixed()->size);
  }
};

// A comparison between the proposed output value of a register, and the
// output register itself.
struct RegisterCondition final : EnforceCtx, with_type_tag< Constraint, Tag< 0 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);
  RegisterCondition() : EnforceCtx(1u, kind) {}
  std::string Name() const override { return "register_condition." + EnforceCtx::suffix_(); }
};

// A comparison between the proposed output value of a register, and the
// output register itself.
struct HintCondition final : EnforceCtx, with_type_tag< Constraint, Tag< 1 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);
  HintCondition() : EnforceCtx(1u, kind) {}
  std::string Name() const override { return "hint_condition." + EnforceCtx::suffix_(); }
};

// Says that we are preserving the value of a register.
struct PreservedCondition final : EnforceCtx, with_type_tag< Constraint, Tag< 2 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);
  PreservedCondition() : EnforceCtx(1u, kind) {}
  std::string Name() const override { return "preserved_condition." + EnforceCtx::suffix_(); }
};

// Says that we are moving one register to a different register.
struct CopyCondition final : EnforceCtx, with_type_tag< Constraint, Tag< 3 > > {
  static constexpr uint32_t kind = compute_kind(Operation::kind);
  CopyCondition() : EnforceCtx(1u, kind) {}
  std::string Name() const override { return "copy_condition." + EnforceCtx::suffix_(); }
};

#define make_bool_op(cls, idx) \
struct cls final : Operation, with_type_tag< BoolOp, Tag< idx > > { \
  static constexpr uint32_t kind = compute_kind(Operation::kind); \
  cls() : Operation(1u, kind) {} \
  std::string Name() const override { return "#cls"; } \
};

make_bool_op(DecodeCondition, 0)
make_bool_op(VerifyInstruction, 1)
make_bool_op(Or, 2)
make_bool_op(OnlyOneCondition, 3)

#undef make_bool_op

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

  template<typename CB>
  auto match(Operation *op, CB cb) {
    if (op->op_code == OP::kind) {
      cb(dynamic_cast<OP *>(op));
    }
  }

  template<typename CB>
  auto match_d(uint32_t kind, CB cb) {
    if (kind == OP::kind) {
      cb(static_cast<OP *>(nullptr));
    }
  }
};

using generic_list_t =
TL<
  Constant, Undefined, LLVMOperation, Not, Concat,
  CountLeadingZeroes, CountTrailingZeroes, Extract, PopulationCount,
  Parity, InputRegister, InputImmediate,  OutputRegister, InputInstructionBits,
  RegisterCondition, PreservedCondition, CopyCondition, DecodeCondition,
  VerifyInstruction, OnlyOneCondition,
  Hint, HintCondition, Select,
  InputErrorFlag, OutputErrorFlag, Or
>;

using node_list_t = merge< generic_list_t, llvm_ops_t >;

template< typename T >
struct to_mat_def_list {
  using type = MaterializedDefList< T >;
};

using m_def_lists = apply< node_list_t, to_mat_def_list >;

template< typename L > struct Attributes {};

template< typename ... Ops >
struct Attributes< TL< Ops ... > > : Ops ... {

  template<typename T>
  using parent = MaterializedDefList< T >;

  template<typename T>
  auto &Attr() {
    return this->parent<T>::Attr();
  }

  template<typename CB>
  void ForEachOperation(CB cb) {
    (this->Ops::ForEachOperation(cb), ...);
  }

  void ClearWithoutErasure() {
    auto clear = [](auto &field) {
      for (auto op : field) {
        op->operands.ClearWithoutErasure();
      }
    };
    (this->Ops::Apply(clear), ...);
  }

  template<typename CB>
  void ForEachField(CB cb) {
    (this->Ops::Apply(cb), ...);
  }

  std::size_t RemoveUnused() {
    return (this->Ops::RemoveUnused() + ...);
  }

  template<typename CB> auto match(Operation *op, CB cb) { (this->Ops::match(op, cb), ...); }
  template<typename CB> auto match_d(uint32_t k, CB cb) { (this->Ops::match_d(k, cb), ...); }
};
using AllAttributes = Attributes< m_def_lists >;


// Represents the results of verifying a circuit.
class Circuit : public Operation, public AllAttributes {
 public:
  static constexpr inline uint32_t kind = 0x1;

  virtual ~Circuit(void) = default;

  using circuit_ptr_t = std::unique_ptr<Circuit>;


  static circuit_ptr_t make_circuit(const std::string &arch_name,
                                    const std::string &os_name,
                                    const std::string &file_name,
                                    const Optimizations &opts={});

  static circuit_ptr_t make_circuit(const std::string &arch_name,
                                    const std::string &os_name,
                                    const llvm::StringRef &bytes,
                                    const Optimizations &opts={});

  static circuit_ptr_t make_circuit(const std::string &arch_name,
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

  template<typename ...Args>
  Operation *Adopt(uint64_t id, uint32_t kind, Args &&...args) {
    Operation *out;
    auto adopt = [&](auto op) {
      using raw_t = std::decay_t< decltype( op ) >;
      out = this->Create< raw_t >( std::forward< Args >( args )... );
    };
    return out;
  }

  template<typename T>
  T *Fork(T *original) {
    T copy{*original};
    copy._id = ++ids;
    copy.operands.clear();
    return Attr<T>().Adopt(std::move(copy));
  }

  Operation *Fork(Operation *op) {
    Operation *out;
    auto fork = [&](auto op) {
      out = this->Fork(op);
    };

    this->match(op, fork);
    return out;
  }

  template<typename What>
  auto fetch_singular() {
    auto &all = this->Attr<What>();
    CHECK_EQ(all.Size(), 1);
    return all[0];
  }

  template<typename What, bool allow_failure = true>
  auto fetch_reg(const std::string &name) -> What * {
    for (auto reg : this->Attr<What>()) {
      if (reg->reg_name == name) {
        return reg;
      }
    }
    if constexpr (!allow_failure) {
      LOG(FATAL) << "Register " << name << " not present";
    }
    return nullptr;
  }

  InputInstructionBits *input_inst_bits() { return fetch_singular<InputInstructionBits>(); }
  InputErrorFlag *input_ebit() { return fetch_singular<InputErrorFlag>(); }
  OutputErrorFlag *output_ebit() { return fetch_singular<OutputErrorFlag>(); }

  using cstr_ref = const std::string &;
  InputRegister *input_reg(cstr_ref name) { return fetch_reg<InputRegister>(name); }
  OutputRegister *output_reg(cstr_ref name) { return fetch_reg<OutputRegister>(name); }

 public:
  std::string Name() const override { return "circuit"; }
  Circuit(void);
};


template< typename D, typename L > struct Visitor_ {};

template<typename Derived, typename ... Ops >
struct Visitor_< Derived, TL< Ops ... > > {
  void Visit(Operation *op) {
    op->Traverse(*this);
  }

  Derived &self() { return static_cast<Derived &>(*this); }

  template<typename T, typename ...Tail, typename ... Args>
  auto Visit_(Operation *op, Args &&...args) {
    if (op->op_code == T::kind) {
      return self().Visit(dynamic_cast<T *>(op), std::forward<Args>(args)...);
    }
    if constexpr (sizeof...(Tail) != 0) {
      return this->Visit_<Tail ...>(op, std::forward<Args>(args)...);
    } else {
      return self().Visit(op, std::forward<Args>(args)...);
    }
  }

  template<typename ...Args>
  void Dispatch(Operation *op, Args &&...args) {
    this->Visit_<Ops...>(op, std::forward<Args>(args)...);
  }
};

template< typename Derived >
using Visitor = Visitor_< Derived, node_list_t >;

template< typename D, typename L > struct DVisitor_ {};

template<typename Derived, typename ... Ops >
struct DVisitor_< Derived, TL< Ops ... > > {
  Derived &self() { return static_cast<Derived &>(*this); }

  template<typename T, typename ...Tail, typename ...Args>
  auto Visit_(uint32_t kind, Args &&... args) {
    if (kind == T::kind) {
      return self().Visit(static_cast<T *>(nullptr), std::forward<Args>(args)...);
    }
    if constexpr (sizeof...(Tail) != 0) {
      return this->Visit_<Tail ...>(kind, std::forward<Args>(args)...);
    } else {
      LOG(FATAL) << "Kind: " << kind << " does not correspond to known Operation!";
    }
  }

  template<typename ...Args>
  auto Dispatch(uint32_t kind, Args &&...args) {
    return this->Visit_<Ops...>(kind, std::forward<Args>(args)...);
  }
};

template< typename Derived >
using DVisitor = DVisitor_< Derived, node_list_t >;

template <typename Derived>
struct UniqueVisitor : public Visitor<Derived> {
  using parent = Visitor<Derived>;

  void Dispatch(Operation *op) {
    if (seen_ops.count(op)) {
      return;
    }
    seen_ops.insert(op);
    this->parent::Dispatch(op);
  }

  void Reset() {
    seen_ops.clear();
  }

  std::unordered_set<Operation *> seen_ops;
};

template<typename Kind>
static inline std::string to_string(Kind kind) {
  switch (kind) {
    case Constant::kind : return "CONSTANT";
    case Undefined::kind : return "UNDEFINED";
    case LLVMOperation::kind : return "LLVMOPERATION";
    case Not::kind : return "NOT";
    case Extract::kind : return "EXTRACT";
    case Concat::kind : return "CONCAT";
    case PopulationCount::kind : return "POPULATION_COUNT";
    case Parity::kind : return "PARITY";
    case CountLeadingZeroes::kind : return "COUNT_LEADING_ZEROES";
    case CountTrailingZeroes::kind : return "COUNT_TRAILING_ZEROES";
    case InputRegister::kind: return "INPUT_REGISTER";
    case InputImmediate::kind : return "INPUT_IMMEDIATE";
    case OutputRegister::kind : return "OUTPUT_REGISTER";
    case InputInstructionBits::kind : return "INSTRUNCTION_BITS";
    case HintCondition::kind : return "HINT_CONDITION";
    case RegisterCondition::kind : return "OUTPUT_REGISTER_CHECK";
    case PreservedCondition::kind : return "PRESERVED_REGISTER_CHECK";
    case CopyCondition::kind : return "COPIED_REGISTER_CHECK";
    case DecodeCondition::kind : return "INSTRUCTION_BITS_CHECK";
    case VerifyInstruction::kind : return "AND";
    case Hint::kind : return "HINT";
    case OnlyOneCondition::kind : return "XOR";
    case Or::kind : return "OR";
    case Circuit::kind : return "RESULT";
    case Select::kind : return "SELECT";
    case InputErrorFlag::kind : return "INPUT_ERROR_FLAG";
    case OutputErrorFlag::kind : return "OUTPUT_ERROR_FLAG";
    default : LOG(FATAL) << "Unknown kind " << kind; return "";
  }
}

template<typename Kind>
static inline Kind from_string(const std::string &name) {
  if (name == "CONSTANT") return Constant::kind;
  if (name == "UNDEFINED") return Undefined::kind;
  if (name == "LLVMOPERATION") return LLVMOperation::kind;
  if (name == "NOT") return Not::kind;
  if (name == "EXTRACT") return Extract::kind;
  if (name == "CONCAT") return Concat::kind;
  if (name == "POPULATION_COUNT") return PopulationCount::kind;
  if (name == "PARITY") return Parity::kind;
  if (name == "COUNT_LEADING_ZEROES") return CountLeadingZeroes::kind;
  if (name == "COUNT_TRAILING_ZEROES") return CountTrailingZeroes::kind;
  if (name == "INPUT_REFGISTER") return InputRegister::kind;
  if (name == "OUTPOUT_REGISTER") return OutputRegister::kind;
  if (name == "INSTRUNCTION_BITS") return InputInstructionBits::kind;
  if (name == "HINT_CONDITION") return HintCondition::kind;
  if (name == "OUTPUT_REGISTER_CHECK") return RegisterCondition::kind;
  if (name == "PRESERVED_REGISTER_CHECK") return PreservedCondition::kind;
  if (name == "COPIED_REGISTER_CHECK") return CopyCondition::kind;
  if (name == "INSTRUCTION_BITS_CHECK") return DecodeCondition::kind;
  if (name == "AND") return VerifyInstruction::kind;
  if (name == "HINT") return Hint::kind;
  if (name == "XOR") return OnlyOneCondition::kind;
  if (name == "OR") return Or::kind;
  if (name == "RESULT") return Circuit::kind;
  if (name == "SELECT") return Select::kind;
  if (name == "INPUT_ERROR_FLAG") return InputErrorFlag::kind;
  if (name == "OUTPUT_ERROR_FLAG") return OutputErrorFlag::kind;
  LOG(FATAL) << "Unknown name " << name;
  return "";
}

}  // namespace circuitous
