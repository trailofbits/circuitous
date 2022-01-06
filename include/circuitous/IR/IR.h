/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Util/UseDef.h>
#include <circuitous/Util/TypeList.hpp>
#include <circuitous/IR/Metadata.hpp>

#include <bitset>
#include <iosfwd>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_set>
#include <unordered_map>
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
namespace circ {

// A general instruction.
struct Operation : public Node<Operation>, HasStringMeta {
 public:
  static constexpr uint32_t kind = 0;
  static constexpr uint32_t bool_size = 1u;

  virtual ~Operation() = default;

  virtual std::string Name() const;
  static std::string op_code_str() { NOT_IMPLEMENTED(); }
  virtual bool Equals(const Operation *that) const;

  auto &operator[](std::size_t idx) { return operands[idx]; }
  const auto &operator[](std::size_t idx) const { return operands[idx]; }

  // `id` should be unique in a given circuit.
  uint64_t id() const { return _id; }


  // Size in bits of this instruction's "result" value. For example, a zero-
  // extension will represent the size of the output value.
  const unsigned size{0};

  // The "opcode" of this.
  const unsigned op_code{0};

  // Must be set manually after ctor is called.
  uint64_t _id = 0;


  template <typename Vis>
  void Traverse(Vis &vis) {
    for (auto op : operands) {
      vis.Dispatch(op);
    }
  }

  // Please note, that id is not set.
  explicit Operation(unsigned size_, unsigned op_code_) : size(size_), op_code(op_code_) {}
};

template< typename T >
bool is_specialization(uint32_t derived) {
  auto relevant = derived & T::mask;
  return relevant == T::kind;
}

template<typename ...Ts>
bool is_one_of(Operation *op) {
  return (is_specialization<Ts>(op->op_code) || ...);
}

template< typename T >
bool is_of(Operation *op) {
  return (op->op_code & T::mask) == T::apply(0u);
}

template< typename Base, Base root_, uint8_t position_ >
struct kind_fragment {
  static constexpr Base root = root_;
  static constexpr uint8_t position = position_;

  static constexpr uint32_t apply(uint32_t to) {
    return ( static_cast< uint32_t >( root ) << position ) + to;
  }

  static constexpr uint32_t mask = static_cast< uint32_t >( Base{0} - 1 ) << position;
  static constexpr uint32_t kind = apply(0u);
};

template< uint8_t root > using type_fragment = kind_fragment< uint8_t, root, 24 >;
template< uint16_t root > using tag_fragment = kind_fragment< uint16_t, root, 8 >;
template< uint8_t root > using meta_fragment = kind_fragment< uint8_t, root, 0 >;

template< typename ... Fragments >
struct make_kind_ : Fragments ... {

  template<typename T, typename ...Ts>
  static constexpr uint32_t _apply(uint32_t to) {
    uint32_t tmp = T::apply(to);
    if constexpr (sizeof...(Ts) != 0) {
      return _apply<Ts...>(tmp);
    }
    return tmp;
  }

  static constexpr uint32_t apply(uint32_t to) {
    return _apply< Fragments... >(to);
  }

  static constexpr uint32_t mask = ( Fragments::mask | ... | 0u );
  static constexpr uint32_t kind = apply(0u);
};

template< typename Type, typename Tag, typename Meta = meta_fragment< 3 > >
using make_kind = make_kind_< Type, Tag, Meta >;

using LeafValue = type_fragment< 0x1 >;
using HiddenValue = type_fragment< 0x2 >;
using BitManip = type_fragment< 0x3 >;
using BitOp = type_fragment< 0x4 >;

using Computational = type_fragment< 0x5 >;

using Constraint = type_fragment< 0x6 >;
using Uncat = type_fragment< 0x7 >;
using BoolOp = type_fragment< 0x9 >;

using Root = type_fragment< 0xf >;

/* Leaves */

template< template< typename > class T >
struct Input : T< meta_fragment< 0 > > {
  using parent_t = T< meta_fragment< 0 > >;
  using parent_t::parent_t;
  static std::string op_code_str() { return "in." + parent_t::op_code_str(); }
  std::string Name() const override { return "In." + parent_t::Name(); }
};

template< template< typename > class T >
struct Output : T< meta_fragment< 1 > > {
  using parent_t = T< meta_fragment< 1 > >;
  using parent_t::parent_t;
  static std::string op_code_str() { return "out." + parent_t::op_code_str(); }
  std::string Name() const override { return "Out." + parent_t::Name(); }
};

/* I/O & Leaf nodes */

template< typename meta_fragment_ >
struct Register : Operation, make_kind< LeafValue, tag_fragment< 3 >, meta_fragment_ > {
  using make_kind< LeafValue, tag_fragment< 3 >, meta_fragment_ >::apply;
  static constexpr uint32_t kind = apply(Operation::kind);

  Register(const std::string &rn_, uint32_t size_)
      : Operation(size_, kind), reg_name(rn_)
  {}

  static std::string op_code_str() { return "register"; }
  std::string Name() const override { return "register." + reg_name; }
  bool Equals(const Operation *other) const override { NOT_IMPLEMENTED(); }

  std::string reg_name;
};

using InputRegister = Input< Register >;
using OutputRegister = Output< Register >;

template< typename meta_fragment_ >
struct ErrorFlag : Operation, make_kind< LeafValue, tag_fragment< 4 >, meta_fragment_ > {
  using make_kind< LeafValue, tag_fragment< 4 >, meta_fragment_ >::apply;
  static constexpr uint32_t kind = apply(Operation::kind);

  ErrorFlag(uint32_t size_ = 1u) : Operation(size_, kind) {}

  static std::string op_code_str() { return "error_flag"; }
  std::string Name() const override { return "error_flag"; }
};

using InputErrorFlag = Input< ErrorFlag >;
using OutputErrorFlag = Output< ErrorFlag >;

template< typename meta_fragment_ >
struct Timestamp : Operation, make_kind< LeafValue, tag_fragment< 7 >, meta_fragment_ > {
  using make_kind< LeafValue, tag_fragment< 7 >, meta_fragment_ >::apply;
  static constexpr uint32_t kind = apply(Operation::kind);

  Timestamp(uint32_t size_ = 64u) : Operation(size_, kind) {}

  static std::string op_code_str() { return "timestamp"; }
  std::string Name() const override { return "timestamp"; }
};

using InputTimestamp = Input< Timestamp >;
using OutputTimestamp = Output< Timestamp >;

// An undefined value.
struct Undefined final : Operation, make_kind< LeafValue, tag_fragment< 2 > > {
  static constexpr uint32_t kind = apply(Operation::kind);

  explicit Undefined(unsigned size_) : Operation(size_, kind) {}
  static std::string op_code_str() { return "undefined"; }
  std::string Name() const override { return "undefined"; }
};

struct Memory : Operation, make_kind< LeafValue, tag_fragment< 8 > > {
  static constexpr uint32_t kind = apply(Operation::kind);

  explicit Memory(unsigned size_, uint32_t mem_idx_)
      : Operation(size_, kind), mem_idx(mem_idx_) {}

  static uint32_t  expected_size(uint32_t ptr_size);

  static std::string op_code_str() { return "memory"; }
  std::string Name() const override { return "memory." + std::to_string(mem_idx); }

  uint32_t mem_idx = 0;
};

struct Constant final : Operation, make_kind< LeafValue, tag_fragment< 1 > > {
  static constexpr uint32_t kind = apply(Operation::kind);

  explicit Constant(std::string bits_, unsigned size_)
      : Operation(size_, kind),
        bits(std::move(bits_))
  {}

  bool Equals(const Operation *that) const override;
  static std::string op_code_str() { return "constant"; }
  std::string Name() const override;

  // Value of this constant. The least significant bit is stored in `bits[0]`,
  // and the most significant bit is stored in `bits[size - 1u]`.
  const std::string bits;
};

struct Advice final : Operation, make_kind< LeafValue, tag_fragment< 5 > > {
  static constexpr uint32_t kind = apply(Operation::kind);

  inline explicit Advice(unsigned size_, uint32_t advice_idx_)
    : Operation(size_, kind), advice_idx(advice_idx_) {}

  static std::string op_code_str() { return "Advice"; }
  std::string Name() const override { return "Advice." + std::to_string(advice_idx) ; }

  uint32_t advice_idx = 0;
};

// Input bits that
struct InputInstructionBits : Operation, make_kind< LeafValue, tag_fragment< 6 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  explicit InputInstructionBits(unsigned size_) : Operation(size_, kind) {}

  static std::string op_code_str() { return "instruction_bits"; }
  std::string Name() const override { return "instruction_bits"; }
};

using leaf_values_ts = tl::make_list<
    InputInstructionBits, Advice, Constant, Undefined,
    InputTimestamp, OutputTimestamp,
    InputErrorFlag, OutputErrorFlag,
    InputRegister, OutputRegister, Memory
>;

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

struct MemoryConstraint : Operation {
  using Operation::Operation;

  enum : uint8_t { kFixed = 0u, kSize = 1u, kAddr = 2u, kTS = 3u, kValue = 4u };

  auto hint_arg() const  { return operands[kFixed]; }
  auto size_arg() const { return operands[kSize]; }
  auto addr_arg() const { return operands[kAddr]; }
  auto ts_arg() const { return operands[kTS]; }
  auto val_arg() const { return operands[kValue]; }
  auto mem_idx() const {
    return dynamic_cast< Memory * >( hint_arg() )->mem_idx;
  }
};

// A comparison between the proposed output value of a register, and the
// output register itself.
struct RegConstraint final : EnforceCtx, make_kind< Constraint, tag_fragment< 0 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  RegConstraint() : EnforceCtx(this->bool_size, kind) {}
  static std::string op_code_str() { return "register_constraint"; }
  std::string Name() const override { return "register_constraint." + EnforceCtx::suffix_(); }
};

// A comparison between the proposed output value of a register, and the
// output register itself.
struct AdviceConstraint final : EnforceCtx, make_kind< Constraint, tag_fragment< 1 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  AdviceConstraint() : EnforceCtx(this->bool_size, kind) {}
  static std::string op_code_str() { return "advice_constraint"; }
  std::string Name() const override { return "advice_constraint." + EnforceCtx::suffix_(); }
};

// Says that we are preserving the value of a register.
struct PreservedConstraint final : EnforceCtx, make_kind< Constraint, tag_fragment< 2 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  PreservedConstraint() : EnforceCtx(this->bool_size, kind) {}
  static std::string op_code_str() { return "preserved_constraint"; }
  std::string Name() const override { return "preserved_constraint." + EnforceCtx::suffix_(); }
};

// Says that we are moving one register to a different register.
struct CopyConstraint final : EnforceCtx, make_kind< Constraint, tag_fragment< 3 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  CopyConstraint() : EnforceCtx(this->bool_size, kind) {}
  static std::string op_code_str() { return "copy_constraint"; }
  std::string Name() const override { return "copy_constraint." + EnforceCtx::suffix_(); }
};

struct WriteConstraint : MemoryConstraint, make_kind< Constraint, tag_fragment< 4 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  WriteConstraint() : MemoryConstraint(this->bool_size, kind) {}
  static std::string op_code_str() { return "write_constraint"; }
  std::string Name() const override { return "write_constraint"; }
};

struct ReadConstraint : MemoryConstraint, make_kind< Constraint, tag_fragment< 5 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  ReadConstraint() : MemoryConstraint(this->bool_size, kind) {}
  static std::string op_code_str() { return "read_constraint"; }
  std::string Name() const override { return "read_constraint"; }

  Operation *val_arg() const { UNREACHABLE() << "There is no `val_arg` in read_constraint."; }
};

struct UnusedConstraint : Operation, make_kind< Constraint, tag_fragment< 6 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  UnusedConstraint() : Operation(this->bool_size, kind) {}
  static std::string op_code_str() { return "unused_constraint"; }
  std::string Name() const override { return "unused_constraint"; }
};

// TODO(lukas): It would be nice to move these to struct defs
static inline bool constrained_by(Operation *v, Operation *c) {
  switch (v->op_code) {
    case OutputErrorFlag::kind:
    case OutputTimestamp::kind:
    case OutputRegister::kind :
        return is_one_of<RegConstraint, PreservedConstraint, CopyConstraint>(c);
    case Advice::kind : return is_one_of<AdviceConstraint>(c);
    case Memory::kind :
        return is_one_of<ReadConstraint, WriteConstraint, UnusedConstraint>(c);
    default: return true;
  }
}

/* LLVMOP */

#define declare_llvm_op(cls, idx) \
struct cls final : Operation, make_kind< Computational, tag_fragment< idx > > { \
  static constexpr uint32_t kind = apply(Operation::kind); \
  cls(unsigned size_) : Operation(size_, kind) {} \
  static std::string op_code_str() { return #cls; } \
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
declare_llvm_op(Icmp_uge, 16)
declare_llvm_op(Icmp_ule, 17)
declare_llvm_op(Icmp_sgt, 18)
declare_llvm_op(Icmp_sge, 19)
declare_llvm_op(Icmp_sle, 20)

struct BSelect final : Operation, make_kind< Computational, tag_fragment< 21 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  BSelect(unsigned size_) : Operation(size_, kind) {}
  static std::string op_code_str() { return "BSelect"; }
  std::string Name() const override { return "BSelect"; }

  Operation *cond() { return operands[0]; }
  Operation *true_v() { return operands[1]; }
  Operation *false_v() { return operands[2]; }
};

declare_llvm_op(CAnd, 22);
declare_llvm_op(COr, 23);
declare_llvm_op(CXor, 24);

#undef declare_llvm_op

using llvm_ops_t = tl::TL<
    Add, Sub, Mul, UDiv, SDiv, Shl, LShr, AShr, Trunc, ZExt, SExt,
    Icmp_ult, Icmp_slt, Icmp_ugt, Icmp_eq, Icmp_ne, Icmp_uge, Icmp_ule,
    Icmp_sgt, Icmp_sge, Icmp_sle,
    BSelect,
    CAnd, COr, CXor
>;

/* Hidden */

struct InputImmediate : Operation, make_kind< HiddenValue, tag_fragment< 0 > > {
  static constexpr uint32_t kind = apply(Operation::kind);

  static std::string op_code_str() { return "input_immediate"; }
  std::string Name() const override { return "input_immediate"; }
  bool Equals(const Operation *that) const override;

  explicit InputImmediate(unsigned size_) : Operation(size_, kind) {}
};

/* BitManips */

struct Extract final : Operation, make_kind< BitManip, tag_fragment< 0 > > {
  static constexpr uint32_t kind = apply(Operation::kind);

  static std::string op_code_str() { return "extract"; }
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


struct Concat final : Operation, make_kind< BitManip, tag_fragment< 1 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  Concat(uint32_t size_) : Operation(size_, kind) {}
  static std::string op_code_str() { return "concat"; }
  std::string Name() const override { return "concat"; }
};

/* BitOps */

struct PopulationCount final : Operation, make_kind< BitOp, tag_fragment< 0 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  explicit PopulationCount(unsigned size_) : Operation(size_, kind) {}
  static std::string op_code_str() { return "pop_count"; }
  std::string Name() const override { return "pop_count"; }
};

struct CountLeadingZeroes final : Operation, make_kind< BitOp, tag_fragment< 1 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  explicit CountLeadingZeroes(unsigned size_) : Operation(size_, kind) {}
  static std::string op_code_str() { return "count_lead_zeroes"; }
  std::string Name() const override { return "count_lead_zeroes"; }
};

struct CountTrailingZeroes final : Operation, make_kind< BitOp, tag_fragment< 2 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  explicit CountTrailingZeroes(unsigned size_) : Operation(size_, kind) {}
  static std::string op_code_str() { return "count_trailing_zeroes"; }
  std::string Name() const override { return "count_trailing_zeroes"; }
};

struct Not final : Operation, make_kind< BitOp, tag_fragment< 3 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  explicit Not(unsigned size_) : Operation(size_, kind) {}
  static std::string op_code_str() { return "not"; }
  std::string Name() const override { return "not"; }
};

struct Parity final : Operation, make_kind< BitOp, tag_fragment< 4 > > {
  static constexpr uint32_t kind = apply(Operation::kind);
  explicit Parity() : Operation(1, kind) {}

  static std::string op_code_str() { return "parity"; }
  std::string Name() const override { return "parity"; }
};

/* Without category */

struct Select : Operation, make_kind< Uncat, tag_fragment< 0 > > {
  static constexpr uint32_t kind = apply(Operation::kind);

  explicit Select(uint32_t bits_, uint32_t size_)
      : Operation(size_, kind), bits(bits_)
  {}
  static std::string op_code_str() { return "select"; }
  std::string Name() const override {
    std::stringstream ss;
    ss << "select." << bits;
    return ss.str();
  }

  Operation *selector() { return operands[0]; }

  // Return one of the `2 ^ bits` values. It is also expected that this node
  // has `2 ^ bits + 1` operands.
  uint32_t bits = 0;
};

#define make_bool_op(cls, idx) \
struct cls final : Operation, make_kind< BoolOp, tag_fragment< idx > > { \
  static constexpr uint32_t kind = apply(Operation::kind); \
  cls() : Operation(this->bool_size, kind) {} \
  static std::string op_code_str() { return #cls; } \
  std::string Name() const override { return #cls; } \
};

make_bool_op(DecodeCondition, 0)
make_bool_op(VerifyInstruction, 1)
make_bool_op(Or, 2)
make_bool_op(OnlyOneCondition, 3)
make_bool_op(And, 4)

#undef make_bool_op

using generic_list_t =
tl::TL<
  Not, Concat,
  CountLeadingZeroes, CountTrailingZeroes, Extract, PopulationCount,
  Parity, InputImmediate,
  RegConstraint, PreservedConstraint, CopyConstraint, DecodeCondition,
  ReadConstraint, WriteConstraint, UnusedConstraint,
  VerifyInstruction, OnlyOneCondition,
  AdviceConstraint, Select, And,
  Or
>;

using subnode_list_t = tl::merge< generic_list_t, llvm_ops_t, leaf_values_ts >;



}  // namespace circ
