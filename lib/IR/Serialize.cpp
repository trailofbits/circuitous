/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Hash.h>
#include <circuitous/IR/IR.h>
#include <glog/logging.h>

#include <algorithm>
#include <cstdint>
#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>

#if 0
#  define DEBUG_WRITE(str) \
    do { \
      for (auto i = 0u; str[i]; ++i) { \
        Write(static_cast<int8_t>(str[i])); \
      } \
    } while (false)

#  define DEBUG_READ(str) \
    do { \
      int8_t ch; \
      for (auto i = 0u; str[i]; ++i) { \
        Read(ch); \
        CHECK_EQ(static_cast<char>(ch), str[i]); \
      } \
    } while (false)
#else
#  define DEBUG_READ(...)
#  define DEBUG_WRITE(...)
#endif

namespace circuitous {
namespace {

class SerializeVisitor : public Visitor<SerializeVisitor> {
 public:
  explicit SerializeVisitor(std::ostream &os_) : os(os_), hasher() {}

  void Write(Operation *op) {
    auto offset_it = offset.find(op);
    if (offset_it == offset.end()) {
      Write(static_cast<uint8_t>(0));
      DEBUG_WRITE("OP:");
      Write(hasher[op]);
      CHECK_LE(0u, op->op_code);
      CHECK_LT(op->op_code, Operation::kOnlyOneCondition);
      Write(static_cast<uint8_t>(op->op_code));
      offset.emplace(op, curr_offset);
      this->Visit(op);
      DEBUG_WRITE(";");
    } else {
      Write(static_cast<uint8_t>(0xff));
      DEBUG_WRITE("XR:");
      Write<int32_t>(curr_offset - offset_it->second);
      DEBUG_WRITE(";");
    }
  }

  void Write(uint8_t byte) {
    os << byte;
    curr_offset += 1;
  }

  void Write(int8_t byte) {
    os << static_cast<uint8_t>(byte);
    curr_offset += 1;
  }

  template <typename T>
  void Write(const std::vector<T> &elems) {
    DEBUG_WRITE("[");
    Write<uint32_t>(static_cast<uint32_t>(elems.size()));
    for (const auto &elem : elems) {
      Write(elem);
    }
    DEBUG_WRITE("]");
  }

  template <typename T>
  void Write(const UseList<T> &elems) {
    DEBUG_WRITE("[");
    Write<uint32_t>(static_cast<uint32_t>(elems.Size()));
    auto sep = "";
    for (const auto elem : elems) {
      DEBUG_WRITE(sep);
      Write(elem);
      sep = "|";
    }
    DEBUG_WRITE("]");
  }

  void Write(const std::string &str) {
    DEBUG_WRITE("<");
    Write<uint32_t>(static_cast<uint32_t>(str.size()));
    for (auto ch : str) {
      Write(ch);
    }
    DEBUG_WRITE(">");
  }

  template <typename T>
  void Write(const T &data) {
    auto bytes = reinterpret_cast<const uint8_t *>(&data);
    for (auto i = 0ull; i < sizeof(data); ++i) {
      Write(static_cast<uint8_t>(bytes[i]));
    }
  }

  void VisitOperation(Operation *op) {
    DEBUG_WRITE("{size=");
    Write(op->size);
    DEBUG_WRITE(";operands=");
    Write(op->operands);
    DEBUG_WRITE("}");
  }

  void VisitConstant(Constant *op) {
    DEBUG_WRITE("{CONST:");
    Write(op->size);
    Write(op->bits);
    DEBUG_WRITE("}");
  }

  void VisitInputRegister(InputRegister *op) {
    DEBUG_WRITE("{INREG:");
    Write(op->size);
    Write(op->reg_name);
    DEBUG_WRITE("}");
  }

  void VisitOutputRegister(OutputRegister *op) {
    DEBUG_WRITE("{OUTREG:");
    Write(op->size);
    Write(op->reg_name);
    DEBUG_WRITE("}");
  }

  void VisitExtract(Extract *op) {
    DEBUG_WRITE("{EXTRACT:");
    Write(op->high_bit_exc);
    Write(op->low_bit_inc);
    Write(op->operands[0]);
    DEBUG_WRITE("}");
  }

  void VisitLLVMOperation(LLVMOperation *op) {
    DEBUG_WRITE("{LLVM:");
    Write(op->size);
    Write(op->llvm_op_code);
    Write(op->llvm_predicate);
    Write(op->operands);
    DEBUG_WRITE("}");
  }

 private:
  std::ostream &os;
  int32_t curr_offset{0};
  std::unordered_map<Operation *, int32_t> offset;
  HashVisitor hasher;
};

class DeserializeVisitor {
 public:
  explicit DeserializeVisitor(std::istream &is_, Circuit *circuit_)
      : is(is_),
        circuit(circuit_),
        hasher() {}

  Operation *Decode(unsigned op_code) {
    switch (op_code) {
#define GOTO_VISITOR(type, field) \
  case Operation::k##type: return this->Decode##type();

      FOR_EACH_OPERATION(GOTO_VISITOR)
#undef GOTO_VISITOR

      default:
        LOG(FATAL) << "Cannot deserialize opcode " << op_code;
        return nullptr;
    }
  }

  void Read(Operation *&op) {
    uint8_t sel = 0;
    Read(sel);
    if (sel == 0u) {
      uint64_t hash = 0;
      DEBUG_READ("OP:");
      Read(hash);
      uint8_t op_code = 0;
      Read(op_code);
      auto prev_offset = curr_offset;

      op = Decode(op_code);

      DEBUG_READ(";");

      auto range = hash_to_ops.equal_range(hash);
      Operation *found_op = nullptr;
      for (auto it = range.first; it != range.second; ++it) {
        if (op->Equals(it->second)) {
          found_op = it->second;
          break;
        }
      }

      if (!found_op) {
        hash_to_ops.emplace(hash, op);
      } else if (found_op != op) {
        op->ReplaceAllUsesWith(found_op);
        op = found_op;
      }
      offset_to_op.emplace(prev_offset, op);

    } else if (sel == 0xffu) {
      DEBUG_READ("XR:");
      int32_t disp = 0;
      auto prev_offset = curr_offset;
      Read(disp);
      DEBUG_READ(";");
      auto op_offset = prev_offset - disp;
      auto op_it = offset_to_op.find(op_offset);
      if (op_it == offset_to_op.end()) {
        LOG(FATAL) << "Could not find node at offset " << op_offset;
      } else {
        op = op_it->second;
      }

    } else {
      LOG(FATAL) << "Unexpected tag for an operation reference: " << sel;
    }
  }

  void Read(uint8_t &byte) {
    CHECK(!is.eof());
    CHECK(is.good());
    const auto old_offset = static_cast<long long>(is.tellg());
    is >> byte;
    if (!is.eof()) {
      CHECK_EQ(old_offset + 1, static_cast<long long>(is.tellg()));
    }
    curr_offset += 1;
  }

  void Read(int8_t &byte) {
    uint8_t b;
    CHECK(!is.eof());
    CHECK(is.good());
    const auto old_offset = static_cast<long long>(is.tellg());
    is >> b;
    if (!is.eof()) {
      CHECK_EQ(old_offset + 1, static_cast<long long>(is.tellg()));
    }
    byte = static_cast<int8_t>(b);
    curr_offset += 1;
  }

  template <typename T>
  void Read(std::vector<T> &elems) {
    DEBUG_READ("[");
    uint32_t size = 0u;
    Read(size);
    elems.resize(size);
    for (auto i = 0u; i < size; ++i) {
      Read(elems[i]);
    }
    DEBUG_READ("]");
  }

  template <typename T>
  void Read(UseList<T> &elems) {
    DEBUG_READ("[");
    uint32_t size = 0u;
    Read(size);
    auto sep = "";
    for (auto i = 0u; i < size; ++i) {
      T *ref = nullptr;
      DEBUG_READ(sep);
      Read(ref);
      elems.AddUse(ref);
      sep = "|";
    }
    DEBUG_READ("]");
  }

  void Read(std::string &str) {
    DEBUG_READ("<");
    uint32_t size = 0u;
    Read(size);
    str.resize(size);
    for (auto i = 0u; i < size; ++i) {
      Read(str[i]);
    }
    DEBUG_READ(">");
  }

  template <typename T>
  void Read(T &data) {
    auto bytes = reinterpret_cast<uint8_t *>(&data);
    for (auto i = 0ull; i < sizeof(data); ++i) {
      Read(bytes[i]);
    }
  }

  InputRegister *DecodeInputRegister(void) {
    unsigned size = 0;
    std::string reg_name;
    DEBUG_READ("{INREG:");
    Read(size);
    Read(reg_name);
    DEBUG_READ("}");
    return circuit->input_regs.Create(size, std::move(reg_name));
  }

  Constant *DecodeConstant(void) {
    unsigned size = 0;
    std::string bits;
    DEBUG_READ("{CONST:");
    Read(size);
    Read(bits);
    DEBUG_READ("}");
    return circuit->constants.Create(std::move(bits), size);
  }

  OutputRegister *DecodeOutputRegister(void) {
    unsigned size = 0;
    std::string reg_name;
    DEBUG_READ("{OUTREG:");
    Read(size);
    Read(reg_name);
    DEBUG_READ("}");
    return circuit->output_regs.Create(size, std::move(reg_name));
  }

  Extract *DecodeExtract(void) {
    unsigned high_bit_exc = 0;
    unsigned low_bit_inc = 0;
    Operation *value = nullptr;
    DEBUG_READ("{EXTRACT:");
    Read(high_bit_exc);
    Read(low_bit_inc);
    Read(value);
    DEBUG_READ("}");
    auto op = circuit->extracts.Create(low_bit_inc, high_bit_exc);
    op->operands.AddUse(value);
    return op;
  }

  LLVMOperation *DecodeLLVMOperation(void) {
    unsigned size;
    unsigned llvm_op_code;
    unsigned llvm_predicate;
    DEBUG_READ("{LLVM:");
    Read(size);
    Read(llvm_op_code);
    Read(llvm_predicate);
    auto op = circuit->llvm_insts.Create(llvm_op_code, llvm_predicate, size);
    Read(op->operands);
    DEBUG_READ("}");
    return op;
  }

#define DECODE_GENERIC(cls, field) \
  cls *Decode##cls(void) { \
    unsigned size = 0; \
    DEBUG_READ("{size="); \
    Read(size); \
    auto op = circuit->field.Create(size); \
    DEBUG_READ(";operands="); \
    Read(op->operands); \
    DEBUG_READ("}"); \
    return op; \
  }

#define DECODE_CONDITION(cls, field) \
  cls *Decode##cls(void) { \
    unsigned size = 0; \
    DEBUG_READ("{size="); \
    Read(size); \
    CHECK_EQ(size, 1); \
    auto op = circuit->field.Create(); \
    DEBUG_READ(";operands="); \
    Read(op->operands); \
    DEBUG_READ("}"); \
    return op; \
  }

  DECODE_GENERIC(Undefined, undefs)
  DECODE_GENERIC(Not, nots)
  DECODE_GENERIC(Concat, concats)
  DECODE_GENERIC(PopulationCount, popcounts)
  DECODE_CONDITION(Parity, parities)
  DECODE_GENERIC(CountLeadingZeroes, clzs)
  DECODE_GENERIC(CountTrailingZeroes, ctzs)
  DECODE_CONDITION(ReadMemoryCondition, memory_reads)
  DECODE_CONDITION(RegisterCondition, transitions)
  DECODE_CONDITION(PreservedCondition, preserved_regs)
  DECODE_CONDITION(CopyCondition, copied_regs)
  DECODE_GENERIC(InputInstructionBits, inst_bits)
  DECODE_CONDITION(DecodeCondition, decode_conditions)
  DECODE_GENERIC(Hint, hints)
  DECODE_GENERIC(EquivalenceClass, eq_classes)
  DECODE_CONDITION(VerifyInstruction, verifications)

  Operation *DecodeOnlyOneCondition(void) {
    LOG(FATAL) << "OnlyOneCondition nodes should not appear in serialized file";
    return nullptr;
  }

  std::istream &is;
  Circuit *const circuit;
  int32_t curr_offset{0};
  std::unordered_map<int32_t, Operation *> offset_to_op;
  std::unordered_multimap<uint64_t, Operation *> hash_to_ops;
  HashVisitor hasher;
  std::vector<Operation *> ops;
  std::vector<Undefined *> undefs;
};

}  // namespace

void Circuit::Serialize(std::ostream &os) {
  SerializeVisitor vis(os);
  for (auto xor_all_op : xor_all) {
    for (auto op : xor_all_op->operands) {
      vis.Write(op);
    }
  }
  os.flush();
}

std::unique_ptr<Circuit> Circuit::Deserialize(std::istream &is) {
  std::unique_ptr<Circuit> circuit(new Circuit);
  DeserializeVisitor vis(is, circuit.get());

  auto old_flags = is.flags();
  is.unsetf(std::ios::skipws);

  while (is.good() && !is.eof() && EOF != is.peek()) {
    Operation *op = nullptr;
    vis.Read(op);
    if (!op) {
      break;
    }
  }
  is.flags(old_flags);

  std::unordered_map<std::string, OutputRegister *> out_regs;
  std::unordered_map<std::string, InputRegister *> in_regs;
  std::unordered_set<std::string> seen_reg_names;
  for (auto in_reg : circuit->input_regs) {
    in_regs.emplace(in_reg->reg_name, in_reg);
  }

  for (auto out_reg : circuit->output_regs) {
    out_regs.emplace(out_reg->reg_name, out_reg);
  }

  for (const auto &[name, op] : in_regs) {
    if (!out_regs.count(name)) {
      out_regs.emplace(name,
                       circuit->output_regs.Create(op->size, op->reg_name));
    }
  }

  for (const auto &[name, op] : out_regs) {
    if (!in_regs.count(name)) {
      in_regs.emplace(name, circuit->input_regs.Create(op->size, op->reg_name));
    }
  }

  std::unordered_map<std::string, PreservedCondition *> preserved_regs;
  for (auto cond : circuit->preserved_regs) {
    auto lhs = dynamic_cast<InputRegister *>(cond->operands[0]);
    auto rhs = dynamic_cast<OutputRegister *>(cond->operands[1]);
    CHECK_NOTNULL(lhs);
    CHECK_NOTNULL(rhs);
    CHECK_EQ(lhs->reg_name, rhs->reg_name);
    preserved_regs.emplace(lhs->reg_name, cond);
  }

  auto xor_all = circuit->xor_all.Create();
  circuit->operands.AddUse(xor_all);

  for (auto [offset, op] : vis.offset_to_op) {
    (void) offset;
    auto verify = dynamic_cast<VerifyInstruction *>(op);
    if (!verify) {
      continue;
    }

    xor_all->operands.AddUse(verify);

    seen_reg_names.clear();
    for (auto op : verify->operands) {
      OutputRegister *out_reg = nullptr;

      if (auto transition = dynamic_cast<RegisterCondition *>(op); transition) {
        out_reg = dynamic_cast<OutputRegister *>(transition->operands[1]);

      } else if (auto preserved = dynamic_cast<PreservedCondition *>(op);
                 preserved) {
        out_reg = dynamic_cast<OutputRegister *>(preserved->operands[1]);

      } else if (auto copied = dynamic_cast<CopyCondition *>(op); copied) {
        out_reg = dynamic_cast<OutputRegister *>(copied->operands[1]);
      }

      if (out_reg) {
        CHECK(in_regs.count(out_reg->reg_name));
        CHECK(out_regs.count(out_reg->reg_name));
        seen_reg_names.insert(out_reg->reg_name);
      }
    }

    // Make sure that all verify instruction calls end up verifying the
    // transitions of all registers.
    for (const auto &[name, out_reg] : out_regs) {
      if (!seen_reg_names.count(name)) {
        auto &cond = preserved_regs[name];
        if (!cond) {
          cond = circuit->preserved_regs.Create();
          cond->operands.AddUse(in_regs[name]);
          cond->operands.AddUse(out_regs[name]);
        }
        verify->operands.AddUse(cond);
      }
    }
  }

#define CLEAR_UNUSED(cls, field) circuit->field.RemoveUnused();

  FOR_EACH_OPERATION(CLEAR_UNUSED)
#undef CLEAR_UNUSED

  return circuit;
}

}  // namespace circuitous
