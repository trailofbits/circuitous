/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Hash.h>
#include <circuitous/IR/IR.h>
#include <circuitous/Printers.h>
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <glog/logging.h>
#pragma clang diagnostic pop


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
#  define DEBUG_READ(...) std::cerr << __VA_ARGS__;
#  define DEBUG_WRITE(...) std::cerr << __VA_ARGS__;
#endif

namespace circuitous {
namespace {

struct FileConfig {
  enum class Selector : uint8_t {
    Operation = 0x0,
    Invalid = 0x1,
    Reference = 0xff
  };

  using raw_op_code_t = uint64_t;
  using raw_id_t = uint64_t;

  std::string to_string(const Selector &sel) {
    switch(sel) {
      case Selector::Operation : return "Operation";
      case Selector::Invalid : return "Invalid";
      case Selector::Reference : return "Reference";
    }
  }

};

/* Format in the ir file is following
 * 8: selector, 64: id, 64: opcode, ... rest of the data dependeing on opcode
 * 8: selector, 64: id it references
 * NOTE(lukas): In attempt to make this deterministic I have chosen to replace
 *              pointer hashes with deterministic ids. If you want, feel free to
 *              tweak it, but please make sure `x == store(load(x))` -- with respect
 *              to semantics (not neccessary ids or other internals).
 */
class SerializeVisitor : public Visitor<SerializeVisitor>, FileConfig {
 using Selector = FileConfig::Selector;

 public:
  ~SerializeVisitor(void) {
    os.flush();
  }

  explicit SerializeVisitor(std::ostream &os_) : os(os_) {}

  void Write(Selector sel) {
    Write(static_cast<std::underlying_type_t<Selector>>(sel));
  }

  void Write(Operation *op) {
    if (!written.count(op->id())) {
      Write(Selector::Operation);
      DEBUG_WRITE("OP:");
      raw_id_t id = hasher[op];
      Write(id);
      CHECK_LE(0u, op->op_code);
      CHECK_LT(op->op_code, Operation::kOnlyOneCondition);
      raw_op_code_t op_code = op->op_code;
      Write(op_code);
      written.insert(op->id());
      this->Visit(op);
      DEBUG_WRITE(";");
    } else {
      Write(Selector::Reference);
      DEBUG_WRITE("XR:");
      Write<raw_id_t>(op->id());
      DEBUG_WRITE(";");
    }
  }

  void Write(uint8_t byte) {
    os << byte;
  }

  void Write(int8_t byte) {
    os << static_cast<uint8_t>(byte);
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

  void VisitInputImmediate(InputImmediate *op) {
    DEBUG_WRITE("{INIMM:");
    Write(op->size);
    DEBUG_WRITE(";operands")
    Write(op->operands);
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
  IdentityHasher hasher;
  std::unordered_set<uint64_t> written;
};

class DeserializeVisitor : FileConfig {
  using Selector = FileConfig::Selector;
 public:
  explicit DeserializeVisitor(std::istream &is_, Circuit *circuit_)
      : is(is_),
        circuit(circuit_)
  {}

  Operation *Decode(raw_id_t id, raw_op_code_t op_code) {
    switch (op_code) {
#define GOTO_VISITOR(type) \
  case Operation::k##type: return this->Decode##type(id);

      FOR_EACH_OPERATION(GOTO_VISITOR)
#undef GOTO_VISITOR

      default:
        LOG(FATAL) << "Cannot deserialize opcode " << op_code;
        return nullptr;
    }
  }

  void Read(Operation *&op) {
    Selector sel = Selector::Invalid;
    Read(sel);
    if (sel == Selector::Operation) {
      raw_id_t hash = 0;
      DEBUG_READ("OP:");
      Read(hash);
      raw_op_code_t op_code = 0;
      Read(op_code);

      op = Decode(hash, op_code);
      id_to_op[hash] = op;

      DEBUG_READ(";");
    } else if (sel == Selector::Reference) {
      DEBUG_READ("XR:");
      raw_id_t hash = 0;
      Read(hash);
      DEBUG_READ(";");
      auto op_it = id_to_op.find(hash);
      if (op_it == id_to_op.end()) {
        LOG(FATAL) << "Could not reference with id: " << hash;
      } else {
        op = op_it->second;
      }

    } else {
      LOG(FATAL) << "Unexpected tag for an operation reference: " << this->to_string(sel);
    }
  }

  void Read(Selector &sel) {
    std::underlying_type_t<Selector> out;
    Read(out);
    sel = static_cast<Selector>(out);
  }

  void Read(uint8_t &byte) {
    CHECK(!is.eof());
    CHECK(is.good());
    const auto old_offset = static_cast<long long>(is.tellg());
    is >> byte;
    if (!is.eof()) {
      CHECK_EQ(old_offset + 1, static_cast<long long>(is.tellg()));
    }
  }

  void Read(int8_t &byte) {
    uint8_t b;
    Read(b);
    byte = static_cast<int8_t>(b);
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

  InputRegister *DecodeInputRegister(uint64_t id) {
    unsigned size = 0;
    std::string reg_name;
    DEBUG_READ("{INREG:");
    Read(size);
    Read(reg_name);
    DEBUG_READ("}");
    return circuit->Adopt<InputRegister>(id, size, std::move(reg_name));
  }

  InputImmediate *DecodeInputImmediate(uint64_t id) {
    unsigned size = 0;
    DEBUG_READ("{INIMM:");
    Read(size);
    auto self = circuit->Adopt<InputImmediate>(id, size);
    Read(self->operands);
    CHECK(self->operands.Size() == 1);
    DEBUG_READ("}");
    return self;
  }

  Constant *DecodeConstant(uint64_t id) {
    unsigned size = 0;
    std::string bits;
    DEBUG_READ("{CONST:");
    Read(size);
    Read(bits);
    DEBUG_READ("}");
    return circuit->Adopt<Constant>(id, std::move(bits), size);
  }

  OutputRegister *DecodeOutputRegister(uint64_t id) {
    unsigned size = 0;
    std::string reg_name;
    DEBUG_READ("{OUTREG:");
    Read(size);
    Read(reg_name);
    DEBUG_READ("}");
    return circuit->Adopt<OutputRegister>(id, size, std::move(reg_name));
  }

  Extract *DecodeExtract(uint64_t id) {
    unsigned high_bit_exc = 0;
    unsigned low_bit_inc = 0;
    Operation *value = nullptr;
    DEBUG_READ("{EXTRACT:");
    Read(high_bit_exc);
    Read(low_bit_inc);
    Read(value);
    DEBUG_READ("}");
    auto op = circuit->Adopt<Extract>(id, low_bit_inc, high_bit_exc);
    op->operands.AddUse(value);
    return op;
  }

  LLVMOperation *DecodeLLVMOperation(uint64_t id) {
    unsigned size;
    unsigned llvm_op_code;
    unsigned llvm_predicate;
    DEBUG_READ("{LLVM:");
    Read(size);
    Read(llvm_op_code);
    Read(llvm_predicate);
    auto op = circuit->Adopt<LLVMOperation>(id, llvm_op_code, llvm_predicate, size);
    Read(op->operands);
    DEBUG_READ("}");
    return op;
  }

#define DECODE_GENERIC(cls) \
  cls *Decode##cls(uint64_t id) { \
    unsigned size = 0; \
    DEBUG_READ("{size="); \
    Read(size); \
    auto op = circuit->Adopt<cls>(id, size); \
    DEBUG_READ(";operands="); \
    Read(op->operands); \
    DEBUG_READ("}"); \
    return op; \
  }

#define DECODE_CONDITION(cls) \
  cls *Decode##cls(uint64_t id) { \
    unsigned size = 0; \
    DEBUG_READ("{size="); \
    Read(size); \
    CHECK_EQ(size, 1); \
    auto op = circuit->Adopt<cls>(id); \
    DEBUG_READ(";operands="); \
    Read(op->operands); \
    DEBUG_READ("}"); \
    return op; \
  }

  DECODE_GENERIC(Undefined)
  DECODE_GENERIC(Not)
  DECODE_GENERIC(Concat)
  DECODE_GENERIC(PopulationCount)
  DECODE_CONDITION(Parity)
  DECODE_GENERIC(CountLeadingZeroes)
  DECODE_GENERIC(CountTrailingZeroes)
  DECODE_CONDITION(ReadMemoryCondition)
  DECODE_CONDITION(HintCondition)
  DECODE_CONDITION(RegisterCondition)
  DECODE_CONDITION(PreservedCondition)
  DECODE_CONDITION(CopyCondition)
  DECODE_GENERIC(InputInstructionBits)
  DECODE_CONDITION(DecodeCondition)
  DECODE_GENERIC(Hint)
  DECODE_CONDITION(VerifyInstruction)

  Operation *DecodeOnlyOneCondition(uint64_t) {
    LOG(FATAL) << "OnlyOneCondition nodes should not appear in serialized file";
    return nullptr;
  }

  std::istream &is;
  Circuit *const circuit;
  std::unordered_map<uint64_t, Operation *> id_to_op;
};

}  // namespace

void Circuit::Serialize(std::ostream &os) {
  SerializeVisitor vis(os);
  for (auto xor_all_op : Attr<OnlyOneCondition>()) {
    for (auto op : xor_all_op->operands) {
      vis.Write(op);
    }
  }
  os.flush();
}

void Circuit::Serialize(
    std::function<std::ostream &(const std::string &)> os_opener) {

  std::unordered_map<std::string, SerializeVisitor> visitors;

  std::string topology;
  for (auto xor_all_op : Attr<OnlyOneCondition>()) {

    // Each of `op` should be a single `VerifyInstruction` operation.
    for (auto op : xor_all_op->operands) {
      std::stringstream ss;
      PrintTopology(
          ss, op, ~0u, +[](Operation *) { return true; });
      ss.str().swap(topology);

      auto it = visitors.find(topology);
      if (it == visitors.end()) {
        bool added = false;
        std::tie(it, added) = visitors.emplace(topology, os_opener(topology));
        CHECK(added);
      }

      it->second.Write(op);
    }
  }
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
  for (auto in_reg : circuit->Attr<InputRegister>()) {
    in_regs.emplace(in_reg->reg_name, in_reg);
  }

  for (auto out_reg : circuit->Attr<OutputRegister>()) {
    out_regs.emplace(out_reg->reg_name, out_reg);
  }

  for (const auto &[name, op] : in_regs) {
    if (!out_regs.count(name)) {
      out_regs.emplace(name,
                       circuit->Create<OutputRegister>(op->size, op->reg_name));
    }
  }

  for (const auto &[name, op] : out_regs) {
    if (!in_regs.count(name)) {
      in_regs.emplace(name, circuit->Create<InputRegister>(op->size, op->reg_name));
    }
  }

  std::unordered_map<std::string, PreservedCondition *> preserved_regs;
  for (auto cond : circuit->Attr<PreservedCondition>()) {
    auto lhs = dynamic_cast<InputRegister *>(cond->operands[0]);
    auto rhs = dynamic_cast<OutputRegister *>(cond->operands[1]);
    CHECK_NOTNULL(lhs);
    CHECK_NOTNULL(rhs);
    CHECK_EQ(lhs->reg_name, rhs->reg_name);
    preserved_regs.emplace(lhs->reg_name, cond);
  }

  auto xor_all = circuit->Create<OnlyOneCondition>();
  circuit->operands.AddUse(xor_all);

  for (auto [_, op] : vis.id_to_op) {
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
          cond = circuit->Create<PreservedCondition>();
          cond->operands.AddUse(in_regs[name]);
          cond->operands.AddUse(out_regs[name]);
        }
        verify->operands.AddUse(cond);
      }
    }
  }

  // TODO(lukas): I think this is not supposed to be here.
  circuit->RemoveUnused();

  return circuit;
}

void Circuit::RemoveUnused(void) {
  uint64_t num_removed = 0;
  auto clear = [&](auto &field) {
    num_removed += field.RemoveUnused();
  };
  while (num_removed) {
    num_removed = 0;
    this->ForEachField(clear);
  }
}

}  // namespace circuitous
