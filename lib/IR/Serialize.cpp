/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Hash.h>
#include <circuitous/IR/Circuit.hpp>
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

namespace circ {
namespace {

struct FileConfig {
  enum class Selector : uint8_t {
    Operation = 0x0,
    Invalid = 0x1,
    Reference = 0xff
  };

  using raw_op_code_t = uint32_t;
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
  ~SerializeVisitor() {
    os.flush();
  }

  explicit SerializeVisitor(std::ostream &os_) : os(os_) {}

  void Write(Selector sel) {
    Write(static_cast<std::underlying_type_t<Selector>>(sel));
  }

  void Write(Operation *op) {
    if (!written.count(op->id())) {
      Write(Selector::Operation);
      raw_id_t id = hasher[op];
      Write(id);
      CHECK_LE(0u, op->op_code);
      raw_op_code_t op_code = op->op_code;
      Write(op_code);
      written.insert(op->id());
      this->Dispatch(op);
    } else {
      Write(Selector::Reference);
      Write<raw_id_t>(op->id());
    }
  }

  void Write(uint8_t byte) {
    os << byte;
  }

  void Write(int8_t byte) {
    os << static_cast<uint8_t>(byte);
  }

  template <typename T>
  void Write(const UseList<T> &elems) {
    Write<uint32_t>(static_cast<uint32_t>(elems.size()));
    for (const auto elem : elems) {
      Write(elem);
    }
  }

  void Write(const std::string &str) {
    Write<uint32_t>(static_cast<uint32_t>(str.size()));
    for (auto ch : str) {
      Write(ch);
    }
  }

  template <typename T>
  void Write(const T &data) {
    auto bytes = reinterpret_cast<const uint8_t *>(&data);
    for (auto i = 0ull; i < sizeof(data); ++i) {
      Write(static_cast<uint8_t>(bytes[i]));
    }
  }

  template<typename ...Args>
  void write(Args &&... args) {
    (Write(std::forward<Args>(args)), ...);
  }

  void Visit(InputRegister *op) { write(op->reg_name, op->size); }
  void Visit(OutputRegister *op) { write(op->reg_name, op->size); }

  void Visit(Operation *op) { write(op->size, op->operands); }
  void Visit(Constant *op) { write(op->size, op->bits); }
  void Visit(InputImmediate *op) { write(op->size, op->operands); }

  void Visit(Extract *op) { write(op->high_bit_exc, op->low_bit_inc, op->operands[0]); }
  void Visit(Select *op) { write(op->size, op->bits, op->operands); }
  void Visit(Memory *op) { write(op->size, op->mem_idx); }
  void Visit(InputErrorFlag *op) {}
  void Visit(OutputErrorFlag *op) {}
  void Visit(InputTimestamp *op) {}
  void Visit(OutputTimestamp *op) {}

 private:
  std::ostream &os;
  IdentityHasher hasher;
  std::unordered_set<uint64_t> written;
};


namespace detail {

  template< typename D, typename T >
  struct inject {
    Operation *Visit(T *, uint64_t id) {
      auto &self = static_cast< D & >( *this );
      return self.template with_ops< T >(id, self.template read<unsigned>());
    }
  };

  template< typename D, typename ... Ts >
  struct unfolder {};

  template< typename D, typename T >
  struct unfolder< D, T > : inject< D, T > { using inject< D, T >::Visit; };

  template< typename D, typename T, typename ...Ts >
  struct unfolder< D, T, Ts... > : inject< D, T >, unfolder<D, Ts... >
  {
    using inject< D, T >::Visit;
    using unfolder< D, Ts... >::Visit;
  };

  template< typename D, typename L > struct inject_visitors {};
  template< typename D, typename ... Ts >
  struct inject_visitors< D, tl::TL< Ts ... > > : unfolder< D, Ts ... > {};

} // namespace detail

// Inject deserializers of former llvm-operation
template< typename D >
using DeserializeComputational = detail::inject_visitors< D, llvm_ops_t >;

struct DeserializeVisitor : FileConfig, DVisitor< DeserializeVisitor >,
                            DeserializeComputational< DeserializeVisitor >
{
  using Selector = FileConfig::Selector;

  using DeserializeComputational< DeserializeVisitor >::Visit;

  explicit DeserializeVisitor(std::istream &is_, Circuit *circuit_)
      : is(is_),
        circuit(circuit_)
  {}

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

  void Read(std::string &str) {
    uint32_t size = 0u;
    Read(size);
    str.resize(size);
    for (auto i = 0u; i < size; ++i) {
      Read(str[i]);
    }
  }

  template <typename T>
  void Read(T &data) {
    auto bytes = reinterpret_cast<uint8_t *>(&data);
    for (auto i = 0ull; i < sizeof(data); ++i) {
      Read(bytes[i]);
    }
  }

  void Read(int8_t &byte) {
    uint8_t b;
    Read(b);
    byte = static_cast<int8_t>(b);
  }

  template<typename ...Args>
  std::tuple<Args ...> read() {
    std::tuple< Args ... > out;

    auto read_ = [&](Args &... args) { (Read(args), ... ); };
    std::apply(read_, out);
    return out;
  }


  void ReadOps(Operation *elems) {
    auto [size] = read<uint32_t>();
    for (auto i = 0u; i < size; ++i) {
      elems->AddUse(Read());
    }
  }

  Operation *Read() {
    auto [sel] = read< Selector >();

    if (sel == Selector::Operation) {
      auto [hash, op_code] = read< raw_id_t, raw_op_code_t >();

      auto op = Decode(hash, op_code);
      id_to_op[hash] = op;
      return op;

    }
    if (sel == Selector::Reference) {
      auto [hash] = read< raw_id_t >();

      auto op_it = id_to_op.find(hash);
      if (op_it == id_to_op.end()) {
        LOG(FATAL) << "Could not reference with id: " << hash;
      }
      return op_it->second;
    }
    LOG(FATAL) << "Unexpected tag for an operation reference: " << this->to_string(sel);
  }

  template< typename T >
  Operation *Visit(T *op, uint64_t id) {
    LOG(FATAL) << "Cannot deserialize " << T::kind << ". Most likely cause is missing impl.";
  }

  Operation *Decode(raw_id_t id, raw_op_code_t op_code) {
    return this->Dispatch(op_code, id);
  }

  template<typename T, typename ...Args>
  auto make_op(uint64_t id, std::tuple< Args... > &&args) {
    auto make = [&](Args &&... args) {
      return circuit->Adopt<T>(id, std::forward<Args>(args)... );
    };
    return std::apply(make, std::forward<std::tuple<Args ...>>(args));
  }

  template<typename T, typename U>
  auto with_ops(uint64_t id, U &&u) {
    auto self = make_op<T>(id, std::forward<U>(u));
    ReadOps(self);
    return self;
  }

  template<typename T>
  auto reg_like(uint64_t id) {
    return make_op<T>(id, read<unsigned, std::string>());
  }

  template<typename T>
  auto reg_like_(uint64_t id) {
    return make_op<T>(id, read<std::string, unsigned>());
  }

  Operation *Visit(InputRegister *, uint64_t id) {
    return reg_like_<InputRegister>(id);
  }
  Operation *Visit(OutputRegister *, uint64_t id) {
    return reg_like_<OutputRegister>(id);
  }

  Operation *Visit(InputImmediate *, uint64_t id) {
    auto op = make_op<InputImmediate>(id, read<unsigned>());
    ReadOps(op);
    return op;
  }

  Operation *Visit(Memory *, uint64_t id) {
    auto [size, mem_id] = read<unsigned, unsigned>();
    CHECK(size == Memory::default_size);
    return circuit->Adopt<Memory>(id, mem_id);
  }

  Operation *Visit(Constant *, uint64_t id) {
    auto [size, bits] = read<unsigned, std::string>();
    return circuit->Adopt<Constant>(id, std::move(bits), size);
  }

  Operation *Visit(Extract *, uint64_t id) {
    auto [high, low] = read<unsigned, unsigned>();
    auto op = circuit->Adopt<Extract>(id, low, high);
    op->AddUse(Read());
    return op;
  }

  Operation *Visit(Select *, uint64_t id) {
    auto [size, bits] = read<unsigned, unsigned>();
    auto op = circuit->Adopt<Select>(id, bits, size);
    ReadOps(op);
    return op;
  }

  Operation *Visit(InputErrorFlag *, uint64_t id) {
    return circuit->Adopt<InputErrorFlag>(id);
  }
  Operation *Visit(OutputErrorFlag *, uint64_t id) {
    return circuit->Adopt<OutputErrorFlag>(id);
  }
  Operation *Visit(InputTimestamp *, uint64_t id) {
    return circuit->Adopt<InputTimestamp>(id);
  }
  Operation *Visit(OutputTimestamp *, uint64_t id) {
    return circuit->Adopt<OutputTimestamp>(id);
  }

  template<typename T>
  auto make_generic(uint64_t id) { return with_ops<T>(id, read<unsigned>()); }

  template<typename T>
  auto make_condition(uint64_t id) {
    auto [size] = read<unsigned>();
    CHECK(size == 1);
    auto self = circuit->Adopt<T>(id);
    ReadOps(self);
    return self;
  }

  #define DECODE_GENERIC(cls) \
  Operation *Visit(cls *, uint64_t id) { return make_generic<cls>(id); }

  #define DECODE_CONDITION(cls) \
  Operation *Visit(cls *, uint64_t id) {return make_condition<cls>(id); }

  DECODE_GENERIC(Undefined)
  DECODE_GENERIC(Not)
  DECODE_GENERIC(Concat)
  DECODE_GENERIC(PopulationCount)
  DECODE_CONDITION(Parity)
  DECODE_GENERIC(CountLeadingZeroes)
  DECODE_GENERIC(CountTrailingZeroes)

  DECODE_CONDITION(AdviceConstraint)
  DECODE_CONDITION(RegConstraint)
  DECODE_CONDITION(PreservedConstraint)
  DECODE_CONDITION(CopyConstraint)
  DECODE_CONDITION(ReadConstraint)
  DECODE_CONDITION(WriteConstraint)

  DECODE_GENERIC(InputInstructionBits)
  DECODE_CONDITION(DecodeCondition)
  DECODE_GENERIC(Advice)
  DECODE_CONDITION(Or)
  DECODE_CONDITION(And)
  DECODE_CONDITION(VerifyInstruction)
  DECODE_CONDITION(OnlyOneCondition)

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

void Circuit::Serialize(std::function<std::ostream &(const std::string &)> os_opener) {
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
    auto op = vis.Read();
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
                       circuit->Create<OutputRegister>(op->reg_name , op->size));
    }
  }

  for (const auto &[name, op] : out_regs) {
    if (!in_regs.count(name)) {
      in_regs.emplace(name, circuit->Create<InputRegister>(op->reg_name, op->size));
    }
  }

  std::unordered_map<std::string, PreservedConstraint *> preserved_regs;
  for (auto cond : circuit->Attr<PreservedConstraint>()) {
    auto lhs = dynamic_cast<InputRegister *>(cond->operands[0]);
    auto rhs = dynamic_cast<OutputRegister *>(cond->operands[1]);
    CHECK_NOTNULL(lhs);
    CHECK_NOTNULL(rhs);
    CHECK_EQ(lhs->reg_name, rhs->reg_name);
    preserved_regs.emplace(lhs->reg_name, cond);
  }

  auto xor_all = circuit->Create<OnlyOneCondition>();
  circuit->AddUse(xor_all);

  for (auto [_, op] : vis.id_to_op) {
    auto verify = dynamic_cast<VerifyInstruction *>(op);
    if (!verify) {
      continue;
    }

    xor_all->AddUse(verify);

    seen_reg_names.clear();
    for (auto op : verify->operands) {
      OutputRegister *out_reg = nullptr;

      if (auto transition = dynamic_cast<RegConstraint *>(op); transition) {
        out_reg = dynamic_cast<OutputRegister *>(transition->operands[1]);

      } else if (auto preserved = dynamic_cast<PreservedConstraint *>(op);
                 preserved) {
        out_reg = dynamic_cast<OutputRegister *>(preserved->operands[1]);

      } else if (auto copied = dynamic_cast<CopyConstraint *>(op); copied) {
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
          cond = circuit->Create<PreservedConstraint>();
          cond->AddUse(in_regs[name]);
          cond->AddUse(out_regs[name]);
        }
        verify->AddUse(cond);
      }
    }
  }

  // TODO(lukas): I think this is not supposed to be here.
  circuit->RemoveUnused();

  return circuit;
}

}  // namespace circ
