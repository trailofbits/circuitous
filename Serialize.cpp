/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include "IR.h"

#include <glog/logging.h>

#include <cstdint>
#include <iostream>
#include <unordered_map>

#include "Hash.h"

namespace circuitous {
namespace {

class SerializeVisitor : public Visitor<SerializeVisitor> {
 public:
  explicit SerializeVisitor(std::ostream &os_)
      : os(os_),
        hasher() {}

  void Write(Operation *op) {
    auto offset_it = offset.find(op);
    if (offset_it == offset.end()) {
      Write<uint8_t>(0);
      Write(hasher[op]);
      Write(op->op_code);
      offset.emplace(op, curr_offset);
      this->Visit(op);
    } else {
      Write<uint8_t>(0xff);
      Write<int32_t>(curr_offset - offset_it->second);
    }
  }
  void Write(uint8_t byte) {
    os << byte;
    curr_offset += 1;
  }

  void Write(int8_t byte) {
    os << byte;
    curr_offset += 1;
  }

  template <typename T>
  void Write(const std::vector<T> &elems) {
    Write<uint32_t>(static_cast<uint32_t>(elems.size()));
    for (const auto &elem : elems) {
      Write(elem);
    }
  }

  template <typename T>
  void Write(const UseList<T> &elems) {
    Write<uint32_t>(static_cast<uint32_t>(elems.Size()));
    for (const auto &elem : elems) {
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
      Write<uint8_t>(bytes[i]);
    }
  }

  void VisitOperation(Operation *op) {
    Write(op->size);
    Write(op->operands);
  }

  void VisitConstant(Constant *op) {
    Write(op->size);
    Write(op->bits);
  }

  void VisitInputRegister(InputRegister *op) {
    Write(op->size);
    Write(op->reg_name);
  }

  void VisitOutputRegister(OutputRegister *op) {
    Write(op->size);
    Write(op->reg_name);
  }

  void VisitExtract(Extract *op) {
    Write(op->size);
    Write(op->high_hit_exc);
    Write(op->low_bit_inc);
    Write(op->operands[0]);
  }

 private:
  std::ostream &os;
  int32_t curr_offset{0};
  std::unordered_map<Operation *, int32_t> offset;
  HashVisitor hasher;
};


class DeserializeVisitor : public Visitor<SerializeVisitor> {
 public:
  explicit DeserializeVisitor(std::istream &is_, Circuit *circuit_)
      : is(is_),
        circuit(circuit_),
        hasher() {}

  void Read(Operation *&op) {
    uint8_t sel = 0;
    Read(sel);
    if (sel == 0u) {
      uint64_t hash = 0;
      Read(hash);
      auto prev_offset = curr_offset;


    } else if (sel == 0xffu) {
      int32_t disp = 0;
      auto prev_offset = curr_offset;
      Read(disp);
      auto op_offset = prev_offset + disp;

    } else {
      LOG(FATAL)
          << "Unexpected tag for an operation reference: " << sel;
    }

    auto offset_it = offset.find(op);
    if (offset_it == offset.end()) {
      Write<uint8_t>(0);
      Write(hasher[op]);
      offset.emplace(op, curr_offset);
      this->Visit(op);
    } else {
      Write<uint8_t>(0xff);
      Write(hasher[op]);
      Write<int32_t>(curr_offset - offset_it->second);
    }
  }
  void Read(uint8_t &byte) {
    is >> byte;
    curr_offset += 1;
  }

  void Read(int8_t &byte) {
    is >> byte;
    curr_offset += 1;
  }

  template <typename T>
  void Read(std::vector<T> &elems) {
    uint32_t size = 0u;
    Read(size);
    elems.resize(size);
    for (auto i = 0u; i < size; ++i) {
      Read(elems[i]);
    }
  }

  template <typename T>
  void Read(UseList<T> &elems) {
    uint32_t size = 0u;
    Read(size);
    for (auto i = 0u; i < size; ++i) {
      T *ref = nullptr;
      Read(ref);
      elems.AddUse(ref);
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

  void VisitOperation(Operation *op) {
    Write(op->op_code);
    Write(op->size);
    Write(op->operands);
  }

  void VisitConstant(Constant *op) {
    Write(op->op_code);
    Write(op->size);
    Write(op->bits);
  }

  void VisitInputRegister(InputRegister *op) {
    Write(op->op_code);
    Write(op->size);
    Write(op->reg_name);
  }

  void VisitOutputRegister(OutputRegister *op) {
    Write(op->op_code);
    Write(op->size);
    Write(op->reg_name);
  }

  void VisitExtract(Extract *op) {
    Write(op->op_code);
    Write(op->size);
    Write(op->high_hit_exc);
    Write(op->low_bit_inc);
    Write(op->operands[0]);
  }

 private:
  std::ostream &is;
  Circuit * const circuit;
  int32_t curr_offset{0};
  std::unordered_map<int32_t, Operation *> offset_to_op;
  std::unordered_multimap<uint64_t, Operation *> has_to_ops;
  HashVisitor hasher;
};

}  // namespace

void Serialize(std::ostream &os, Circuit *circuit) {
  SerializeVisitor vis(os);
  vis.Visit(circuit);
  os.flush();
}

std::unique_ptr<Circuit> Deserialize(std::istream &is) {
  return nullptr;
}

}  // namespace circuitous
