/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma pragma once

#include <circuitous/IR/IR.h>

#include <deque>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>

namespace circuitous {

bool IsLeaf(Operation *op) {
  switch(op->op_code) {
    case Operation::kInputRegister:
    case Operation::kOutputRegister:
    case Operation::kConstant:
    case Operation::kHint:
    case Operation::kInputInstructionBits:
      return true;
    default:
      return false;
  }
}

using operation_set_t = std::unordered_set<Operation *>;

template<typename T>
struct SubtreeCollector {
  std::unordered_multiset<T *> collected;

  template<typename C>
  SubtreeCollector<T> &Run(const C &ops) {
    for (auto op : ops) {
      Run(op);
    }
    return *this;
  }

  SubtreeCollector<T> &Run(Operation *o) {
    if (o->op_code == T::kind) {
      collected.insert(dynamic_cast<T *>(o));
    }
    for (auto op : o->operands) {
      Run(op);
    }
    return *this;
  }

  template<typename CB>
  auto Apply(CB cb) {
    using res_t = decltype(cb(*collected.begin()));
    std::vector<res_t> out;
    for (auto x : collected) {
      out.push_back(cb(x));
    }
    return out;
  }
};

struct CtxCollector {
  using ctxs_t = std::unordered_set<Operation *>;
  using ctxs_map_t = std::unordered_map<Operation *, ctxs_t>;

  ctxs_map_t op_to_ctxs;

  using entry_t = std::pair<Operation *, Operation *>;
  std::deque<entry_t> todo;

  CtxCollector &Run(Circuit *circuit) {
    for (auto x : circuit->Attr<VerifyInstruction>()) {
      op_to_ctxs[x] = {x};
      todo.push_back({x, nullptr});
    }

    while (!todo.empty()) {
      const auto &[x, y] = todo.front();
      todo.pop_front();
      Update(x, y);
    }
    return *this;
  }

  void Update(Operation *node, Operation *user) {
    if (user) {
      auto &ctxs = op_to_ctxs[node];
      auto &user_ctxs = op_to_ctxs[user];
      ctxs.insert(user_ctxs.begin(), user_ctxs.end());
    }
    for (auto op : node->operands) {
      todo.emplace_back(op, node);
    }
  }

  ctxs_map_t Get() { return op_to_ctxs; }
};

namespace cmp {

struct StrictComparator {
  template<typename T>
  auto ForwardAs(Operation *lhs, Operation *rhs) {
    return Compare(dynamic_cast<T *>(lhs), dynamic_cast<T *>(rhs));
  }

  bool Compare(Operation *lhs, Operation *rhs) {
    return lhs->Equals(rhs);
    if (lhs->op_code != rhs->op_code || lhs->operands.Size() != rhs->operands.Size()) {
      return false;
    }
    switch (lhs->op_code) {
      case Operation::kConstant:
        return ForwardAs<Constant>(lhs, rhs);
      case Operation::kInputRegister:
        return ForwardAs<InputRegister>(lhs, rhs);
      case Operation::kOutputRegister:
        return ForwardAs<OutputRegister>(lhs, rhs);
      case Operation::kLLVMOperation:
        if (!ForwardAs<LLVMOperation>(lhs, rhs)) return false;
      case Operation::kExtract:
        if (!ForwardAs<Extract>(lhs, rhs)) return false;
      case Operation::kHint:
        if (!ForwardAs<Hint>(lhs, rhs)) return false;
      default:
        break;
    }
    for (auto i = 0ull; i < lhs->operands.Size(); ++i) {
      if (!Compare(lhs->operands[i], rhs->operands[i])) {
        return false;
      }
    }
    return true;
  }


  bool Compare(Constant *a, Constant *b) { return a->bits == b->bits; }
  bool Compare(InputRegister *a, InputRegister *b) { return a->reg_name == b->reg_name; }
  bool Compare(LLVMOperation *a, LLVMOperation *b) { return false; }
  bool Compare(Extract *a, Extract *b) { return false; }
};

bool StrictStructural(Operation *rhs, Operation *lhs) {
  return false;
}

} // namespace cmp

namespace print {

template<typename Derived>
struct Topology {
  std::stringstream ss;

  Derived &Self() { return static_cast<Derived &>(*this); }

  template<typename C>
  std::string Hash(const C& ops) {
    for (auto op : ops) {
      Run(op);
    }
    return Get();
  }

  void Print(Operation *op, uint8_t depth) {
    auto indent = Self().Indent(depth);
    ss << indent << Self().Op(op);
    ss << "( ";
    for (auto o : op->operands) {
      Print(o, depth + 1);
      ss << Self().separator;
    }
    ss << indent << ")";
  }

  void Run(Operation *op) {
    Print(op, 0);
    ss << "|";
  }

  std::string Get() { return ss.str(); }

  std::string Indent(uint8_t) { return {}; }
};

struct FullNames : Topology<FullNames> {
  static inline constexpr const char separator = ' ';
  std::string Op(Operation *op) { return op->Name(); }
};

struct PrettyPrinter : FullNames {
  std::string Indent(uint8_t depth) {
    return std::string(depth * 2, ' ');
  }
};

} // namespace print

} // namespace circuitous

