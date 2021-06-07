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

template<typename T, typename ...Ts>
bool IsOneOf(Operation *op) {
  if (op->op_code == T::kind) {
    return true;
  }

  if constexpr (sizeof...(Ts) == 0) {
    return false;
  } else {
    return IsOneOf<Ts...>(op);
  }
}

static inline bool IsLeafNode(Operation *op) {
  switch(op->op_code) {
    case InputRegister::kind:
    case OutputRegister::kind:
    case Constant::kind:
    case Hint::kind:
    case Undefined::kind:
    case InputInstructionBits::kind:
      return true;
    default:
      return false;
  }
}

template<typename T>
bool Is(Operation *op) {
  return op->op_code == T::kind;
}

// Return number of HINT users that
static inline std::size_t HintUsers(Operation *hint) {
  CHECK(Is<Hint>(hint));
  std::size_t users = 0;
  for (auto user : hint->users) {
    if (!Is<HintCondition>(user)) {
      ++users;
    }
  }
  return users;
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



namespace print {
  template<typename Derived>
  struct Topology {
    std::stringstream ss;
    using hash_t = std::string;

    Derived &Self() { return static_cast<Derived &>(*this); }

    template<typename C>
    std::string Hash(const C& ops) {
      std::stringstream ss;
      for (auto op : ops) {
        ss << Hash(op);
        ss << " | ";
      }
      return ss.str();
    }

    std::string Hash(Operation *op) {
      return Self().Print(op, 0);
    }

    std::string Children(Operation *op, uint8_t depth) {
      std::string out;
      for (auto o : op->operands) {
        out += Self().Print(o, depth + 1);
        out += Self().separator;
      }
      return out;
    }

    std::string Print(Operation *op) {
      return Self().Print(op, 0);
    }

    std::string Print(Operation *op, uint8_t depth) {
      auto indent = Self().Indent(depth);
      std::string out;
      out += indent;
      out += Self().Op(op);
      out += "( ";
      out += Self().Children(op, depth);
      out += indent + ")";
      return out;
    }

    std::string Get() { return ss.str(); }
    std::string Indent(uint8_t) { return {}; }
  };

  template<typename Next>
  struct WithCache : Next {
    using parent_t = Next;
    using hash_t = typename parent_t::hash_t;

    using Next::Hash;

    std::unordered_map<Operation *, hash_t > op_to_hash;

    std::string Print(Operation *op, uint8_t depth) {
      auto it = op_to_hash.find(op);
      if (it != op_to_hash.end()) {
        return it->second;
      }
      auto x = this->parent_t::Print(op, depth + 1);
      op_to_hash[op] = x;
      return x;
    }
  };

  template<typename Next>
  struct FullNames_ : Next {
    static inline constexpr const char separator = ' ';
    std::string Op(Operation *op) { return op->Name(); }
  };

  struct FullNames : FullNames_<WithCache<Topology<FullNames>>> {};

  struct PrettyPrinter : FullNames {
    std::string Indent(uint8_t depth) {
      return std::string(depth * 2, ' ');
    }
  };

} // namespace print


namespace collect {
  struct Ctxs {
    using ctxs_t = std::unordered_set<Operation *>;
    using ctxs_map_t = std::unordered_map<Operation *, ctxs_t>;

    ctxs_map_t op_to_ctxs;

    void Root(Operation *op) {
      op_to_ctxs[op] = { op };
    }

    void Update(Operation *node, Operation *user) {
      if (user) {
        auto &ctxs = op_to_ctxs[node];
        auto &user_ctxs = op_to_ctxs[user];
        ctxs.insert(user_ctxs.begin(), user_ctxs.end());
      }
    }
  };

  struct Hashes : print::FullNames {
    void Root(Operation *op) {
      Hash(op);
    }

    void Update(Operation *node, Operation *user) {
      CHECK(op_to_hash.count(node));
    }
  };


  template< typename ...Ts >
  struct UpTree {
    std::unordered_set<Operation *> collected;

    void Run(Operation *op) {
      if (IsOneOf<Ts...>(op)) {
        collected.insert(op);
      }
      for (auto o : op->users) {
        Run(o);
      }
    }
  };
} // namespace collect

template<typename ...Collectors>
struct Collector : Collectors ... {
  using self_t = Collector<Collectors...>;

  using entry_t = std::pair<Operation *, Operation *>;
  std::deque<entry_t> todo;

  self_t &Run(Circuit *circuit) {
    for (auto x : circuit->Attr<VerifyInstruction>()) {
      (Collectors::Root(x), ...);
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
    (Collectors::Update(node, user), ...);
    for (auto op : node->operands) {
      todo.emplace_back(op, node);
    }
  }
};

using CtxCollector = Collector<collect::Ctxs>;

static inline Operation *GetContext(Operation *op) {
  collect::UpTree<VerifyInstruction> collector;
  collector.Run(op);
  auto &ctxs = collector.collected;
  CHECK(ctxs.size() == 1);
  return *(ctxs.begin());
}

static inline std::unordered_set<Operation *> GetContexts(Operation *op) {
  collect::UpTree<VerifyInstruction> collector;
  collector.Run(op);
  return collector.collected;
}

} // namespace circuitous

