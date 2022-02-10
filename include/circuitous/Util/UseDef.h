/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <iterator>
#include <memory>
#include <unordered_set>
#include <vector>

#include <circuitous/Support/Check.hpp>

namespace circ {

// Class that owns all the memory for nodes of type `Value`.
// Other classes should operate on raw pointers
// It is not strictly enforced, but it is expected `Value` inherits
// from `Node` class.
template<typename Value>
struct DefList {
  using storage_t = std::unordered_set<std::unique_ptr<Value>>;

  template<typename SI>
  struct It {
    using value_type = Value *;

    It(SI it_) : it(it_) {}

    auto &operator++() noexcept { ++it; return *this; }
    bool operator==(const It<SI> &other) const { return it == other.it; }
    bool operator!=(const It<SI> &other) const { return !(*this == other); }

    value_type operator*() const noexcept { return it->get(); }

    SI it;
  };

  using iterator = It<typename storage_t::iterator>;

  auto begin() { return iterator(defs.begin()); }
  auto  end() { return iterator(defs.end()); }

  using value_type = Value *;

  template<typename ...Args>
  auto Create(Args &&...args)
  -> std::enable_if_t< std::is_constructible_v< Value, Args ...>, Value* > {
    auto new_def = new Value(std::forward<Args>(args)...);
    defs.emplace(new_def);
    return new_def;
  }

  Value *Adopt(Value &&val) {
    auto [it, _] = defs.insert(std::make_unique<Value>(std::move(val)));
    return it->get();
  }

  auto Size() const { return defs.size(); }
  bool Empty() const noexcept { return defs.size() == 0; }

  value_type operator[](uint32_t idx) { return std::next(defs.begin(), idx)->get(); }

  // CB should have type `void()(std::uniqe_ptr<T> &&)` -- it will be given
  // ownership of the object.
  template<typename CB>
  std::size_t RemoveUnused(CB cb) {
    std::size_t num = 0;
    for (auto it = defs.begin(); it != defs.end();) {
      if ((*it)->users.size() == 0) {
        cb(std::move(*it));
        it = defs.erase(it);
        ++num;
      } else {
        ++it;
      }
    }
    return num;
  }

  storage_t defs;
};

template<typename T>
struct Use {
  T *user;
  T *use;
};

template<typename T>
struct UseList : std::vector<T *> {
  using impl = std::vector<T *>;

  auto Size() const { return this->size(); }
  auto Empty() const { return this->empty(); }
};

// Value can be used by others, `users` are used to travel up the data flow.
// `add(x, y)` -- `add` is in `users` of both `x` and `y`
template<typename T>
struct Value {
  UseList<T> users;

  void RemoveUser(T *user) {
    auto it = std::find(users.begin(), users.end(), user);
    check(it != users.end()) << "Trying to remove user that is not part of the list.";
    std::swap(users.back(), *it);
    users.pop_back();
  }

};

// add(x, y) -> `add` node uses `x` and `y`.
template<typename T>
struct User {
  UseList<T> operands;

  void RemoveOperand(T *op) {
    auto it = std::find(operands.begin(), operands.end(), op);
    check(it != operands.end()) << "Trying to remove operand that is not part of the list.";
    std::swap(operands.back(), *it);
    operands.pop_back();
  }
};

// The node can be both user and value.
// The values should always be in consistent state e.g. if `x in y.operands`, then
// `y in yx.users`.
template<typename T>
struct Node : Value<T>, User<T> {

  T *Raw() { return static_cast<T *>(this); }

  void AddUse(T *other) {
    this->operands.push_back(other);
    other->users.push_back(Raw());
  }

  void RemoveUse(T *parent) {
    parent->RemoveOperand(Raw());
    this->RemoveUser(parent);
  }

  void ReplaceUse(T *other, std::size_t at) {
    check(this->operands.size() > at);
    this->operands[at]->RemoveUser(Raw());

    this->operands[at] = other;
    other->users.push_back(Raw());
  }


  void ReplaceAllUsesWith(T *other) {
    check(other != this) << "Trying to replace all uses of X with X, probably error.";

    auto fetch = [&](const auto &where){
      for (std::size_t i = 0; i < where.size(); ++i) {
        if (where[i] == this) {
          return i;
        }
      }
      unreachable() << "User and uses are out of sync.";
    };

    for (auto user : this->users) {
      // NOTE(lukas): I actually do not expect these vectors to grow much.
      //              And given the cache friendliness of vector this should
      //              not be a bottle-neck.
      auto idx = fetch(user->operands);
      user->operands[idx] = other;

      other->users.push_back(user);
    }
    this->users.clear();
  }

  template<typename U, typename CB>
  void ForEachUse(CB cb) {
    std::vector<U *> frozen;
    for (auto user : this->users) {
      if (auto casted = dynamic_cast<U *>(user)) {
        frozen.push_back(casted);
      }
    }
    for (auto op : frozen) {
      cb(op);
    }
  }
};

}
