/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-compare"
#pragma clang diagnostic ignored "-Wconversion"
#include <glog/logging.h>
#pragma clang diagnostic pop

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <iterator>
#include <memory>
#include <vector>

namespace circuitous {


class User {
 public:
  template <typename T>
  explicit User(T *) {}

  virtual ~User(void);

  // NOTE(pag): It is forbidden to remove/replace uses within an `Update`.
  virtual void Update(uint64_t next_timestamp);

  static uint64_t gNextTimestamp;

 protected:
  User(void) = delete;

  uint64_t timestamp{0u};
};

template <typename T>
class Use;

template <typename T>
class UseRef;

template <typename T>
class WeakUseRef;

template <typename T>
class Def;

template <typename T>
class UseList;

template <typename T>
class UseListIterator;

template <typename T>
class Use {
 public:
#ifndef NDEBUG
  inline ~Use(void) {
    def_being_used = nullptr;
    user = nullptr;
    index = 0u;
  }
#endif

  T *get(void) const noexcept {
    return def_being_used;
  }

  T *operator->(void) const noexcept {
    return def_being_used;
  }

  T *operator*(void) const noexcept {
    return *def_being_used;
  }

 private:
  template <typename>
  friend class UseRef;

  template <typename>
  friend class WeakUseRef;

  template <typename>
  friend class UseList;

  template <typename>
  friend class UseListIterator;

  template <typename>
  friend class Def;

  User *user{nullptr};
  T *def_being_used{nullptr};
  unsigned index{0u};
};

// An iterator over a use list.
template <typename T>
class UseListIterator {
 public:
  inline UseListIterator(const Use<T> *const *it_) noexcept : it(it_) {}

  inline UseListIterator<T> &operator++(void) noexcept {
    ++it;
    return *this;
  }

  inline UseListIterator<T> operator++(int) noexcept {
    return it++;
  }

  inline bool operator==(UseListIterator<T> that) const noexcept {
    return it == that.it;
  }

  inline bool operator!=(UseListIterator<T> that) const noexcept {
    return it != that.it;
  }

  inline bool operator<(UseListIterator<T> that) const noexcept {
    return it < that.it;
  }

  inline T *operator*(void) const noexcept {
    return (*it)->def_being_used;
  }

  inline T *operator[](size_t index) const noexcept {
    return it[index]->def_being_used;
  }

  inline unsigned operator-(const UseListIterator<T> &that) const noexcept {
    return static_cast<unsigned>(it - that.it);
  }

  inline UseListIterator<T> operator+(unsigned disp) const noexcept {
    return UseListIterator<T>(&(it[disp]));
  }

 private:
  const Use<T> *const *it;
};

// A list of uses.
template <typename T>
class UseList {
 public:
  UseList(UseList<T> &&that) noexcept
      : owner(that.owner),
        uses(std::move(that.uses)),
        is_weak(that.is_weak) {}

  UseList(User *owner_) : owner(owner_), is_weak(false) {}

  UseList(User *owner_, bool is_weak_) : owner(owner_), is_weak(is_weak_) {}

  ~UseList(void) {
    Clear();
  }

  bool operator==(const UseList<T> &that) const {
    return uses == that.uses;
  }

  bool operator!=(const UseList<T> &that) const {
    return uses != that.uses;
  }

  inline T *operator[](size_t index) const {
    return uses[index]->def_being_used;
  }

  UseListIterator<T> begin(void) const {
    return uses.data();
  }

  UseListIterator<T> end(void) const {
    return &(uses.data()[uses.size()]);
  }

  void AddUse(Def<T> *def);

  unsigned Size(void) const noexcept {
    return static_cast<unsigned>(uses.size());
  }

  bool Empty(void) const noexcept {
    return uses.empty();
  }

  void Sort(void) noexcept {
    std::sort(uses.begin(), uses.end(), OrderUses);
    Reindex();
  }

  template <typename CB>
  void RemoveIf(CB cb) noexcept {
    for (auto &use : uses) {
      if (use) {
        auto def = use->get();
        if (cb(def)) {
          DeleteUse(use);
          use = nullptr;
        }
      }
    }

    auto it = std::remove_if(uses.begin(), uses.end(),
                             [](Use<T> *use) { return !use; });

    uses.erase(it, uses.end());
  }

  void RemoveNull(void) noexcept {
    RemoveIf([](T *def) { return !def; });
  }

  void Unique(void) noexcept {
    RemoveNull();
    std::sort(uses.begin(), uses.end(), OrderUses);

    for (size_t i = 1u, j = 0u, max_i = uses.size(); i < max_i; ++i) {
      auto &use = uses[i];
      if (UsesEqual(uses[j], use)) {
        DeleteUse(use);
        use = nullptr;
      } else {
        j = i;
      }
    }

    RemoveNull();
    std::sort(uses.begin(), uses.end(),
              [](Use<T> *a, Use<T> *b) { return a->index < b->index; });
    Reindex();
  }

  void ClearWithoutErasure(void) {
    if (is_weak) {
      Clear();
    } else {
      uses.clear();
    }
  }

  void Swap(UseList<T> &that) {
    assert(is_weak == that.is_weak);

    const auto t = User::gNextTimestamp++;
    if (owner == that.owner) {
      uses.swap(that.uses);
      owner->Update(t);

    } else {
      for (Use<T> *use : uses) {
        if (use) {
          assert(use->user == owner);
          use->user = that.owner;
        }
      }
      for (Use<T> *that_use : that.uses) {
        if (that_use) {
          assert(that_use->user == that.owner);
          that_use->user = owner;
        }
      }

      uses.swap(that.uses);
      owner->Update(t);
      that.owner->Update(t);
    }
  }

  void Clear(void);

 private:
  void DeleteUse(Use<T> *use) {
    const auto def = use->def_being_used;
    if (def) {
      if (is_weak) {
        def->EraseWeakUse(use);
        delete use;
      } else {
        def->EraseUse(use);
      }

    } else if (is_weak) {
      delete use;

    // The def was deleted before all of its uses were deleted.
    } else {
      assert(use->index == ~0u);
      delete use;
    }
  }

  void Reindex(void) noexcept {
    auto i = 0u;
    for (auto use : uses) {
      if (use) {
        use->index = i;
      }
      ++i;
    }
  }

  static bool OrderUses(Use<T> *a, Use<T> *b) {
    if (a && b) {
      const auto a_def = a->get();
      const auto b_def = b->get();
      if (a_def == b_def) {
        return false;
      } else {
        return a_def->Sort() < b_def->Sort();
      }
    } else {
      return a < b;
    }
  }

  static bool UsesEqual(Use<T> *a, Use<T> *b) {
    if (a == b) {
      return true;
    } else if (a && b) {
      return a->get() == b->get();
    } else {
      return false;
    }
  }

  User *owner;
  std::vector<Use<T> *> uses;

  // NOTE(pag): If this is a weak use list, then the list itself owns the memory
  //            of the uses, not the user.
  const bool is_weak;
};

template <typename T>
class WeakUseList : public UseList<T> {
 public:
  WeakUseList(WeakUseList<T> &&that) noexcept : UseList<T>(that) {}

  WeakUseList(User *owner_) : UseList<T>(owner_, true /* is_weak */) {}

  void Swap(WeakUseList<T> &that) {
    this->UseList<T>::Swap(that);
  }

 private:
  void Swap(UseList<T> &) {
    __builtin_unreachable();
  }
};

template <typename T>
class DefList;

// An iterator over a definition list.
template <typename T>
class DefListIterator {
 public:
  inline DefListIterator(const std::unique_ptr<T> *it_) noexcept : it(it_) {}

  inline DefListIterator<T> &operator++(void) noexcept {
    ++it;
    return *this;
  }

  inline DefListIterator<T> operator++(int) noexcept {
    return it++;
  }

  inline bool operator==(DefListIterator<T> that) const noexcept {
    return it == that.it;
  }

  inline bool operator!=(DefListIterator<T> that) const noexcept {
    return it != that.it;
  }

  inline bool operator<(DefListIterator<T> that) const noexcept {
    return it < that.it;
  }

  inline T *operator*(void) const noexcept {
    return it->get();
  }

  inline T *operator[](size_t index) const noexcept {
    return it[index].get();
  }

  inline unsigned operator-(const DefListIterator<T> &that) const noexcept {
    return static_cast<unsigned>(it - that.it);
  }

  inline DefListIterator<T> operator-(unsigned sub) const noexcept {
    return DefListIterator<T>(&(it[-static_cast<int>(sub)]));
  }

  inline DefListIterator<T> operator+(unsigned add) const noexcept {
    return DefListIterator<T>(&(it[add]));
  }

 private:
  const std::unique_ptr<T> *it;
};

// CRTP class for definitions of `T`. `Use<T>::def_being_used` should point to
// a `T`, which is derived from `Def<T>`.
template <typename T>
class Def {
 public:
  explicit Def(T *self_) : self(self_) {}

  ~Def(void) {
    for (Use<T> *use : weak_uses) {
      assert(use->def_being_used == self);
      use->user = nullptr;
      use->def_being_used = nullptr;
    }
    weak_uses.clear();

    // If we delete a def and it still has uses, then we don't delete
    // the uses, and instead let the use lists delete them when they go out
    // of scope.
    std::vector<std::unique_ptr<Use<T>>> our_uses;
    our_uses.swap(uses);
    for (auto &use : our_uses) {
      assert(use->def_being_used == self);
      use->user = nullptr;
      use->def_being_used = nullptr;
      use->index = ~0u;
      use.release();
    }
  }

  Use<T> *CreateUse(User *user) {
    const auto use = new Use<T>;
    user->Update(User::gNextTimestamp++);
    use->def_being_used = this->self;
    use->user = user;
    uses.emplace_back(use);
    return use;
  }

  Use<T> *CreateWeakUse(User *user) {
    const auto use = new Use<T>;
    use->def_being_used = this->self;
    use->user = user;
    weak_uses.push_back(use);
    return use;
  }

  void ReplaceAllUsesWith(T *that) {
    if (self == that->self) {
      return;
    }

    // Migrate the weak uses; we don't actually own them.
    for (auto weak_use : weak_uses) {
      if (weak_use) {
        assert(weak_use->def_being_used == self);
        weak_use->def_being_used = that->self;
        that->weak_uses.push_back(weak_use);
      }
    }
    weak_uses.clear();

    auto i = that->uses.size();

    // First, move the uses into the target's use list.
    for (auto &use : uses) {
      if (const auto use_val = use.release(); use_val) {
        use_val->def_being_used = that->self;
        that->uses.emplace_back(use_val);
      }
    }

    uses.clear();

    // Now, go through all those moved uses (in the target's use list), and
    // tell all the users that there's been a strong update.
    //
    // NOTE(pag): We assume that `Update` does not end up triggering any
    //            use removals.
    const auto max_i = that->uses.size();
    const auto time = User::gNextTimestamp++;
    for (; i < max_i; ++i) {
      if (Use<T> *use_val = that->uses[i].get(); use_val) {
        use_val->user->Update(time);
      }
    }
  }

  T *operator->(void) const noexcept {
    return self;
  }

  T &operator*(void) const noexcept {
    return *self;
  }

  bool IsUsed(void) const noexcept {
    return !uses.empty();
  }

  unsigned NumUses(void) const noexcept {
    return static_cast<unsigned>(uses.size());
  }

  template <typename U, typename CB>
  inline void ForEachUse(CB cb) const {
    for (const auto &use : uses) {
      if (use) {
        assert(self == use->def_being_used);
        if (auto user = dynamic_cast<U *>(use->user); user) {
          cb(user);
        }
      }
    }
  }

 private:
  template <typename>
  friend class Use;

  template <typename>
  friend class UseRef;

  template <typename>
  friend class WeakUseRef;

  template <typename>
  friend class UseList;

  template <typename>
  friend class DefList;

  static bool CompareUsePointers1(const std::unique_ptr<Use<T>> &a,
                                  const std::unique_ptr<Use<T>> &b) {
    return a.get() < b.get();
  }

  static bool CompareUsePointers2(const std::unique_ptr<Use<T>> &a, Use<T> *b) {
    return a.get() < b;
  }

  // Erase a use from the uses list.
  void EraseUse(Use<T> *to_remove) {
    if (!to_remove) {
      return;
    }

    assert(to_remove->def_being_used == self);
    const auto end = uses.end();
    auto it = std::remove_if(
        uses.begin(), end,
        [=](const std::unique_ptr<Use<T>> &a) { return a.get() == to_remove; });
    assert(it != end);
    uses.erase(it, end);
  }

  // Erase a use from the uses list.
  void EraseWeakUse(Use<T> *to_remove) {
    if (!to_remove) {
      return;
    }

    assert(to_remove->def_being_used == self);
    auto it = std::remove_if(weak_uses.begin(), weak_uses.end(),
                             [=](Use<T> *a) { return a == to_remove; });
    assert(it != weak_uses.end());
    weak_uses.erase(it, weak_uses.end());
  }

  // Points to this definition, just in case of multiple inheritance.
  T *const self;

  // All uses of this definition.
  std::vector<std::unique_ptr<Use<T>>> uses;
  std::vector<Use<T> *> weak_uses;
};

template <typename T>
void UseList<T>::Clear(void) {
  if (uses.empty()) {
    return;
  }

  std::vector<Use<T> *> old_uses;
  uses.swap(old_uses);

  for (auto use : old_uses) {
    DeleteUse(use);
  }
}

template <typename T>
void UseList<T>::AddUse(Def<T> *def) {
  if (def) {
    Use<T> *new_use = nullptr;
    if (is_weak) {
      new_use = def->CreateWeakUse(owner);
    } else {
      new_use = def->CreateUse(owner);
    }
    new_use->index = static_cast<unsigned>(uses.size());
    uses.push_back(new_use);
  }
}

template <typename T>
class UseRef {
 public:
  UseRef(User *user, Def<T> *def) : use(def ? def->CreateUse(user) : nullptr) {}

  void Swap(UseRef<T> &that) {
    if (use && that.use) {
      assert(use->user == that.use->user);
    }
    std::swap(use, that.use);
  }

  UseRef(void) = default;

  ~UseRef(void) {
    if (use) {
      const auto use_copy = use;
      use = nullptr;
      use_copy->def_being_used->EraseUse(use_copy);
    }
  }

  T *operator->(void) const noexcept {
    return use->def_being_used;
  }

  T &operator*(void) const noexcept {
    return *(use->def_being_used);
  }

  T *get(void) const noexcept {
    return use ? use->def_being_used : nullptr;
  }

  operator bool(void) const noexcept {
    return !!use;
  }

  void ClearWithoutErasure(void) {
    use = nullptr;
  }

 private:
  UseRef(const UseRef<T> &) = delete;
  UseRef(UseRef<T> &&) noexcept = delete;
  UseRef<T> &operator=(const UseRef<T> &) = delete;
  UseRef<T> &operator=(UseRef<T> &&) noexcept = delete;

  Use<T> *use{nullptr};
};

template <typename T>
class WeakUseRef {
 public:
  WeakUseRef(User *user, Def<T> *def)
      : use(def ? def->CreateWeakUse(user) : nullptr) {}

  void Swap(WeakUseRef<T> &that) {
    std::swap(use, that.use);
  }

  void Clear(void) {
    if (const auto use_copy = use; use_copy) {
      use = nullptr;
      if (auto def = use_copy->def_being_used; def) {
        def->EraseWeakUse(use_copy);
      }
      delete use;
    }
  }

  void ClearWithoutErasure(void) {
    Clear();
  }

  WeakUseRef(void) = default;

  ~WeakUseRef(void) {
    Clear();
  }

  T *operator->(void) const noexcept {
    return use->def_being_used;
  }

  T &operator*(void) const noexcept {
    return *(use->def_being_used);
  }

  T *get(void) const noexcept {
    return use ? use->def_being_used : nullptr;
  }

  operator bool(void) const noexcept {
    return use && use->def_being_used;
  }

 private:
  WeakUseRef(const WeakUseRef<T> &) = delete;
  WeakUseRef(WeakUseRef<T> &&) noexcept = delete;
  WeakUseRef<T> &operator=(const WeakUseRef<T> &) = delete;
  WeakUseRef<T> &operator=(WeakUseRef<T> &&) noexcept = delete;

  Use<T> *use{nullptr};
};

template <typename T>
class DefList {
 public:
  DefList(DefList<T> &&that) noexcept
      : owner(that.owner),
        defs(std::move(that.defs)) {}

  DefList<T> &operator=(DefList<T> &&that) noexcept {
    Clear();
    owner = that.owner;
    defs = std::move(that.defs);
    return *this;
  }

  DefList(void) = default;

  DefList(User *owner_) : owner(owner_) {}

  template <typename... Args>
  T *Create(Args &&... args) {
    auto new_def = new T(std::forward<Args>(args)...);
    defs.emplace_back(new_def);
    return new_def;
  }

  template <typename D, typename... Args>
  D *CreateDerived(Args &&... args) {
    auto new_def = new D(std::forward<Args>(args)...);
    defs.emplace_back(new_def);
    return new_def;
  }

  DefListIterator<T> begin(void) const noexcept {
    return defs.data();
  }

  DefListIterator<T> end(void) const noexcept {
    return &(defs.data()[defs.size()]);
  }

  T *operator[](size_t index) const noexcept {
    return dynamic_cast<T *>(defs[index]->self);
  }

  unsigned Size(void) const noexcept {
    return static_cast<unsigned>(defs.size());
  }

  bool Empty(void) const noexcept {
    return defs.empty();
  }

  template <typename CB>
  size_t RemoveIf(CB cb) {
    const auto old_size = defs.size();
    auto it = std::remove_if(
        defs.begin(), defs.end(),
        [&cb](const std::unique_ptr<T> &d) { return cb(d.get()); });
    defs.erase(it, defs.end());
    return defs.size() - old_size;
  }

  void Swap(DefList<T> &that) {
    defs.swap(that.defs);
    if (owner && that.owner) {
      owner->Update(User::gNextTimestamp++);
      if (owner != that.owner) {
        that.owner->Update(User::gNextTimestamp++);
      }
    } else if (owner) {
      owner->Update(User::gNextTimestamp++);

    } else if (that.owner) {
      that.owner->Update(User::gNextTimestamp++);
    }
  }

  template <typename C>
  void Sort(C cmp) {
    std::sort(defs.begin(), defs.end(),
              [&cmp](const std::unique_ptr<T> &a, const std::unique_ptr<T> &b) {
                return cmp(a.get(), b.get());
              });
    if (owner) {
      owner->Update(User::gNextTimestamp++);
    }
  }

  size_t RemoveUnused(void) {
    return RemoveIf([](T *v) { return !v->IsUsed(); });
  }

  void Clear(void) {
    if (defs.empty()) {
      return;
    }

    std::vector<std::unique_ptr<T>> old_defs;
    old_defs.swap(defs);
    old_defs.clear();
  }

  ~DefList(void) {
    Clear();
  }

 private:
  User *owner{nullptr};
  std::vector<std::unique_ptr<T>> defs;
};

template <typename T>
class Node;

template <typename T>
class UsedNodeIterator {
 public:
  inline UsedNodeIterator(UseListIterator<Node<T>> it_) : it(it_) {}

  inline UsedNodeIterator<T> &operator++(void) noexcept {
    ++it;
    return *this;
  }

  inline UsedNodeIterator<T> operator++(int) noexcept {
    return it++;
  }

  inline bool operator==(UsedNodeIterator<T> that) const noexcept {
    return it == that.it;
  }

  inline bool operator!=(UsedNodeIterator<T> that) const noexcept {
    return it != that.it;
  }

  inline T operator*(void) const noexcept {
    return T(*it);
  }

  inline T operator[](size_t index) const noexcept {
    return T(it[index]);
  }

  inline unsigned operator-(const UsedNodeIterator<T> &that) const noexcept {
    return it - that.it;
  }

 private:
  UseListIterator<Node<T>> it;
};

template <typename T>
class DefinedNodeIterator {
 public:
  inline DefinedNodeIterator(DefListIterator<Node<T>> it_) : it(it_) {}

  inline DefinedNodeIterator<T> &operator++(void) noexcept {
    ++it;
    return *this;
  }

  inline DefinedNodeIterator<T> operator++(int) noexcept {
    return it++;
  }

  inline bool operator==(DefinedNodeIterator<T> that) const noexcept {
    return it == that.it;
  }

  inline bool operator!=(DefinedNodeIterator<T> that) const noexcept {
    return it != that.it;
  }

  inline T operator*(void) const noexcept {
    return T(*it);
  }

  inline T operator[](size_t index) const noexcept {
    return T(it[index]);
  }

  inline unsigned operator-(const DefinedNodeIterator<T> &that) const noexcept {
    return it - that.it;
  }

 private:
  DefListIterator<Node<T>> it;
};

template <typename T>
struct UsedNodeRange {
 public:
  UsedNodeIterator<T> begin(void) const {
    return begin_;
  }
  UsedNodeIterator<T> end(void) const {
    return end_;
  }

  bool empty(void) const noexcept {
    return begin_ == end_;
  }

  unsigned size(void) const noexcept {
    return end_ - begin_;
  }

  inline T operator[](size_t index) const noexcept {
    return begin_[index];
  }

  const UsedNodeIterator<T> begin_;
  const UsedNodeIterator<T> end_;
};

template <typename T>
struct DefinedNodeRange {
 public:
  DefinedNodeIterator<T> begin(void) const {
    return begin_;
  }
  DefinedNodeIterator<T> end(void) const {
    return end_;
  }
  bool empty(void) const noexcept {
    return begin_ == end_;
  }

  unsigned size(void) const noexcept {
    return end_ - begin_;
  }

  inline T operator[](size_t index) const noexcept {
    return begin_[index];
  }
  const DefinedNodeIterator<T> begin_;
  const DefinedNodeIterator<T> end_;
};

}  // namespace circuitous
namespace std {

template <typename T>
struct iterator_traits<::circuitous::UseListIterator<T>> {
 public:
  typedef std::forward_iterator_tag iterator_category;
  typedef T *value_type;
  typedef unsigned difference_type;
  typedef void pointer;
  typedef void reference;
};

template <typename T>
struct iterator_traits<::circuitous::DefListIterator<T>> {
 public:
  typedef std::forward_iterator_tag iterator_category;
  typedef T *value_type;
  typedef unsigned difference_type;
  typedef void pointer;
  typedef void reference;
};

template <typename T>
struct iterator_traits<::circuitous::UsedNodeIterator<T>> {
 public:
  typedef std::forward_iterator_tag iterator_category;
  typedef T value_type;
  typedef unsigned difference_type;
  typedef void pointer;
  typedef T reference;
};

template <typename T>
struct iterator_traits<::circuitous::DefinedNodeIterator<T>> {
 public:
  typedef std::forward_iterator_tag iterator_category;
  typedef T value_type;
  typedef unsigned difference_type;
  typedef void pointer;
  typedef T reference;
};

}  // namespace std
