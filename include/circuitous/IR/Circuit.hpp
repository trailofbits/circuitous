/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Storage.hpp>

namespace circ {

  struct Circuit : CircuitStorage, Operation, make_kind< Root, tag_fragment< 0 > > {
    static constexpr uint32_t kind = apply(Operation::kind);
    virtual ~Circuit() = default;

    using circuit_ptr_t = std::unique_ptr<Circuit>;

    static circuit_ptr_t make_circuit(std::string_view arch_name,
                                      std::string_view os_name,
                                      const llvm::StringRef &bytes);

    static circuit_ptr_t make_circuit(std::string_view arch_name,
                                      std::string_view os_name,
                                      std::string_view bytes);
    void Serialize(std::ostream &os);

    void Serialize(std::function<std::ostream &(const std::string &)> os_opener);

    static std::unique_ptr<Circuit> Deserialize(std::istream &is);

    static std::string op_code_str() { return "circuit"; }

    std::string Name() const override { return "circuit"; }
    // TODO(lukas): Will be deprecated in the future.
    Circuit(uint32_t ptr_size_=64) : Operation(this->bool_size, kind), ptr_size(ptr_size_) {}

    uint32_t ptr_size;
  };

  using all_nodes_list_t = tl::push_front< Circuit, subnode_list_t >;

  template< typename D, typename L > struct Visitor_ {};

  template<typename Derived, typename ... Ops >
  struct Visitor_< Derived, tl::TL< Ops ... > > {
    void Visit(Operation *op) {
      op->Traverse(*this);
    }

    Derived &self() { return static_cast<Derived &>(*this); }

    template<typename T, typename ...Tail, typename ... Args>
    auto Visit_(Operation *op, Args &&...args) {
      if (is_specialization< T >(op->op_code)) {
        return self().Visit(dynamic_cast<T *>(op), std::forward<Args>(args)...);
      }
      if constexpr (sizeof...(Tail) != 0) {
        return this->Visit_<Tail ...>(op, std::forward<Args>(args)...);
      } else {
        return self().Visit(op, std::forward<Args>(args)...);
      }
    }

    template<typename ...Args>
    auto Dispatch(Operation *op, Args &&...args) {
      return this->Visit_<Ops...>(op, std::forward<Args>(args)...);
    }
  };

  template< typename Derived >
  using Visitor = Visitor_< Derived, all_nodes_list_t >;

  template< typename D, typename L > struct NonRecursiveVisitor_ {};

  template< typename Derived, typename ... Ops >
  struct NonRecursiveVisitor_< Derived, tl::TL< Ops ... > >
  {
    Derived &self() { return static_cast<Derived &>(*this); }

    template<typename T, typename ...Tail, typename ... Args>
    auto Visit_(Operation *op, Args &&...args) {
      if (is_specialization< T >(op->op_code))
        return self().Visit(dynamic_cast<T *>(op), std::forward<Args>(args)...);

      if constexpr (sizeof...(Tail) != 0) {
        return this->Visit_<Tail ...>(op, std::forward<Args>(args)...);
      } else {
        LOG(FATAL) << "unhandled operation";
      }
    }

    template<typename ...Args>
    auto Dispatch(Operation *op, Args &&...args) {
      return this->Visit_<Ops...>(op, std::forward<Args>(args)...);
    }
  };

  template< typename Derived >
  using NonRecursiveVisitor = NonRecursiveVisitor_< Derived, all_nodes_list_t >;

  template< typename D, typename L > struct DVisitor_ {};

  template<typename Derived, typename ... Ops >
  struct DVisitor_< Derived, tl::TL< Ops ... > > {
    Derived &self() { return static_cast<Derived &>(*this); }

    template<typename T, typename ...Tail, typename ...Args>
    auto Visit_(uint32_t kind, Args &&... args) {
      if (is_specialization< T >(kind)) {
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
  using DVisitor = DVisitor_< Derived, all_nodes_list_t >;

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

  static inline std::string shatter_fragment_as_str(uint32_t rkind)
  {
    std::stringstream ss;
    ss << std::hex << "[ ";
    ss << (rkind & (0xffu << 24)) << " "
       << (rkind & (0xffffu << 8)) << " "
       << (rkind & 0xffu);
    ss << " ]: ";
    return ss.str();
  }

  // TODO(lukas): Generalise comparator and move to `tl::`.
  template< typename F, typename H, typename ... Tail >
  auto runtime_find_(uint32_t rkind, F &&f) {
    if (H::kind == rkind) {
      return f(static_cast<H *>(nullptr));
    }
    if constexpr (sizeof...(Tail) != 0) {
      return runtime_find_< F, Tail ... >(rkind, f);
    } else {
      LOG(FATAL) << "runtime find on: " << shatter_fragment_as_str(rkind) << " failed";
    }
  }

  // These may be quite expensive to re-compute, if they are called
  // often, consider refactor or at least caching.
  template< typename F, typename ... Ts >
  auto runtime_find( tl::TL< Ts ... >, uint32_t rkind, F &&f) {
    return runtime_find_< F, Ts... >(rkind, std::forward< F >(f));
  }

  static inline std::string op_code_str(uint32_t rkind) {
   auto get_name = [](auto x) {
      using raw_t = std::remove_pointer_t< std::decay_t< decltype( x ) > >;
      return raw_t::op_code_str();
    };

    static std::unordered_map< uint32_t, std::string > cache;
    if (!cache.count(rkind)) {
      cache[rkind] = runtime_find(all_nodes_list_t{}, rkind, get_name);
    }

    return cache[rkind];
  }


  // TODO(lukas): This is just a hotfix, ideally we want the type list
  //              will all the nodes to carry names.
  static inline std::string fragment_as_str(uint32_t kind) {
    std::stringstream ss;
    ss << shatter_fragment_as_str(kind);
    ss << op_code_str(kind);
    return ss.str();
  }

  template<typename Kind>
  static inline std::string to_string(Kind kind) {
    return fragment_as_str(kind);
  }

  template< bool with_meta = true >
  static inline std::string pretty_print(Operation *op) {
    std::stringstream ss;
    ss << op->id() << ": " << to_string(op->op_code);
    if constexpr (with_meta) {
      if (op->meta_size())
        ss << std::endl << op->dump_meta();
    }
    return ss.str();
  }

} // namespace circ
