/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>
#include <circuitous/IR/Storage.hpp>

namespace circ {

  // We can try to do some optimizations, but we want to be
  // able to toggle them (if for nothing we want to be able to
  // test them)
  struct Optimizations {
    bool reduce_imms = false;
  };

  struct Circuit : CircuitStorage, Operation {
    static constexpr inline uint32_t kind = 0x1;

    virtual ~Circuit() = default;

    using circuit_ptr_t = std::unique_ptr<Circuit>;

    static circuit_ptr_t make_circuit(std::string_view arch_name,
                                      std::string_view os_name,
                                      const llvm::StringRef &bytes,
                                      const Optimizations &opts={});

    static circuit_ptr_t make_circuit(std::string_view arch_name,
                                      std::string_view os_name,
                                      std::string_view bytes,
                                      const Optimizations &opts={});
    void Serialize(std::ostream &os);

    void Serialize(std::function<std::ostream &(const std::string &)> os_opener);

    static std::unique_ptr<Circuit> Deserialize(std::istream &is);


    std::string Name() const override { return "circuit"; }
    Circuit() : Operation(this->bool_size, kind) {}
  };


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
  using Visitor = Visitor_< Derived, node_list_t >;

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
  using DVisitor = DVisitor_< Derived, node_list_t >;

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

} // namespace circ