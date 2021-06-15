/*
 * Copyright (c) 2020-2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Util/TypeList.hpp>
#include <circuitous/IR/IR.h>

namespace circuitous {

  template<typename OP>
  struct MaterializedDefList {
    DefList<OP> data;

    std::size_t RemoveUnused() {
      auto notify_operands = [](auto &&x) {
        for (auto op : x->operands) {
          op->RemoveUser(x.get());
        }
      };
      return data.RemoveUnused(notify_operands);
    }

    template<typename CB>
    void ForEachOperation(CB &&cb) {
      for (auto op : this->data) {
        cb(op);
      }
    }

    template<typename CB>
    void Apply(CB &&cb) {
      cb(data);
    }

    auto &Attr() { return data; }

    template<typename CB>
    auto match(Operation *op, CB cb) {
      if (op->op_code == OP::kind) {
        cb(dynamic_cast<OP *>(op));
      }
    }

    template<typename CB>
    auto match_d(uint32_t kind, CB cb) -> decltype( cb( std::declval< OP * >() ) ) {
      if (kind == OP::kind) {
        cb(static_cast<OP *>(nullptr));
      }
    }
  };


  template< typename L > struct Attributes {};

  template< typename ... Ops >
  struct Attributes< tl::TL< Ops ... > > : Ops ... {

    template<typename T>
    using parent = MaterializedDefList< T >;

    template<typename T>
    auto &Attr() {
      return this->parent<T>::Attr();
    }

    template<typename CB>
    void ForEachOperation(CB cb) {
      (this->Ops::ForEachOperation(cb), ...);
    }

    void ClearWithoutErasure() {
      auto clear = [](auto &field) {
        for (auto op : field) {
          op->operands.ClearWithoutErasure();
        }
      };
      (this->Ops::Apply(clear), ...);
    }

    template<typename CB>
    void ForEachField(CB cb) {
      (this->Ops::Apply(cb), ...);
    }

    std::size_t RemoveUnused() {
      return (this->Ops::RemoveUnused() + ...);
    }

    template<typename CB> auto match(Operation *op, CB cb) { (this->Ops::match(op, cb), ...); }
    template<typename CB> auto match_d(uint32_t k, CB cb) { (this->Ops::match_d(k, cb), ...); }
  };

  template< typename T >
  struct to_mat_def_list {
    using type = MaterializedDefList< T >;
  };
  using m_def_lists = tl::apply< node_list_t, to_mat_def_list >;
  using AllAttributes = Attributes< m_def_lists >;

  // NOTE(lukas): This is not templated - it is not like we are going
  //              to have more in near future and it would just pollute
  //              error messages even further.
  struct CircuitStorage : Attributes< m_def_lists > {
    void RemoveUnused() {
      while (this->AllAttributes::RemoveUnused()) {}
    }

    template<typename T>
    std::size_t RemoveUnused() {
      return this->AllAttributes::parent<T>::RemoveUnused();
    }
    // clang-format off

    using AllAttributes::ForEachOperation;

    uint64_t ids = 0;
    static constexpr inline uint64_t max_id = (1ull >> 60);

    template<typename T> auto &Attr() {
      static_assert(std::is_base_of_v<Operation, T>);
      return this->AllAttributes::Attr<T>();
    }

    template<typename T, typename ...Args>
    auto Create(Args &&...args) {
      auto op = Attr<T>().Create(std::forward<Args>(args)...);
      op->_id = ++ids;
      return op;
    }

    template<typename ...Args>
    Operation *Create(uint32_t kind, Args &&...args) {
      Operation *out = nullptr;

      // NOTE(lukas): Templated lambda crashes my local clang
      auto create = [&](auto op) {
        using raw_t = std::remove_pointer_t< std::decay_t< decltype( op ) > >;
        // Down the line we will invoke `raw_t( std::forward< Args >(args) ... )`
        // and since ctors are not uniform, the invocation may not be well-formed
        // which leads to compile error.
        if constexpr ( std::is_constructible_v< raw_t, Args ... > ) {
          out = this->Create< raw_t >( std::forward< Args >( args ) ... );
        }
      };

      this->match_d( kind, create );
      return out;
    }

    template<typename T, typename ...Args>
    T* Adopt(uint64_t id, Args &&...args) {
      auto op = Attr<T>().Create(std::forward<Args>(args)...);
      op->_id = id;
      ids = std::max(ids, id);
      return op;
    }

    template<typename ...Args>
    Operation *Adopt(uint64_t id, uint32_t kind, Args &&...args) {
      Operation *out;
      auto adopt = [&](auto op) {
        using raw_t = std::remove_pointer_t< std::decay_t< decltype( op ) > >;
        if constexpr ( std::is_constructible_v< raw_t, Args ... > ) {
          out = this->Adopt< raw_t >( id, std::forward< Args >( args )... );
        }
      };
      return out;
    }

    template<typename T>
    T *Fork(T *original) {
      T copy{*original};
      copy._id = ++ids;
      copy.operands.clear();
      return Attr<T>().Adopt(std::move(copy));
    }

    Operation *Fork(Operation *op) {
      Operation *out;
      auto fork = [&](auto op) {
        out = this->Fork(op);
      };

      this->match(op, fork);
      return out;
    }

    template<typename What>
    auto fetch_singular() {
      auto &all = this->Attr<What>();
      CHECK_EQ(all.Size(), 1);
      return all[0];
    }

    template<typename What, bool allow_failure = true>
    auto fetch_reg(const std::string &name) -> What * {
      for (auto reg : this->Attr<What>()) {
        if (reg->reg_name == name) {
          return reg;
        }
      }
      if constexpr (!allow_failure) {
        LOG(FATAL) << "Register " << name << " not present";
      }
      return nullptr;
    }

    InputInstructionBits *input_inst_bits() { return fetch_singular<InputInstructionBits>(); }
    InputErrorFlag *input_ebit() { return fetch_singular<InputErrorFlag>(); }
    OutputErrorFlag *output_ebit() { return fetch_singular<OutputErrorFlag>(); }

    using cstr_ref = const std::string &;
    InputRegister *input_reg(cstr_ref name) { return fetch_reg<InputRegister>(name); }
    OutputRegister *output_reg(cstr_ref name) { return fetch_reg<OutputRegister>(name); }
  };



} // namespace circuitous