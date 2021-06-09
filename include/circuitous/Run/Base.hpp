/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Verify.hpp>

#include <circuitous/Run/Trace.hpp>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <gflags/gflags.h>
#include <glog/logging.h>

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/ADT/APInt.h>
#pragma clang diagnostic pop

namespace circuitous::run {

  template<typename Self>
  struct Base_ : public Visitor<Self> {
    using raw_value_type = llvm::APInt;
    // If no value is held <=> value is undefined
    using value_type = std::optional<raw_value_type>;
    using node_values_t = std::unordered_map<Operation *, value_type>;

    Circuit *circuit{nullptr};
    bool changed{false};

    std::unordered_map<Operation *, value_type> node_values;

    value_type Undef() const { return {}; }
    llvm::APInt TrueVal() const { return llvm::APInt(1, 1); }
    llvm::APInt FalseVal() const { return llvm::APInt(1, 0); }
    llvm::APInt BoolVal(bool v) const { return (v) ? TrueVal() : FalseVal(); }

    Base_(Circuit *circuit_) : circuit(circuit_) {}

    void init() {}

    template<typename ...Args>
    bool ValidVals(Args &&... args) {
      return (has_value(args) && ...);
    }

    bool ValidChildren(Operation *op) {
      for (auto child : op->operands) {
        if (!GetNodeVal(child)) {
          return false;
        }
      }
      return true;
    }

    std::unordered_map<Operation *, llvm::APInt> values() const {
      std::unordered_map<Operation *, llvm::APInt> out;
      for (auto &[op, val] : node_values) {
        if (val) {
          out[op] = *val;
        }
      }
      return out;
    }

    auto &self() { return static_cast<Self &>(*this); }

    template<typename Op, typename F>
    auto safe(Op *op, F &&f) {
      if (!ValidChildren(op)) {
        return self().SetNodeVal(op, Undef());
      }
      self().SetNodeVal(op, f(op));
    }

    void SetNodeVal(Operation *op, const value_type &val);

    value_type GetNodeVal(Operation *op) const ;
    value_type get(Operation *op) const  {
      return GetNodeVal(op);
    }

    value_type get(Operation *op, std::size_t idx) {
      CHECK(op->operands.size() > idx);
      return get( ( *op )[ idx ] );
    }

    bool has_value(Operation *op) const { return node_values.count(op); }

    void set_input_state(const trace::Entry &in);
    void set_output_state(const trace::Entry &out);

    bool get_result() const;
    trace::Entry get_output_state() const;

    template<typename T>
    std::unordered_map<T *, value_type> get_derived() const {
      LOG(FATAL) << "Base_ cannot export derived values.";
    }

    // Default
    void Visit(Operation *op);
    // Circuit
    void Visit(Circuit *op);
  };

  // Tags that tells us about which visits are implemented by a layer
  // eventually we want to have all included to be able to interpret
  // the circuit properly.
  // Semantics of input/output nodes
  struct io_sem {};
  // Semantics of operations (addition, and, xor, ...)
  struct op_sem {};
  // Semantics of conditions (hint check, output check, ...)
  struct c_sem {};

  template<typename T, typename ... Kinds>
  static inline constexpr bool valid_interpreter_() {
    return (std::is_base_of_v<Kinds, T> && ...);
  }

  template<typename T>
  static inline constexpr bool valid_interpreter() {
    return valid_interpreter_<T, io_sem, op_sem, c_sem>();
  }

  template<typename Next>
  struct OpSem : Next, op_sem {
    using value_type = typename Next::value_type;
    using raw_value_type = typename Next::raw_value_type;

    using Next::self;
    using Next::Visit;
    using Next::safe;


    using Next::Next;

    // Constant semantics
    void Visit(Constant *op);
    void Visit(Undefined *op);

    // Operations
    void Visit(Concat *op);
    void Visit(Extract *op);
    void Visit(Not *op);
    void Visit(Select *op);
    void Visit(Parity *op);
    void Visit(PopulationCount *op);
    void Visit(Or *op);

    // Must be called in `safe` context.
    auto lhs(Operation *op) { return *self().get(op, 0); }
    auto rhs(Operation *op) { return *self().get(op, 1); }
    bool is_zero(const llvm::APInt &i) { return i.toString(16, false) == "0"; }

    void Visit(BSelect *op_) {
      auto sel = [&](auto op) {
        return (self().get(op, 0)->getBoolValue()) ? self().get(op, 1) : self().get(op, 2);
      };
      safe(op_, sel);
    }

    void Visit(Add *op) { safe(op, [&](auto o){ return lhs(o) + rhs(o); } ); }
    void Visit(Sub *op) { safe(op, [&](auto o){ return lhs(o) - rhs(o); } ); }
    void Visit(Mul *op) { safe(op, [&](auto o){ return lhs(o) * rhs(o); } ); }

    void Visit(UDiv *op) {
      auto div = [&](auto o) {
        return (is_zero(rhs(o))) ? this->Undef() : std::make_optional( lhs(o).udiv(rhs(o)) );
      };
      safe(op, div);
    }
    void Visit(SDiv *op) {
      auto div = [&](auto o) {
        return (is_zero(rhs(o))) ? this->Undef() : std::make_optional( lhs(o).sdiv(rhs(o)) );
      };
      safe(op, div);
    }

    void Visit(CAnd *op) { safe(op, [&](auto o){ return lhs(o) & rhs(o); } ); }
    void Visit(COr *op) { safe(op, [&](auto o){ return lhs(o) | rhs(o); } ); }
    void Visit(CXor *op) { safe(op, [&](auto o){ return lhs(o) ^ rhs(o); } ); }



    void Visit(Shl *op) { safe(op, [&](auto o){ return lhs(o) << rhs(o); } ); }
    void Visit(LShr *op) { safe(op, [&](auto o){ return lhs(o).lshr(rhs(o)); } ); }
    void Visit(AShr *op) { safe(op, [&](auto o){ return lhs(o).ashr(rhs(o)); } ); }

    void Visit(Trunc *op) {
      safe(op, [&](auto o){ return lhs(o).trunc(o->size); } );
    }
    void Visit(ZExt *op) { safe(op, [&](auto o){ return lhs(o).zext(o->size);   } ); }
    void Visit(SExt *op) { safe(op, [&](auto o){ return lhs(o).sext(o->size);  } ); }

    void Visit(Icmp_ult *op) { safe(op, [&](auto o){ return this->BoolVal(lhs(o).ult(rhs(o))); } ); }
    void Visit(Icmp_slt *op) { safe(op, [&](auto o){ return this->BoolVal(lhs(o).slt(rhs(o))); } ); }
    void Visit(Icmp_ugt *op) { safe(op, [&](auto o){ return this->BoolVal(lhs(o).ugt(rhs(o))); } ); }
    void Visit(Icmp_eq *op) { safe(op, [&](auto o){ return this->BoolVal(lhs(o) == rhs(o)); } ); }
    void Visit(Icmp_ne *op) { safe(op, [&](auto o){ return this->BoolVal(lhs(o) != rhs(o)); } ); }
  };

  #include "Base.tpp"

  template<typename Next>
  struct Ctx_ : Next {
    using Next::self;
    using Next::safe;
    using Next::Visit;

    using Next::Next;

    std::unordered_set<Operation *> supplied;
    std::unordered_set<Operation *> derived;

    template<typename T, typename ...Ts>
    void init() {
      for (auto op : this->circuit->template Attr<T>()) {
        if (this->has_value(op)) {
          supplied.insert(op);
        } else {
          derived.insert(op);
        }
      }
      if constexpr (sizeof ... (Ts) != 0) {
        init<Ts...>();
      }
    }

    void init();
    void verify_cond(Operation *op);
    void derive_cond(Operation *op);

    template<typename T>
    void handle_cond(T *op) {
      if (supplied.count(op->operands[1])) {
        return verify_cond(op);
      }
      return derive_cond(op);
    }

    template<typename T>
    auto get_derived() const {
      std::unordered_map<T *, typename Next::value_type> out;
      for (auto op : derived) {
        if (op->op_code == T::kind) {
          out[dynamic_cast<T *>(op)] = this->get(op);
        }
      }
      return out;
    }
  };

  template<typename Next>
  struct IOSem : Next, io_sem {
    using value_type = typename Next::value_type;
    using raw_value_type = typename Next::raw_value_type;

    using Next::self;
    using Next::safe;
    using Next::Visit;

    using Next::Next;


    // Input semantics
    void Visit(InputRegister *op);
    void Visit(InputImmediate *op);
    void Visit(InputErrorFlag *op);
    void Visit(InputInstructionBits *op);

    // Output semantics
    void Visit(OutputRegister *op);
    void Visit(OutputErrorFlag *op);
    void Visit(Hint *op);
  };

  template<typename Next>
  struct CSem : Next, c_sem {
    using value_type = typename Next::value_type;
    using raw_value_type = typename Next::raw_value_type;

    using Next::self;
    using Next::safe;
    using Next::Visit;

    using Next::Next;


    // Condition semantics
    void Visit(DecodeCondition *op);
    void Visit(RegisterCondition *op);
    void Visit(PreservedCondition *op);
    void Visit(CopyCondition *op);
    void Visit(HintCondition *op);
    void Visit(VerifyInstruction *op);
    void Visit(OnlyOneCondition *op);
  };

  #include <circuitous/Run/Derive.tpp>

  namespace verify {
    template<typename Next>
    struct Ctx : Ctx_<Next> {
      using parent_t = Ctx_<Next>;

      using Next::self;
      using parent_t::parent_t;


      void init() {
        parent_t::init();
        for (auto op : this->derived) {
          this->supplied.insert(op);
          self().SetNodeVal(op, this->Undef());
        }
        this->derived.clear();
      }
    };

    template<typename Next>
    using Base = CSem<IOSem<Ctx<Next>>>;
  } // namespace verify

  namespace derive {
    template<typename S>
    using Base = CSem<IOSem<Ctx_<S>>>;
  } // namespace derive

  template< typename S >
  using DBase = derive::Base<OpSem<Base_<S>>>;

  template<typename S>
  using VBase = verify::Base<OpSem<Base_<S>>>;

} // namespace circuitous::run