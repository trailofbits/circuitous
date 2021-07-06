/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Verify.hpp>

#include <circuitous/Run/Trace.hpp>
#include <circuitous/IR/Intrinsics.hpp>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <gflags/gflags.h>
#include <glog/logging.h>

#include <llvm/ADT/StringRef.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/ADT/APInt.h>
#pragma clang diagnostic pop

namespace circ::run {

  // TODO(lukas): Most likely it will be required for this to be an attribute
  //              as we will need to move/copy it between `Spawn` classes.
  struct HasMemory {
    using raw_value_type = llvm::APInt;
    using value_type = std::optional<raw_value_type>;

    std::unordered_map<uint64_t, raw_value_type> memory;

    template< typename U >
    bool defined(uint64_t addr, U size) {
      for (auto i = 0u; i < size; ++i) {
        if (!memory.count(addr + i)) {
          return false;
        }
      }
      return true;
    }

    template< typename U >
    value_type load(uint64_t addr, U size_) {
      U size = size_;
      if (!defined(addr, size)) {
        return {};
      }

      llvm::APInt build{ static_cast< uint32_t >(size * 8), 0, false };
      for (auto i = 0u; i < size; ++i) {
        build.insertBits( memory[addr + i], i * 8 );
      }
      return build;
    }

    void store(uint64_t addr, raw_value_type val) {
      CHECK( val.getBitWidth() % 8 == 0 )
          << "Cannot store val that has unalinged bw such as " << val.getBitWidth();

      for (auto i = 0u; i < val.getBitWidth(); i += 8) {
        memory[addr + i] = val.extractBits(8, i);
      }
    }

    using Parsed = intrinsics::Memory::Parsed<llvm::APInt>;

    Parsed deconstruct(const llvm::APInt &value);
    llvm::APInt construct(const Parsed &parsed);

  };

  template<typename Self>
  struct Base_ : public Visitor<Self>, HasMemory {
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

    std::unordered_map<Operation *, value_type> values() const {
      return node_values;
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
    void Visit(And *op);

    // Must be called in `safe` context.
    auto lhs(Operation *op) { return *self().get(op, 0); }
    auto rhs(Operation *op) { return *self().get(op, 1); }
    bool is_zero(const llvm::APInt &i) { return i.isNullValue(); }

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

    auto bv(bool b) { return this->BoolVal(b); }

    void Visit(Icmp_ult *op) { safe(op, [&](auto o){ return bv(lhs(o).ult(rhs(o))); } ); }
    void Visit(Icmp_slt *op) { safe(op, [&](auto o){ return bv(lhs(o).slt(rhs(o))); } ); }
    void Visit(Icmp_ugt *op) { safe(op, [&](auto o){ return bv(lhs(o).ugt(rhs(o))); } ); }

    void Visit(Icmp_uge *op) { safe(op, [&](auto o){ return bv(lhs(o).uge(rhs(o))); } ); }
    void Visit(Icmp_ule *op) { safe(op, [&](auto o){ return bv(lhs(o).ule(rhs(o))); } ); }
    void Visit(Icmp_sgt *op) { safe(op, [&](auto o){ return bv(lhs(o).sgt(rhs(o))); } ); }
    void Visit(Icmp_sge *op) { safe(op, [&](auto o){ return bv(lhs(o).sge(rhs(o))); } ); }
    void Visit(Icmp_sle *op) { safe(op, [&](auto o){ return bv(lhs(o).sle(rhs(o))); } ); }

    void Visit(Icmp_eq *op) { safe(op, [&](auto o){ return bv(lhs(o) == rhs(o)); } ); }
    void Visit(Icmp_ne *op) { safe(op, [&](auto o){ return bv(lhs(o) != rhs(o)); } ); }
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


    std::vector<HasMemory::Parsed> get_derived_mem() {
      std::vector<HasMemory::Parsed> out;
      for (auto op : this->circuit->template Attr<Memory>()) {
        // TODO(lukas): Check if memory was derived or supplied
        // TODO(lukas): This should never happen once we enforce zeroed unused hints
        if (this->has_value(op)) {
          out.push_back(this->deconstruct(*this->get(op)));
        }
      }
      return out;
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
    void Visit(Advice *op);
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
    void Visit(RegConstraint *op);
    void Visit(PreservedConstraint *op);
    void Visit(CopyConstraint *op);
    void Visit(AdviceConstraint *op);

    void Visit(VerifyInstruction *op);
    void Visit(OnlyOneCondition *op);

    void Visit(ReadConstraint *op);
    void Visit(WriteConstraint *op);
    void Visit(UnusedConstraint *op);
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

} // namespace circ::run