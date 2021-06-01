/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>
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
    void VisitOperation(Operation *op);
    // Circuit
    void VisitCircuit(Circuit *op);
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
    using Next::safe;
    using Next::Next;

    // Constant semantics
    void VisitConstant(Constant *op);
    void VisitUndefined(Undefined *op);

    // Operations
    void VisitConcat(Concat *op);
    void VisitExtract(Extract *op);
    void VisitNot(Not *op);
    void VisitLLVMOperation(LLVMOperation *op);
    void VisitSelect(Select *op);
    void VisitParity(Parity *op);
    void VisitPopulationCount(PopulationCount *op);
  };

  #include "Base.tpp"

  template<typename Next>
  struct Ctx_ : Next {
    using Next::self;
    using Next::safe;
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
    using Next::Next;

    // Input semantics
    void VisitInputRegister(InputRegister *op);
    void VisitInputImmediate(InputImmediate *op);
    void VisitInputErrorFlag(InputErrorFlag *op);
    void VisitInputInstructionBits(InputInstructionBits *op);

    // Output semantics
    void VisitOutputRegister(OutputRegister *op);
    void VisitOutputErrorFlag(OutputErrorFlag *op);
    void VisitHint(Hint *op);
  };

  template<typename Next>
  struct CSem : Next, c_sem {
    using value_type = typename Next::value_type;
    using raw_value_type = typename Next::raw_value_type;

    using Next::self;
    using Next::safe;
    using Next::Next;

    // Condition semantics
    void VisitDecodeCondition(DecodeCondition *op);
    void VisitRegisterCondition(RegisterCondition *op);
    void VisitPreservedCondition(PreservedCondition *op);
    void VisitCopyCondition(CopyCondition *op);
    void VisitHintCondition(HintCondition *op);
    void VisitVerifyInstruction(VerifyInstruction *op);
    void VisitOnlyOneCondition(OnlyOneCondition *op);
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