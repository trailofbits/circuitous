/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>

namespace circ::run {

  struct Formatter {
    std::stringstream ss;

    Formatter &operator<<(const std::string &str) {
      ss << str;
      return *this;
    }

    template<typename T>
    Formatter &operator<<(const std::optional<T> &t) {
      if (t) {
        *this << *t;
      } else {
        *this << "{ undef }";
      }
      return *this;
    }

    Formatter &operator<<(const llvm::APInt &apint) {
      ss << llvm::toString(apint, 16, false);
      return *this;
    }

    Formatter &operator<<(Operation *op) {
      ss << op->op_code_str() << " " << op->id();
      return *this;
    }

    auto str() { return ss.str(); }
  };

  template<typename Interpreter>
  struct Inspector_ {
    using spawn_t = typename Interpreter::spawn_t;

    using value_type = typename spawn_t::value_type;
    using raw_value_type = typename spawn_t::raw_value_type;

    Interpreter *what;
    spawn_t *lenses;
    Circuit *circuit;


    Inspector_(Interpreter *what_) : what(what_), circuit(what_->circuit) {}

    // Move focus to chosen runner
    bool focus() { focus(what->acceptor); return lenses; }
    void focus(spawn_t *s) { lenses = s; }
    void focus(uint32_t i) {
      focus(&(std::next(what->runners.begin(), i)->second));
    }


    auto &operator*() { return *lenses; }
    auto operator->() { return lenses; }

    // result queries
    bool result() { return *lenses->result; }

    bool g_result() { return what->result(); }

    // memory queries

    // node queries

    // ctx queries
    void ctx_(Formatter &ss, Operation *op, int32_t depth = 0, uint32_t current = 1) {
      if (depth <= 0) {
        return;
      }
      std::string prefix(current * 2, '.');
      auto val = lenses->node_values[op];
      ss << prefix << "  " << op << " -> " << val << "\n";
      for (auto o : op->operands) {
        ctx_(ss, o, depth - 1, current + 1);
      }
    }

    std::string ctx(int32_t depth = 1) {
      Formatter ss;
      ctx_(ss, lenses->current, depth, 0);
      return ss.str();
    }

    // derived queries

  };

  template<typename I>
  using Inspector = Inspector_< I >;

} // namespace circ::run
