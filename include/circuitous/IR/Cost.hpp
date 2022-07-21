/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Visitors.hpp>

#include <unordered_map>
#include <map>
#include <sstream>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#pragma clang diagnostic pop

namespace circ {

struct RawNodesCounter_ : UniqueVisitor<RawNodesCounter_> {
  using parent = UniqueVisitor<RawNodesCounter_>;

  using arg_details_t = std::vector< uint32_t >;
  struct hash {
    auto operator()(const arg_details_t &args) const {
      std::stringstream ss;
      for (auto x : args) ss << x;
      return std::hash< std::string >{}(ss.str());
    }
  };

  using args_occurences_t = std::unordered_map< const arg_details_t, uint64_t, hash >;
  // Maps kind -> number of times it was seen
  using operation_occurences_t = std::map< Operation::kind_t, args_occurences_t >;
  operation_occurences_t nodes;

  arg_details_t get_arg_detail(Operation *op) {
    arg_details_t out{ op->size };
    for (auto o : op->operands())
      out.push_back(o->size);
    return out;
  }

  uint64_t total_count(const args_occurences_t &occurencies) const {
    uint64_t out = 0;
    for (const auto &[_, count] : occurencies)
      out += count;
    return out;
  }

  uint64_t total_count(Operation::kind_t rkind) {
    return total_count(nodes[rkind]);
  }

  void Process(Operation *op) {
    auto &it = nodes[op->op_code];
    auto [entry, _2] = it.try_emplace(get_arg_detail(op), 0);
    ++entry->second;
  }

  void visit(Operation *op) {
    Process(op);
    op->traverse(*this);
  }

  void Run(Operation *op) {
    dispatch(op);
  }

  auto Export() { return *this; }

  template<typename CB>
  void Diff(const RawNodesCounter_ &o, CB cb) const {
    auto diff = [&](auto self_, auto other_) {
      auto self = self_.begin();
      auto other = other_.begin();
      while (self != self_.end() && other != other_.end()) {
        if (self->first == other->first){
          cb(self->first, this->total_count(self->second), o.total_count(other->second));
          ++self; ++other;
        } else if (self->first < other->first) {
          cb(self->first, this->total_count(self->second), 0ull);
          ++self;
        } else {
          cb(other->first, 0ull, o.total_count(other->second));
          ++other;
        }
      }
      while (self != self_.end()) {
        cb(self->first, this->total_count(self->second), 0ull);
        ++self;
      }
      while (other != other_.end()) {
        cb(other->first, 0ull, o.total_count(other->second));
        ++other;
      }
    };

    diff(nodes, o.nodes);
  }
};

using RawNodesCounter = RawNodesCounter_;

template<typename Collector>
struct Printer {
  using args_occurences_t = typename Collector::args_occurences_t;

  template<typename OS>
  static void Print(OS &os, const Collector &self) {
    os << "Node counts:" << std::endl;
    for (auto &[op_code, occurencies] : self.nodes) {
      os << " " << op_code_str(op_code)
         << " " << self.total_count(occurencies)
         << std::endl;
      print_occurences(os, occurencies, "\t");
    }
    os << std::endl;
  }

  template<typename OS>
  static void print_occurences(
      OS &os, const args_occurences_t &self, const std::string &prefix)
  {
    for (const auto &[occ, count] : self)
    {
      os << prefix << count << ": ";
      for (std::size_t i = 1; i < occ.size(); ++i)
        os << occ[i] << " ";
      if (occ.size() == 1)
        os << "() ";
      os << "-> " << occ.front() << std::endl;
    }
  }

  template<typename OS>
  static void Diff(OS &os, const Collector &self, const Collector &other) {
    auto red = [](auto what) -> std::string {
      return "\033[91m" + std::to_string(what) + "\033[0m";
    };
    auto green = [](auto what) -> std::string {
      return "\033[92m" + std::to_string(what) + "\033[0m";
    };

    auto log = [&](auto what, auto orig, auto updated) {
      if (orig == updated)
        return;

      int64_t changed_by = static_cast< int64_t >(updated) - static_cast< int64_t >(orig);

      os << " " << op_code_str(what) << "( ";
      if (changed_by > 0) os << red(changed_by);
      if (changed_by < 0) os << green(changed_by);
      os << " )" << std::endl;

    };
    self.Diff(other, log);
  }
};

template<typename Collector, typename P>
struct PrintStatistics_ : Collector, P {
  template<typename OS>
  void Run(Operation *op, OS &os) {
    this->Collector::Run(op);
    this->P::Print(os, *this);
  }
};

using StatsPrinter = PrintStatistics_<RawNodesCounter, Printer<RawNodesCounter>>;

// TODO(lukas): Make more configurable
static inline std::string GetStats(Operation *op) {
  std::stringstream ss;
  StatsPrinter().Run(op, ss);
  return ss.str();
}

} // namespace circ
