/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <map>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>

#include <circuitous/IR/Verify.hpp>
#include <circuitous/IR/Cost.hpp>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <glog/logging.h>
#pragma clang diagnostic pop

namespace circuitous {

class Circuit;

// Look for uses of population count that operates on a zero-extended value, and
// change it to operate on the original value.
bool StrengthReducePopulationCount(Circuit *circuit);

// Look for uses of the population count instruction that look like they are
// actually computing the parity of some bits, and then replace that computation
// with a parity node.
bool ConvertPopCountToParity(Circuit *circuit);

// Look for common topological structures and extract them so that they are
// shared by multiple different expressions.
bool ExtractCommonTopologies(Circuit *circuit);

// Merge all of the hint inputs into a single "wide" input hint that is of
// sufficient size to support all verifiers. In place of the individual hints,
// the verifiers pull out slices of wide hint value with an EXTRACT.
bool MergeHints(Circuit *circuit);

bool MergeByBytes(Circuit *circuit);
bool DAGify(Circuit *circuit);

struct KnownPasses {
  using pass_t = bool(*)(Circuit *);
  using transformation_t = std::pair<uint8_t, pass_t>;
  using storage_t  = std::unordered_map<std::string, transformation_t>;

  static const inline storage_t passes = {
    {"dagify", { 0, &DAGify } },
    {"popcount2parity", { 2, &ConvertPopCountToParity } },
    {"reducepopcount", { 1, &StrengthReducePopulationCount } },
    {"extractcommon", { 3, &ExtractCommonTopologies } },
    {"mergehints", { 4, &MergeHints } }
  };

  static std::string PassName(pass_t pass) {
    for (auto &[name, entry] : passes) {
      if (entry.second == pass) {
        return name;
      }
    }
    LOG(FATAL) << "Pass is not known";
  }

  static transformation_t Get(const std::string &name) {
    auto it = passes.find(name);
    CHECK(it != passes.end());
    return it->second;
  }
};

struct Manager : KnownPasses {
  std::map<uint8_t, pass_t> selected;

  void AddPass(const std::string &name) {
    const auto &[priority, pass] = Get(name);
    LOG(INFO) << "Adding pass" << name;
    selected[priority] = pass;
  }

  void RunPass(Circuit *circuit, pass_t pass) {
    pass(circuit);
    circuit->RemoveUnused();
  }

};

template<typename Next>
struct WithHistory : Next {
  using pass_t = typename Next::pass_t;
  using entry_t = std::pair<std::string, RawNodesCounter>;

  std::vector<entry_t> history;

  void RunPass(Circuit *circuit, pass_t pass) {
    auto collect = [&](auto name) {
      LOG(INFO) << "Capturing stats";
      RawNodesCounter collector;
      collector.Run(circuit);
      history.emplace_back(name, std::move(collector));
      LOG(INFO) << "Done.";
    };

    if (history.size() == 0) {
      collect("Before");
    }
    this->Next::RunPass(circuit, pass);
    collect("After " + this->PassName(pass));
  }

  std::string Stats() {
    using printer_t = Printer<RawNodesCounter>;

    std::stringstream ss;
    auto &[name, def] = history[0];
    ss << name << ":" << std::endl;
    printer_t::Print(ss, def);
    for (std::size_t i = 1; i < history.size(); ++i) {
      auto &[name, def] = history[i];
      ss << name << ":" << std::endl;
      printer_t::Diff(ss, history[i - 1].second, def);
    }

    ss << std::endl << "In the end:" << std::endl;
    printer_t::Print(ss, history.back().second);
    ss << std::endl;
    return ss.str();
  }
};

template<typename Logger, typename Next>
struct Defensive : Next {
  using pass_t = typename Next::pass_t;

  void RunPass(Circuit *circuit, pass_t pass) {
    auto name = this->PassName(pass);
    Logger::log("Going to run transformation", name);
    this->Next::RunPass(circuit, pass);
    Logger::log("Done. Running verify pass:");
    const auto &[status, msg, warnings] = VerifyCircuit(circuit);
    if (!status) {
      Logger::fail("Verify failed");
      Logger::fail(warnings);
      Logger::fail(msg);
      Logger::kill();
    }
    if (!warnings.empty()) {
      Logger::log(warnings);
    }
    Logger::log("Circuit is okay.");
  }
};

template<typename Next>
struct ManagerAPI : Next {
  using pass_t = typename Next::pass_t;

  void RunPass(Circuit *circuit, pass_t pass) {
    this->Next::RunPass(circuit, pass);
  }

  void Run(Circuit *circuit) {
    for (auto &[_, pass] : this->selected) {
      RunPass(circuit, pass);
    }
  }

  void AddPass(const std::string &name) {
    this->Next::AddPass(name);
  }

};

template<typename Logger>
using DebugOptimizer = ManagerAPI<Defensive<Logger, WithHistory<Manager>>>;
template<typename Logger>
using DefaultOptimizer = ManagerAPI<Defensive<Logger, Manager>>;

}  // namespace circuitous
