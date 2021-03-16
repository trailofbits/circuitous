/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <string>
#include <unordered_map>
#include <unordered_set>

#include <circuitous/IR/Verify.hpp>

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

struct KnownPasses {
  using pass_t = bool(*)(Circuit *);
  using storage_t  = std::unordered_map<std::string, bool(*)(Circuit *)>;

  static const inline storage_t passes = {
    {"popcount2parity", ConvertPopCountToParity},
    {"reducepopcount", StrengthReducePopulationCount},
    {"extractcommon", ExtractCommonTopologies},
    {"mergehints", MergeHints},
    {"mergebybytes", MergeByBytes}
  };

  static pass_t Get(const std::string &name) {
    auto it = passes.find(name);
    CHECK(it != passes.end());
    return it->second;
  }

  static void Run(Circuit *circuit, const std::string &name) {
    Get(name)(circuit);
  }
};


struct OptManager : KnownPasses {
  using KnownPasses::Run;

  std::unordered_set<pass_t> selected;

  void AddPass(const std::string &name) {
    selected.insert(Get(name));
  }

  std::vector<pass_t> Order() {
    LOG(FATAL) << "Implement ordering!";
  }

  void Run(Circuit *circuit) {
    for (auto pass : selected) {
      pass(circuit);
    }
  }
};

template<typename Logger>
struct DefensiveOptManger : OptManager {

  void Run(Circuit *circuit, const std::string &name) {
    Logger::log("Going to run transformation", name);
    this->OptManager::Run(circuit, name);
    Logger::log("Done. Running verify pass:");
    const auto &[status, msg] = VerifyCircuit(circuit);
    if (!status) {
      Logger::fail("Verify failed");
      Logger::fail(msg);
      Logger::kill();
    }
    Logger::log("Circuit is okay.");
  }
};



}  // namespace circuitous
