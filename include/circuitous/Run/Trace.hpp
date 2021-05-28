/*
 * Copyright (c) 2021-present Trail of Bits, Inc.
 */

#pragma once

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <gflags/gflags.h>
#include <glog/logging.h>

#include <llvm/Support/JSON.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
#pragma clang diagnostic pop

#include <fstream>

namespace circuitous::run::trace {

  struct Entry {
    // `trace_id` is used to identify traces from batches
    uint64_t trace_id = 0;

    // Actual timestamp of this `Entry`.
    uint64_t timestamp = 0;
    std::unordered_map<std::string, uint64_t> regs;
    std::string inst_bits;
    bool ebit;

    // TOOD(lukas): Once we support it
    std::unordered_map<std::string, uint64_t> hints;
    std::unordered_map<std::string, uint64_t> mem_hints;

    std::string to_string(uint8_t indent = 0, bool skip_header=true) const {
      std::stringstream ss;
      std::string prefix(indent * 2, ' ');

      if (!skip_header) {
        ss << prefix << "trace_id: " << trace_id << std::endl;
        ss << prefix << "......" << std::endl;
      }
      ss << prefix << "timestamp: " << timestamp << std::endl;
      ss << prefix << "ibits: " << inst_bits << std::endl;
      ss << prefix << "ebits: " << ebit << std::endl;
      ss << prefix << "regs:" << std::endl;
      for (auto &[reg, val] : regs) {
        ss << prefix << "  |- " << reg << " -> " << val << std::endl;
      }
      return ss.str();
    }

    llvm::APInt get_inst_bits(uint32_t size) const {
      return llvm::APInt(size, inst_bits, /* radix = */ 16U);
    }
    llvm::APInt get_ebit() const { return (ebit) ? llvm::APInt(1, 1) : llvm::APInt(1, 0); }
  };

  struct Trace {
    uint64_t trace_id;
    std::map<uint64_t, Entry> entries;

    std::string to_string() const {
      std::stringstream ss;
      ss << "trace_id: " << trace_id << std::endl;;
      ss << "......" << std::endl;
      for (auto &[_, entry] : entries) {
        ss << entry.to_string(1);
      }
      return ss.str();
    }
  };

  auto load_json(const std::string &path) {
    // Open JSON
    auto maybe_buff = llvm::MemoryBuffer::getFile(path);
    if (!maybe_buff) {
      LOG(FATAL) << "Error while opening JSON at: " << path;
    }

    // Parse JSON
    auto maybe_json = llvm::json::parse(maybe_buff.get()->getBuffer());
    if (!maybe_json) {
      LOG(FATAL) << "Error while parsing JSON at: " << path;
    }

    auto out = maybe_json.get().getAsObject();
    CHECK(out) << "Invalid loaded JSON object from: " << path;
    return *out;
  }

  template<typename T>
  auto unwrap(const T &obj) {
    CHECK(obj) << "Trying to unwrap invalid object!";
    return *obj;
  }

  template<typename O>
  auto get_entry(uint64_t trace_id, const O &entry) {
    Entry state;
    state.trace_id = trace_id;
    state.timestamp = static_cast<uint64_t>(unwrap(entry.getInteger("timestamp")));
    state.inst_bits = unwrap(entry.getString("inst_bits"));
    state.ebit = unwrap(entry.getBoolean("ebit"));
    for (auto &[reg, val] : unwrap(entry.getObject("regs"))) {
      auto raw = unwrap(val.getAsString());
      state.regs[reg.str()] = std::strtoull(raw.data(), nullptr, 10);
    }
    return state;
  }

  template<typename T>
  auto get_trace(uint64_t trace_id, const T &arr) {
    Trace out;
    out.trace_id = trace_id;

    for (auto &entry_ : arr) {
      auto entry_obj = unwrap(entry_.getAsObject());
      auto entry = get_entry(trace_id, entry_obj);
      out.entries.emplace(entry.timestamp, std::move(entry));
    }
    return out;
  }

  template<typename T>
  auto get_traces(const T &obj) {
    std::vector<Trace> out;
    for (auto &[trace_id_, traces] : obj) {
      auto trace_id = std::strtoull(trace_id_.str().c_str(), nullptr, 10);
      out.push_back(get_trace(trace_id, unwrap(traces.getAsArray())));
    }
    return out;
  }

} // namespace circuitous::run::trace