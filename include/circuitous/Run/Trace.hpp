/*
 * Copyright (c) 2021-present Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Support/Check.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Warnings.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/Support/JSON.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <fstream>

namespace circ::run::trace {

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
        std::unordered_map<std::string, std::string> mem_hints;

        std::unordered_map<uint64_t, std::string> initial_memory;

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
            for (const auto &[reg, val] : regs) {
                ss << prefix << "  |- " << reg << " -> " << val << std::endl;
            }
            return ss.str();
        }

        std::optional< llvm::APInt > get_mem_hint(const std::string &key) const {
            // TODO(lukas): I do not want to include from `IR` here.
            //              It would probably help to pull out all constants into
            //              separate lightweight header.
            auto it = mem_hints.find(key);
            if (it == mem_hints.end()) {
                return {};
            }
            return { llvm::APInt(208, it->second, 10) };
        }

        llvm::APInt get_inst_bits(uint32_t size) const {
            std::string reoredered;
            check(inst_bits.size() >= 2);
            for (int i = static_cast< int >(inst_bits.size() - 2); i >= 0; i -= 2)
            {
                reoredered += inst_bits.substr(static_cast< unsigned long >(i), 2);
            }
            return llvm::APInt(size, reoredered, /* radix = */ 16U);
        }
        llvm::APInt get_ebit() const { return (ebit) ? llvm::APInt(1, 1) : llvm::APInt(1, 0); }
        llvm::APInt get_timestamp() const {
            return llvm::APInt(64, timestamp);
        }
    };

    struct Trace {
        uint64_t trace_id;
        std::map<uint64_t, Entry> entries;

        std::string to_string() const {
            std::stringstream ss;
            ss << "trace_id: " << trace_id << std::endl;;
            ss << "......" << std::endl;
            for (const auto &[_, entry] : entries) {
                ss << entry.to_string(1);
            }
            return ss.str();
        }
    };

    auto load_json(const std::string &path) {
        // Open JSON
        auto maybe_buff = llvm::MemoryBuffer::getFile(path);
        check(maybe_buff) << "Error while opening JSON at: " << path;

        // Parse JSON
        auto maybe_json = llvm::json::parse(maybe_buff.get()->getBuffer());
        check(maybe_json) << "Error while parsing JSON at: " << path;

        auto out = maybe_json.get().getAsObject();
        check(out) << "Invalid loaded JSON object from: " << path;
        return *out;
    }

    template<typename T>
    auto unwrap(const T &obj) {
        check(obj) << "Trying to unwrap invalid object!";
        return *obj;
    }

    template<typename O>
    auto get_entry(uint64_t trace_id, const O &entry) {
        Entry state;
        state.trace_id = trace_id;
        state.timestamp = static_cast<uint64_t>(unwrap(entry.getInteger("timestamp")));
        state.inst_bits = unwrap(entry.getString("inst_bits"));
        state.ebit = unwrap(entry.getBoolean("ebit"));
        for (const auto &[reg, val] : unwrap(entry.getObject("regs"))) {
            auto raw = unwrap(val.getAsString());
            state.regs[reg.str()] = std::strtoull(raw.data(), nullptr, 10);
        }

        for (const auto &[mem_hint, val] : unwrap(entry.getObject("mem_hints"))) {
            state.mem_hints[mem_hint.str()] = unwrap(val.getAsString());
        }

        // There are some optional args, so if we are done we can return
        if (!entry.getObject("memory")) {
            return state;
        }

        for (const auto &[addr_, mem] : unwrap(entry.getObject("memory"))) {
            auto addr = std::strtoull(addr_.str().c_str(), nullptr, 16);
            state.initial_memory[addr] = unwrap(mem.getAsString());
        }
        return state;
    }

    template<typename T>
    auto get_trace(uint64_t trace_id, const T &arr) {
        Trace out;
        out.trace_id = trace_id;

        for (const auto &entry_ : arr) {
            auto entry_obj = unwrap(entry_.getAsObject());
            auto entry = get_entry(trace_id, entry_obj);
            out.entries.emplace(entry.timestamp, std::move(entry));
        }
        return out;
    }

    template<typename T>
    auto get_traces(const T &obj) {
        std::vector<Trace> out;
        for (const auto &[trace_id_, traces] : obj) {
            auto trace_id = std::strtoull(trace_id_.str().c_str(), nullptr, 10);
            out.push_back(get_trace(trace_id, unwrap(traces.getAsArray())));
        }
        return out;
    }

} // namespace circ::run::trace
