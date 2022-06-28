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

#include <circuitous/Run/State.hpp>
#include <circuitous/Util/Error.hpp>

namespace circ::run::trace
{
    struct MaybeValue : std::optional< std::string >
    {
        using parent_t = std::optional< std::string >;
        using parent_t::parent_t;

        MaybeValue(llvm::Optional< llvm::StringRef > w)
        {
            if (w)
                *this = w->str();
        }

        std::optional< llvm::APInt > cast(std::size_t size, std::size_t radix) const
        {
            if (!this->has_value())
                return {};
            return llvm::APInt(static_cast< uint32_t >(size),
                               **this,
                               static_cast< uint8_t >(radix));
        }
    };

    namespace native
    {

        struct Entry
        {
            std::unordered_map< std::string, value_type > regs;
            // TODO(lukas): Mapping via name can lead to some really weird bugs
            //              as there are no guarantees.
            std::unordered_map< std::string, value_type > advice;
            std::unordered_map< std::string, value_type > mem_hints;

            value_type ebit;
            value_type inst_bits;
            value_type timestamp;

            value_type get_mem_hint(const std::string &name) const;

            bool operator<(const Entry &other) const
            {
                if (!timestamp || !other.timestamp)
                    return false;
                return timestamp->ult(*(other.timestamp));
            }
        };

        struct Trace
        {
            uint64_t id;
            std::multiset< Entry > entries;
            using memory_t = std::unordered_map< uint64_t, value_type >;
            memory_t initial_memory;

            std::string to_string() const;

            auto begin() { return entries.begin(); }
            auto begin() const { return entries.begin(); }
            auto end() { return entries.begin(); }
            auto end() const { return entries.begin(); }
        };

        // Format:
        // {
        //  "id" = Integer
        //  "entries" : [
        //      {
        //          "timestamp" = Number as string in hex
        //          "ebit" = Number as string in hex
        //          "inst_bits" = Number as string in hex
        //          "regs" : [
        //              "RAX" = Number as string in hex
        //              ...
        //          ]
        //          "memory_hints" : [
        //              Packed represenation as number in hex
        //          ]
        //      }
        //  ]
        // }
        struct FromJSON
        {
            using self_t = FromJSON;
            using reg_sizes_map_t = std::unordered_map< std::string, std::size_t >;

            Trace trace;
            std::size_t inst_bits_size;
            reg_sizes_map_t reg_sizes;

            FromJSON(std::size_t inst_bits_size,
                     const reg_sizes_map_t &reg_sizes)
                : inst_bits_size(inst_bits_size), reg_sizes(reg_sizes)
            {}

            template< typename O >
            static auto unwrap(const O &obj)
            {
                check(obj) << "Trying to unwrap invalid object!";
                return *obj;
            }

            template< typename O >
            static auto convert(const O &obj, std::size_t size, std::size_t radix)
            {
                return MaybeValue{obj}.cast(size, radix);
            }

            auto open_json(const std::string &path) const
            {
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

            auto take() { return std::move(trace); }

            Trace::memory_t parse_memory(const auto &obj)
            {
                Trace::memory_t out;
                for (const auto &[raw_addr, raw_val] : obj)
                {
                    auto maybe_addr = convert(raw_addr.str(), 64, 16);
                    check(maybe_addr);
                    auto addr = maybe_addr->getLimitedValue();
                    auto val = unwrap(raw_val.getAsString());

                    check(val.size() % 2 == 0);

                    for (std::size_t i = 0; i < val.size() / 2; ++i)
                        out[addr + i] = convert(val.substr(i * 2, 2), 8, 16);
                }
                return out;
            }

            self_t &run(const std::string &path)
            {
                auto obj = open_json(path);
                trace.id = static_cast< uint64_t >(unwrap(obj.getInteger("id")));
                if (auto maybe_initial_memory = obj.getObject("initial_memory"))
                    trace.initial_memory = parse_memory(unwrap(maybe_initial_memory));
                for (const auto &entry : unwrap(obj.getArray("entries")))
                {
                    auto x = ParseEntry(inst_bits_size, reg_sizes)
                        .run(unwrap(entry.getAsObject()))
                        .take();
                    trace.entries.insert(std::move(x));
                }
                return *this;

            }

            struct ParseEntry
            {
                using self_t = ParseEntry;

                Entry entry;
                std::size_t inst_bits_size;
                const reg_sizes_map_t &reg_sizes;

                ParseEntry(std::size_t inst_bits_size, const reg_sizes_map_t &reg_sizes)
                    : inst_bits_size(inst_bits_size), reg_sizes(reg_sizes)
                {}

                std::optional< std::size_t > reg_size(llvm::StringRef reg)
                {
                    auto it = reg_sizes.find(reg.str());
                    if (it == reg_sizes.end())
                        return {};
                    return it->second;
                }

                self_t &run(const auto &obj)
                {
                    entry.timestamp = convert(obj.getString("timestamp"), 64, 16);
                    entry.ebit = convert(obj.getString("ebit"), 1, 16);
                    entry.inst_bits = construct_inst_bits(
                            unwrap(obj.getString("inst_bits")).str(), inst_bits_size, 16);

                    for (const auto &[reg, val] : unwrap(obj.getObject("regs")))
                        if (auto size = reg_size(reg))
                            entry.regs[reg.str()] = convert(val.getAsString(), *size, 16);

                    for (const auto &[mem_hint, val] : unwrap(obj.getObject("mem_hints")))
                        // TODO(lukas): Configurable memory hint size.
                        entry.mem_hints[mem_hint.str()] = convert(val.getAsString(), 208, 16);
                    return *this;
                }

                llvm::APInt construct_inst_bits(
                        const std::string &str, std::size_t size, uint8_t radix)
                {
                    std::string reordered;

                    check(str.size() >= 2) << str.size() << " content:" << str;
                    for (int i = static_cast< int >(str.size() - 2); i >=0; i -=2)
                        reordered += str.substr(static_cast< unsigned long >(i), 2);
                    return llvm::APInt(static_cast< uint32_t >(size), reordered, radix);
                }

                Entry take() { return std::move(entry); }
            };
        };

        static inline auto load_json(const std::string &path,
                                     std::size_t inst_bits_size,
                                     const FromJSON::reg_sizes_map_t &reg_sizes)
        {
            return FromJSON(inst_bits_size, reg_sizes).run(path).take();
        }
    } // namespace native

} // namespace circ::run::trace
