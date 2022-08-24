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

#include <circuitous/IR/Trace.hpp>

#include <circuitous/Run/State.hpp>

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

        struct Trace
        {
            using Entry = std::map< std::string, std::optional< llvm::APInt > >;
            using memory_t = std::unordered_map< uint64_t, value_type >;

            uint64_t id;
            std::vector< Entry > entries;
            memory_t initial_memory;

            std::string to_string() const;

            auto begin() { return entries.begin(); }
            auto begin() const { return entries.begin(); }
            auto end() { return entries.begin(); }
            auto end() const { return entries.begin(); }

            std::size_t size() const { return entries.size(); }
            auto &operator[](std::size_t idx) { return entries[idx]; }
            const auto &operator[](std::size_t idx) const { return entries[idx]; }
        };

        // Format:
        // {
        //  "id" = Integer
        //  "entries" : [
        //      {
        //          "timestamp" = Number as string in hex
        //          "error_flag" = Number as string in hex
        //          "instruction_bits" = Number as string in hex
        //          "regs" : [
        //              "RAX" = Number as string in hex
        //              ...
        //          ]
        //      }
        //  ]
        // }
        struct FromJSON
        {
            using self_t = FromJSON;
            using reg_sizes_map_t = std::unordered_map< std::string, std::size_t >;

            Trace trace;
            reg_sizes_map_t reg_sizes;

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
                    auto x = ParseEntry().run(unwrap(entry.getAsObject())).take();
                    trace.entries.emplace_back(std::move(x));
                }
                return *this;

            }

            struct ParseEntry
            {
                using self_t = ParseEntry;

                Trace::Entry entry;

                auto convert( llvm::Optional< llvm::StringRef > &&src )
                {
                    check( src );
                    auto size = src->size() * 4;
                    return FromJSON::convert(src, size, 16);
                }


                self_t &run(const auto &obj)
                {
                    entry["timestamp"] = convert(obj.getString("timestamp"));
                    entry["error_flag"] = convert(obj.getString("error_flag"));
                    entry["instruction_bits"] = construct_inst_bits(
                            unwrap(obj.getString("instruction_bits")).str(), 15 * 8, 16);

                    for (const auto &[reg, val] : unwrap(obj.getObject("regs")))
                        entry[reg.str()] = convert(val.getAsString());
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

                Trace::Entry take() { return std::move(entry); }
            };
        };

        static inline auto load_json(const std::string &path)
        {
            return FromJSON().run(path).take();
        }

        static inline std::unordered_map< Operation *, value_type > make_step_trace(
                Circuit *circuit,
                const Trace::Entry &in,
                const Trace::Entry &out)
        {
            using VTrace = ValuedTrace< value_type >;
            auto input = VTrace(circ::Trace::make(circuit), in)
                .specialize(circuit, input_leaves_ts{});
            auto output = VTrace(circ::Trace::make(circuit), out)
                .specialize(circuit, output_leaves_ts{});

            for (const auto &[k, v] : output)
            {
                check(!input.count(k));
                input[k] = v;
            }

            for (auto &[k, v] : input)
            {
                // Coercion of sizes to perfectly fit registers is required (when loading,
                // some approximation is used to decouple loading code from Circuit itself).
                if (v)
                    v = std::make_optional(v->zextOrTrunc(k->size));
            }
            return input;
        }

    } // namespace native

} // namespace circ::run::trace
