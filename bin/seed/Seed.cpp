/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#include <algorithm>
#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <set>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <circuitous/Util/Logging.hpp>
#include <circuitous/Lifter/Shadows.hpp>
#include <circuitous/Fuzz/InstructionFuzzer.hpp>
#include <circuitous/Support/Ciff.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/Support/MemoryBuffer.h>
#include <gflags/gflags.h>
#include <glog/logging.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <remill/Arch/Arch.h>
#include <remill/Arch/Name.h>
#include <remill/Arch/Instruction.h>
#include <remill/BC/Compat/Error.h>
#include <remill/BC/Util.h>
#include <remill/OS/OS.h>

DEFINE_string(arch, "", "");
DEFINE_string(os, "", "");

DEFINE_bool(prune, false, "Prune mode - take existing dbg format and prune,");
DEFINE_string(prune_spec, "", "Path to spec file to prune by.");
DEFINE_string(prune_in, "", "Path to dbg format to be prunned.");

DEFINE_string(config, "", "Path to config file");
DEFINE_string(file, "", "Binary file to load from (separated by ,");
DEFINE_string(out, "", "File to store plain bytes to.");
DEFINE_string(cif, "", "File to store dbg format to.");
DEFINE_string(filter, "", "File that contains allowed opcodes.");

// TODO(lukas): Filter by instruction.
// TODO(lukas): Paralelism
// TODO(lukas): Allow multiple sources
// TODO(lukas): Allow also lift?

namespace prune
{

    std::set< std::string > parse_spec(llvm::StringRef line)
    {
        std::set< std::string > out;
        auto [_, suffix] = line.split(':');
        llvm::SmallVector< llvm::StringRef, 16 > entries;
        suffix.split(entries, ',');
        for (auto entry : entries)
        {
            if (entry[0] == 'x')
                out.insert("0F" + entry.substr(1, 2).upper());
            else
                out.insert(entry.substr(0, 2).upper());
        }
        return out;
    }

      using bytes_and_opcode_t = std::tuple< std::string, std::string >;
      using dbg_format_t = std::vector< bytes_and_opcode_t >;

      dbg_format_t parse_dbg(const std::string &file)
      {
          dbg_format_t out;
          std::ifstream input(file);
          for (std::string line; std::getline(input, line);)
          {
              auto [bytes, iform] = llvm::StringRef(line).split(' ');
              out.emplace_back(bytes.str(), iform.str());
          }
          return out;
      }

    struct Spec
    {
        std::set< std::string > allowed;

        void allow(const std::set< std::string > &to_allow)
        {
            for (const auto &x : to_allow)
            {
                std::cout << "Allowing: " << x << std::endl;
                allowed.insert(llvm::StringRef(x).upper());
            }
        }

        bool is_allowed(const std::string &other)
        {
            return allowed.count(llvm::StringRef(other).upper());
        }

        static Spec load(const std::string &file)
        {
            Spec out;
            std::ifstream input(file);
            for (std::string line; std::getline(input, line);)
                out.allow(parse_spec(line));
            return out;
        }
    };

    struct X86Prefixes
    {
        static inline std::set< std::string > bytes = {
            "F0", // LOCK
            "F2", // REPN(E/Z)
            "F3", // REP(E/Z)
            "2E", // CS
            "36", // SS
            "3E", // DS
            "26", // ES
            "64", // FS
            "65", // GS
            "2E", // Branch not taken
            "3E", // Branch taken
            "66", // Operand size prefix
            "67", // Address size prefix
        };

        static constexpr uint8_t max = 2;
    };

    template< typename Prefixes >
    struct Exec
    {
        Spec spec;
        //std::stringstream dbg;

        Exec() = default;
        Exec(Spec spec_) : spec(std::move(spec_)) {}

        bool is_valid(std::string_view bytes, uint8_t prefixes = 0)
        {
            if (spec.allowed.empty())
                return true;
            CHECK(bytes.size() != 0);
            auto front = std::string(bytes.substr(0, 2));

            auto escaped = [&]() -> std::string {
                if (bytes.size() >= 4)
                  return std::string(bytes.substr(0, 4));
                return "XX";
            }();
            if (Prefixes::bytes.count(front))
            {
                ++prefixes;
                if (prefixes > Prefixes::max)
                {
                    //dbg << "  Denied: prefix count." << std::endl;
                    return false;
                }
                return is_valid(bytes.substr(2), prefixes);
            }
            if (spec.is_allowed(front) || (spec.is_allowed(escaped) && front == "0F"))
            {
                //dbg << "  Allowed." << std::endl;
                return true;
            }

            //dbg << "  Denied: prefix not found in spec." << std::endl;
            return false;
        }

        dbg_format_t filter(const dbg_format_t &entries)
        {
            //dbg << "Allowed: " << spec.allowed.size() << " opcodes." << std::endl;
            dbg_format_t out;
            for (const auto &[data, iform] : entries)
            {
                //dbg << "Handling: " << data << " " << iform << std::endl;
                if (is_valid(data))
                    out.emplace_back(data, iform);
            }
            //std::cout << this->dbg.str();
            return out;
        }
    };

    void store(const dbg_format_t &entries, const std::string &file)
    {
        std::ofstream out(file);
        for (const auto &[bytes, iform]: entries)
            out << bytes << " " << iform << std::endl;
    }

    int exec()
    {
        CHECK(!FLAGS_prune_spec.empty());
        CHECK(!FLAGS_prune_in.empty());
        CHECK(!FLAGS_cif.empty());

        auto entries = parse_dbg(FLAGS_prune_in);
        auto out = Exec< X86Prefixes >(Spec::load(FLAGS_prune_spec)).filter(entries);
        store(out, FLAGS_cif);

        return 0;
    }

} // namespace prune

void dbg_dump_buffer(const llvm::StringRef &buffer)
{
    std::stringstream ss;
    auto mem = 0;
    for (auto x : buffer) {
        ss << std::setw(2) << std::setfill('0') << std::hex
           << static_cast< unsigned >(static_cast< uint8_t >(x));
        ++mem;
        if (mem % 16 == 0)
            ss << std::endl;
        else
            ss << " ";
    }
}

std::string dbg_dump_bytes(const llvm::StringRef &bytes)
{
    std::stringstream ss;
    for (auto c : bytes) {
        ss << std::setw(2) << std::setfill('0') << std::hex
           << static_cast< unsigned >(static_cast< uint8_t >(c));
    }
    return ss.str();
}


struct rinst_fn_comparator
{
    auto operator()(const remill::Instruction &a, const remill::Instruction &b) const
    {
        return std::less< std::string >{}(a.function, b.function);
    }
};

struct rinst_bytes_comparator
{
    using T = const remill::Instruction &;
    auto operator()(T a, T b) const
    {
        return a.bytes == b.bytes;
    }
};

// Must be default constructible
struct Acceptor
{
    using seen_t = std::unordered_set< std::string >;

    using entry_t = std::tuple< circ::shadowinst::Instruction, std::string >;
    using bytes_map_t = std::unordered_map< uint64_t, std::vector< entry_t > >;
    using iclass_cache_t = std::unordered_map< std::string, bytes_map_t >;

    iclass_cache_t shadows;
    std::unordered_set< std::string > allowed;
    seen_t seen;
    prune::Exec< prune::X86Prefixes > spec;

    Acceptor() = default;
    Acceptor(std::unordered_set< std::string > allowed_)
        : allowed(std::move(allowed_))
    {
        CHECK(!allowed.empty());
    }

    std::string get_iclass(const std::string &iform)
    {
        llvm::StringRef sref{iform};

        sref.consume_front("REPNE_");
        sref.consume_front("REP_");

        auto [prefix, _] = sref.split('_');
        return prefix.str();
    }

    bool allowed_iform(const std::string &iform)
    {
        return allowed.count(get_iclass(iform));
    }

    bool already_parsed(const circ::shadowinst::Instruction &nshadow,
            const remill::Instruction &rinst)
    {
        auto remove_shade = [&](const auto &s_inst) {
            return circ::shadowinst::remove_shadowed(s_inst, rinst.bytes);
        };

        auto iclass = get_iclass(rinst.function);
        auto size = rinst.bytes.size();
        for (const auto &[shadow, iform] : shadows[iclass][size])
            if (nshadow == shadow && remove_shade(nshadow) == remove_shade(shadow))
                return true;
        return false;
    }

    void add_to_cache(circ::shadowinst::Instruction nshadow,
            const remill::Instruction &rinst)
    {
        auto iclass = get_iclass(rinst.function);
        auto size = rinst.bytes.size();
        shadows[iclass][size].emplace_back(std::move(nshadow), rinst.function);
    }

    bool should_accept(const remill::Instruction &rinst)
    {
        if (seen.count(rinst.bytes))
            return false;
        seen.insert(rinst.bytes);
        if (!spec.is_valid(dbg_dump_bytes(rinst.bytes)))
            return false;

        if (!allowed_iform(rinst.function))
            return false;

        auto nshadow = circ::InstructionFuzzer{ rinst.arch, rinst }.fuzz_ops();
        if (already_parsed(nshadow, rinst))
            return false;

        add_to_cache(std::move(nshadow), rinst);
        return true;
    }

};

template< typename H >
struct Parsed_
{
    using self_t = Parsed_< H >;
    using rinst_t = remill::Instruction;
    using storage_t = std::multiset< rinst_t, H >;
    storage_t storage;

    Parsed_() = default;
    Parsed_(const storage_t &ns) : storage(ns) {}

    auto *operator->() { return &storage; }
    const auto *operator->() const { return &storage; }

    const auto &get() const { return storage; }
    storage_t take() { return std::move(storage); }

    void emplace(rinst_t inst)
    {
        storage.emplace(std::move(inst));
    }

    template< typename OStream >
    void write_bytes_into(OStream &os)
    {
        for (const auto x : storage)
            os << dbg_dump_bytes(x.bytes);
        os << std::endl;
    }

    template< typename OStream >
    void write_all(OStream &os)
    {
        for (const auto &inst : storage)
            os << dbg_dump_bytes(inst.bytes) << " " << inst.function << std::endl;
    }
};

using Parsed = Parsed_< rinst_fn_comparator >;


template< typename R >
struct Parser
{
    using self_t = Parser &;
    using rinst_t = remill::Instruction;
    using rctx_t = const remill::Arch;

    rctx_t &rctx;

    llvm::StringRef buffer;
    R parsed;

    Acceptor &acceptor;

    Parser(rctx_t &rctx_, Acceptor &acc_) : rctx(rctx_), acceptor(acc_) {}

    self_t &provide(llvm::StringRef what)
    {
        buffer = std::move(what);
        return *this;
    }

    R take() { return std::move(parsed); }

    self_t &run()
    {
        while (!buffer.empty()) {
            rinst_t inst;
            if (rctx.DecodeInstruction(0, buffer.substr(0, 0x20), inst))
                if (acceptor.should_accept(inst))
                    parsed.emplace(std::move(inst));
            buffer = buffer.drop_front(1);
        }
        return *this;
    }
};

std::string dbg_dump(const Parsed &parsed)
{
    std::stringstream ss;
    for (const auto &a : parsed.get())
        ss << dbg_dump_bytes(a.bytes)
           << " " << a.function
           << std::endl;
    return ss.str();
}

std::vector< llvm::StringRef > split(const std::string &str, char delim)
{
    llvm::SmallVector< llvm::StringRef, 16 > out;
    llvm::StringRef(str).split(out, delim);
    return std::vector< llvm::StringRef >(out.begin(), out.end());
}

template< typename C = std::vector< std::string > >
C load_config(const std::string &config)
{
    C out;
    std::ifstream file(config);
    for (std::string line; std::getline(file, line);)
        out.insert(out.end(), std::move(line));
    return out;
}

int main(int argc, char *argv[])
{
    google::ParseCommandLineFlags(&argc, &argv, true);
    google::InitGoogleLogging(argv[0]);

    if (FLAGS_prune)
        return prune::exec();

    CHECK(FLAGS_file.empty() != FLAGS_config.empty());
    CHECK(!FLAGS_out.empty());
    CHECK(!FLAGS_cif.empty());

    std::ofstream out(FLAGS_out);
    std::ofstream dbg(FLAGS_cif);

    auto input_list = [&]() -> std::vector< std::string > {
        if (!FLAGS_file.empty())
        {
            std::vector< std::string > out;
            for (auto &name : split(FLAGS_file, ','))
                out.push_back(name.str());
            return out;
        }
        // It is asserted exactly one of these option is selected.
        return load_config(FLAGS_config);
    }();
    std::cout << "Number of input binaries: " << input_list.size() << std::endl;

    auto olctx = std::make_shared< llvm::LLVMContext >();
    auto arch_name = remill::GetArchName(FLAGS_arch);
    auto os_name = remill::GetOSName(FLAGS_os);
    auto owning_arch_ptr = remill::Arch::Build(olctx.get(), os_name, arch_name);
    auto owning_module_pre = remill::LoadArchSemantics(owning_arch_ptr.get());

    Acceptor acceptor(load_config< std::unordered_set< std::string > >(FLAGS_filter));
    if (!FLAGS_prune_spec.empty())
        acceptor.spec = prune::Exec< prune::X86Prefixes >(prune::Spec::load(FLAGS_prune_spec));

    Parser< Parsed > parser{ *owning_arch_ptr, acceptor };

    uint32_t idx = 0;
    for (auto file : input_list)
    {
        auto before = parser.parsed->size();
        auto begin = std::chrono::steady_clock::now();
        std::cout << "[ " << ++idx << " / " << input_list.size() << " ]"
                  << " ... "
                  << "Parsing: " << file << std::endl;
        auto maybe_buff = llvm::MemoryBuffer::getFile(file, -1, false);
        if (remill::IsError(maybe_buff))
            LOG(FATAL) << remill::GetErrorString(maybe_buff) << std::endl;
        auto buff = remill::GetReference(maybe_buff)->getBuffer();

        parser.provide(buff).run();

        auto end = std::chrono::steady_clock::now();
        std::chrono::duration< double > diff = end - begin;
        std::cout << " ... done,"
                  << parser.parsed->size() << "( +" << parser.parsed->size() - before << " )"
                  << " and took: " << diff.count() << " sec." << std::endl;
    }
    auto filtered = parser.take();
    std::cout << "Parsing done, proceeding to writing result.";
    filtered.write_bytes_into(out);
    filtered.write_all(dbg);
    std::cout << "Done." << std::endl;
    return 0;
}
