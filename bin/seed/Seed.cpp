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

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/Support/MemoryBuffer.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <remill/Arch/Arch.h>
#include <remill/Arch/Name.h>
#include <remill/Arch/Instruction.h>
#include <remill/BC/Compat/Error.h>
#include <remill/BC/Util.h>
#include <remill/OS/OS.h>

DECLARE_string(arch);
DECLARE_string(os);

DEFINE_string(config, "", "Path to config file");
DEFINE_string(file, "", "Binary file to load from (separated by ,");
DEFINE_string(out, "", "File to store plain bytes to.");
DEFINE_string(dbg, "", "File to store plain bytes to.");
DEFINE_string(filter, "", "File that contains allowed opcodes.");

// TODO(lukas): Filter by instruction.
// TODO(lukas): Paralelism
// TODO(lukas): Allow multiple sources
// TODO(lukas): Allow also lift?

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

std::string dbg_dump_bytes(const llvm::StringRef &bytes) {
  std::stringstream ss;
  for (auto c : bytes) {
    ss << std::setw(2) << std::setfill('0') << std::hex
       << static_cast< unsigned >(static_cast< uint8_t >(c));
  }
  return ss.str();
}


struct rinst_fn_comparator {
  auto operator()(const remill::Instruction &a, const remill::Instruction &b) const {
    return std::less< std::string >{}(a.function, b.function);
  }
};

struct rinst_bytes_comparator {
  using T = const remill::Instruction &;
  auto operator()(T a, T b) const {
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

  Acceptor() = default;
  Acceptor(std::unordered_set< std::string > allowed_)
    : allowed(std::move(allowed_))
  {
    CHECK(!allowed.empty());
  }

  std::string get_iclass(const std::string &iform) {
    llvm::StringRef sref{iform};

    sref.consume_front("REPNE_");
    sref.consume_front("REP_");

    auto [prefix, _] = sref.split('_');
    return prefix.str();
  }

  bool allowed_iform(const std::string &iform) {
    return allowed.count(get_iclass(iform));
  }

  bool already_parsed(const circ::shadowinst::Instruction &nshadow,
                      const remill::Instruction &rinst)
  {
    auto iclass = get_iclass(rinst.function);
    auto size = rinst.bytes.size();
    for (const auto &[shadow, iform] : shadows[iclass][size])
      if (nshadow == shadow)
        return false;
    return true;
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
    if (!allowed_iform(rinst.function))
      return false;

    auto nshadow = circ::InstructionFuzzer{ rinst.arch, rinst }.FuzzOps();
    if (!already_parsed(nshadow, rinst))
      return false;

    add_to_cache(std::move(nshadow), rinst);
    return true;
  }

};

template< typename H >
struct Parsed_ {
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

  void emplace(rinst_t inst) {
    storage.emplace(std::move(inst));
  }

  template< typename OStream >
  void write_bytes_into(OStream &os) {
    for (const auto x : storage)
      os << dbg_dump_bytes(x.bytes);
    os << std::endl;
  }

  template< typename OStream >
  void write_all(OStream &os) {
    for (const auto &inst : storage)
      os << dbg_dump_bytes(inst.bytes) << " " << inst.function << std::endl;
  }
};

using Parsed = Parsed_< rinst_fn_comparator >;


template< typename R >
struct Parser {
  using self_t = Parser &;
  using rinst_t = remill::Instruction;
  using rctx_t = const remill::Arch;

  rctx_t &rctx;

  llvm::StringRef buffer;
  R parsed;

  Acceptor &acceptor;

  Parser(rctx_t &rctx_, Acceptor &acc_) : rctx(rctx_), acceptor(acc_) {}

  self_t &provide(llvm::StringRef what) {
    buffer = std::move(what);
    return *this;
  }
  R take() { return std::move(parsed); }

  self_t &run() {
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

std::string dbg_dump(const Parsed &parsed) {
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

int main(int argc, char *argv[]) {
  google::ParseCommandLineFlags(&argc, &argv, true);
  google::InitGoogleLogging(argv[0]);

  CHECK(FLAGS_file.empty() != FLAGS_config.empty());
  CHECK(!FLAGS_out.empty());
  CHECK(!FLAGS_dbg.empty());

  std::ofstream out(FLAGS_out);
  std::ofstream dbg(FLAGS_dbg);

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
  Parser< Parsed > parser{ *owning_arch_ptr, acceptor };

  uint32_t idx = 0;
  for (auto file : input_list) {
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
