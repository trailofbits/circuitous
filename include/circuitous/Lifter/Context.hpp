/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <bitset>
#include <map>
#include <memory>

#include <circuitous/Lifter/Shadows.hpp>

#include <remill/Arch/Arch.h>
#include <remill/Arch/Name.h>
#include <remill/BC/Util.h>
#include <remill/OS/OS.h>

namespace llvm {
  class Module;
  class LLVMContext;
  class Function;
}

namespace remill {
  class Operand;
}

namespace circ {

  struct Names {
    // TODO(lukas): This may deserve its own header, so we do not have to include
    //              everything from here if only metadata names are requested.
    struct meta {
      static constexpr const char *verify_args = "circuitous.verify_fn_args";
      static constexpr const char *dst_reg =     "circuitous.dst.reg";
    };

    static constexpr const char *in = "in";
    static constexpr const char *out = "out";
    static constexpr const char sep = '.';

    static constexpr const char *reg_type_ = "register";
    static constexpr const char *mem_type  = "memory";
    static constexpr const char *flag = "flag";
    static constexpr const char *bits = "bits";
    static constexpr const char *timestamp_type = "timestamp";

    enum class mut_t : uint8_t {
      in = 0x1,
      out = 0x2
    };

    static constexpr uint8_t tied_c = 0x1 + 0x2;

    using meta_t = std::vector<std::string>;
    // [type, name, i/o (mut_t), metdata]
    using parsed_t = std::tuple<std::string, std::string, std::string, std::string>;

    static uint8_t _cast_mut(const std::string &mut) {
      return _cast_mut(llvm::StringRef{mut});
    }

    static uint8_t _cast_mut(llvm::StringRef mut) {
      if (mut == "in")   return static_cast<uint8_t>(mut_t::in);
      if (mut == "out")  return static_cast<uint8_t>(mut_t::out);
      LOG(FATAL) << "Unrecognized mut_t";
    }

    static auto parse(llvm::Value *arg) {
      CHECK(arg->hasName());
      auto [type, type_tail] = arg->getName().split(sep);
      auto [name, name_tail] = type_tail.split(sep);
      auto [mut, mut_tail] = name_tail.split(sep);
      return std::make_tuple(
          std::move(type), std::move(name), std::move(mut), std::move(mut_tail) );
    }

    static auto type(llvm::Value *arg) {
      const auto &[type, _, _1, _2] = parse(arg);
      return type;
    }

    static bool is_reg(llvm::Value *arg) { return type(arg) == reg_type_; }

    static bool is_in_reg(llvm::Value *arg) {
      CHECK(arg->hasName());
      const auto &[type, _, mut, _1] = parse(arg);
      return type == reg_type_ && mut == in;
    }

    static bool is_out_reg(llvm::Value *arg) {
      CHECK(arg->hasName());
      const auto &[type, _, mut, _1] = parse(arg);
      return type == reg_type_ && mut == out;
    }

    using cstr_r = const std::string &;
    static std::string build(cstr_r type, cstr_r base, cstr_r mut) {
      return type + sep + base + sep + mut + sep + "_";
    }

    static std::string instruction_bits() {
      return std::string(bits) + sep + "ibit" + sep + in + sep + "_";
    }

    static std::string as_in_reg(llvm::StringRef name) {
      return std::string(reg_type_) + sep + name.str() + sep + in + sep + "_";
    }

    static std::string as_out_reg(llvm::StringRef name) {
      return std::string(reg_type_) + sep + name.str() + sep + out + sep + "_";
    }

    static auto name(llvm::Value *arg) {
      return std::get<1>(parse(arg));
    }

    static llvm::Argument *dual_reg(llvm::Function *fn, llvm::Value *val) {
      const auto &[type, _, _1, _2] = parse(val);
      CHECK(type == reg_type_) << "Cannot get dual_reg from thing that is not a reg!";

      for (auto &arg : fn->args()) {
        CHECK(arg.hasName());
        if (are_tied(&arg, val)) {
          return &arg;
        }
      }
      LOG(FATAL) << "Dit not find dual_reg to " << remill::LLVMThingToString(val);
    }

    static bool are_tied(llvm::Value *lhs, llvm::Value *rhs) {
      if (!lhs->hasName() || !rhs->hasName()) {
        return false;
      }
      const auto &[ltype, lname, lmut, _] = parse(lhs);
      const auto &[rtype, rname, rmut, _1] = parse(rhs);
      return ltype == rtype && lname == rname && _cast_mut(lmut) + _cast_mut(rmut) == tied_c;
    }
  };

  struct Ctx {
    using arch_ptr_t = remill::Arch::ArchPtr;
    using reg_ptr_t = const remill::Register *;

    std::shared_ptr<llvm::LLVMContext> _llvm_context = std::make_shared<llvm::LLVMContext>();
    arch_ptr_t _arch;
    std::unique_ptr<llvm::Module> _module;
    std::vector<reg_ptr_t> _regs;

    auto llvm_ctx() { return _llvm_context.get(); }
    auto arch() { return _arch.get(); }
    auto module() { return _module.get(); }
    auto &regs() { return _regs; }

    auto state_ptr_type() const { return _arch->StatePointerType(); }
    auto memory_ptr_type() const { return _arch->MemoryPointerType(); }

    static auto make_arch(
      llvm::LLVMContext *ctx, const std::string &os_name, const std::string &arch_name)
    {
      auto os = remill::GetOSName(os_name);
      auto arch = remill::GetArchName(arch_name);
      return remill::Arch::Build(ctx, os, arch);
    }

    auto clean_module(const std::unordered_set<llvm::Function *> &keep) {
      std::vector<llvm::Function *> to_erase;
      for (auto &fn : *module()) {
        if (keep.count(&fn)) {
          continue;
        }
        if (fn.isDeclaration()) {
          continue;
        }

        if (fn.getName().startswith("__remill") || fn.getName().startswith("__circuitous"))
        {
          continue;
        }
        to_erase.push_back(&fn);
      }
      for (auto fn : to_erase) {
        fn->replaceAllUsesWith(llvm::UndefValue::get(fn->getType()));
        fn->eraseFromParent();
      }

      std::vector<llvm::GlobalValue *> gv_to_erase;
      for (auto &gv : module()->globals()) {
        if (gv.getName().startswith("ISEL_")) {
          gv_to_erase.push_back(&gv);
        }
      }
      for (auto gv : gv_to_erase) { gv->eraseFromParent(); }
    }

    Ctx(const std::string &os_name, const std::string &arch_name)
        : _arch(make_arch(_llvm_context.get(), os_name, arch_name)),
          _module(remill::LoadArchSemantics(arch()))
    {
      _arch->ForEachRegister([&](reg_ptr_t reg_) {
        if (auto reg = reg_->EnclosingRegister(); reg == reg_) {
          _regs.push_back(reg);
        }
      });
    }
  };
  using CtxRef = Ctx &;

  // Maximum size of encoding of instruction on amd64
  // TODO(lukas): Make configurable based on arch
  static constexpr const uint32_t kMaxNumInstBits = 15 * 8;

  using InstructionEncoding = std::bitset<kMaxNumInstBits>;

  // Groups together instructions that share a common semantic, and information
  // that is common across all of the encodings of this instruction.
  struct InstructionSelection {

    // Decoded instructs that all share a common ISEL, i.e. instruction semantic.
    std::vector<remill::Instruction> instructions;

    // Bitvector representations of the encodings of the instructions from
    // `instructions`.
    std::vector<InstructionEncoding> encodings;
    std::vector<shadowinst::Instruction> shadows;
    std::vector<llvm::Function *> lifted_fns;

    static auto RemillBytesToEncoding(const std::string &bytes) {
      InstructionEncoding encoding;
      std::size_t i = 0u;
      for (char byte_ : std::string(bytes.rbegin(), bytes.rend())) {
        const auto byte = static_cast<uint8_t>(byte_);
        for (auto b = 0u; b < 8u; ++b, ++i) {
          if ((byte >> b) & 1u) {
            encoding.set(i);
          }
        }
      }
      return encoding;
    }

    // InstructionSelection is sane if all vectors have the same length.
    void AssertIntegrity() const {
      CHECK(instructions.size() == encodings.size() &&
            instructions.size() == shadows.size() &&
            instructions.size() == lifted_fns.size());
    }

    void PartialAdd(remill::Instruction rinst, InstructionEncoding encoding) {
      AssertIntegrity();

      instructions.push_back(std::move(rinst));
      encodings.push_back(std::move(encoding));

      // These will be filled by other part of the pipeline!
      shadows.emplace_back();
      lifted_fns.emplace_back();
    }
  };
  using InstSelections = std::vector<InstructionSelection>;

  struct ISEL_view {
    const remill::Instruction &instruction;
    const InstructionEncoding &encoding;
    const shadowinst::Instruction &shadow;
    llvm::Function *lifted;

    ISEL_view(const InstructionSelection &isel, uint64_t i)
      : instruction(isel.instructions[i]), encoding(isel.encodings[i]),
        shadow(isel.shadows[i]), lifted(isel.lifted_fns[i])
    {}
  };

} // namespace circ