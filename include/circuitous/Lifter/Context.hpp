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

namespace circuitous {

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

} // namespace circuitous