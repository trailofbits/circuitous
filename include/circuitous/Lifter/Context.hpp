/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <bitset>
#include <map>
#include <memory>

#include <circuitous/Lifter/Shadows.hpp>
#include <circuitous/Support/Check.hpp>

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

namespace circ
{
    struct Names
    {
        // TODO(lukas): Merge into `Context`.
        struct meta
        {
            static constexpr const char *verify_args = "circuitous.verify_fn_args";
            static constexpr const char *dst_reg =     "circuitous.dst.reg";
        };
    };

    struct Ctx
    {
        using arch_ptr_t = remill::Arch::ArchPtr;
        using reg_ptr_t = const remill::Register *;
        using regs_t = std::vector< reg_ptr_t >;

        std::shared_ptr< llvm::LLVMContext > _llvm_context =
            std::make_shared< llvm::LLVMContext >();

        arch_ptr_t _arch;
        std::unique_ptr< llvm::Module > _module;
        std::vector< reg_ptr_t > _regs;

        uint32_t ptr_size = 0;

        auto llvm_ctx() { return _llvm_context.get(); }
        auto arch() { return _arch.get(); }
        auto module() { return _module.get(); }
        auto &regs() { return _regs; }

        auto ir() { return llvm::IRBuilder<>{ *llvm_ctx() }; }

        auto state_ptr_type() const { return _arch->StatePointerType(); }
        auto state_type() const { return state_ptr_type()->getPointerElementType(); }
        auto memory_ptr_type() const { return _arch->MemoryPointerType(); }
        auto undef_mem_ptr() { return llvm::UndefValue::get( memory_ptr_type() ); }

        static auto make_arch(
            llvm::LLVMContext *ctx, const std::string &os_name,
            const std::string &arch_name )
        {
            auto os = remill::GetOSName( os_name );
            auto arch = remill::GetArchName( arch_name );
            return remill::Arch::Build (ctx, os, arch );
        }

        auto clean_module( const std::unordered_set<llvm::Function *> &keep )
        {
            std::vector< llvm::Function * > to_erase;
            for (auto &fn : *module())
            {
                if ( keep.count( &fn ) )
                    continue;

                if ( fn.isDeclaration() )
                    continue;

                if ( fn.getName().startswith( "__remill" ) ||
                     fn.getName().startswith( "__circuitous" ) )
                {
                    continue;
                }
                to_erase.push_back(&fn);
            }

            for ( auto fn : to_erase )
            {
                fn->replaceAllUsesWith( llvm::UndefValue::get( fn->getType() ) );
                fn->eraseFromParent();
            }

            std::vector< llvm::GlobalValue * > gv_to_erase;
            for ( auto &gv : module()->globals() )
                if ( gv.getName().startswith( "ISEL_" ) )
                    gv_to_erase.push_back( &gv );

            for ( auto gv : gv_to_erase )
            {
                gv->replaceAllUsesWith( llvm::UndefValue::get( gv->getType() ) );
                gv->eraseFromParent();
            }
        }

        reg_ptr_t pc_reg()
        {
            auto reg = arch()->RegisterByName( arch()->ProgramCounterRegisterName() );
            check( reg ) << "Was not able to fetch program counter register!";
            return reg;
        }

        reg_ptr_t reg( const std::string &name )
        {
            return arch()->RegisterByName( name );
        }

        llvm::Type *reg_type( const std::string &name )
        {
            auto reg_ptr = reg( name );
            // It is highly unlikely this can be recovered from anyway.
            check( reg_ptr );
            return reg_ptr->type;
        }

        bool is_allowed(const std::string &name)
        {
            static const std::unordered_set< std::string > allowed64 =
            {
                "AF", "CF", "PF", "DF", "OF", "SF", "ZF",
                "RAX", "RBX", "RCX", "RDX", "RSI", "RDI", "RBP", "RSP",
                "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15",
                "RIP"
            };

            static const std::unordered_set< std::string > allowed32 =
            {
                "AF", "CF", "PF", "DF", "OF", "SF", "ZF",
                "EAX", "EBX", "ECX", "EDX", "ESI", "EDI", "EBP", "ESP",
                "R8D", "R9D", "R10D", "R11D", "R12D", "R13D", "R14D", "R15D",
                "EIP",
                // Used in string ops
                "ESBASE", "DSBASE", "SSBASE", "GSBASE", "FSBASE", "CSBASE"
            };

            switch ( ptr_size )
            {
                case 64: return allowed64.count( name ) != 0;
                case 32: return allowed32.count( name ) != 0;
                default: unreachable() << "Unsupported ptr size";
            }
        }

        Ctx(const std::string &os_name, const std::string &arch_name)
            : _arch(make_arch(_llvm_context.get(), os_name, arch_name)),
              _module(remill::LoadArchSemantics(arch())),
              ptr_size(_arch->address_size)
        {
          std::stringstream dbg;
          _arch->ForEachRegister([&](reg_ptr_t reg_) {
            if (auto reg = reg_->EnclosingRegister(); reg == reg_ && is_allowed(reg->name)) {
              _regs.push_back(reg);
              dbg << " " << reg->name;
            }
          });
          log_info() << "Initialized regs as: [" << dbg.str() << " ]\n";
        }
    };
    using CtxRef = Ctx &;

    struct has_ctx_ref {
      CtxRef ctx;
      has_ctx_ref(CtxRef ctx_) : ctx(ctx_) {}
    };

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
      check(instructions.size() == encodings.size() &&
            instructions.size() == shadows.size() &&
            instructions.size() == lifted_fns.size());
    }

    void PartialAdd(remill::Instruction rinst, InstructionEncoding encoding) {
      AssertIntegrity();

      instructions.push_back(std::move(rinst));
      encodings.push_back(std::move(encoding));

      // These will be filled by other part of the pipeline!
      shadows.emplace_back(instructions.back().bytes.size() * 8);
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

    ISEL_view(const remill::Instruction &rinst_,
              const InstructionEncoding &enc_,
              const shadowinst::Instruction &shadow_,
              llvm::Function *lifted_)
        : instruction(rinst_),
          encoding(enc_),
          shadow(shadow_),
          lifted(lifted_)
    {}
  };

} // namespace circ
