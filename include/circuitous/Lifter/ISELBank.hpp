/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Support/Check.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/LLVMUtil.hpp>

#include <circuitous/IR/Intrinsics.hpp>

#include <circuitous/Fuzz/InstNavigation.hpp>
#include <circuitous/Lifter/Shadows.hpp>
#include <circuitous/Lifter/SReg.hpp>
#include <circuitous/Lifter/ShadowMat.hpp>

#include <remill/Arch/Arch.h>
#include <remill/Arch/Instruction.h>
#include <remill/BC/Annotate.h>
#include <remill/BC/IntrinsicTable.h>
#include <remill/BC/Lifter.h>
#include <remill/BC/Util.h>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <cstdint>
#include <map>
#include <utility>
#include <vector>

namespace circ::isem
{
    using insts_t = std::vector< llvm::Instruction * >;

    // Linear wrt number of functions in module.
    auto semantic_fns( llvm::Module &in )
        -> gap::generator< llvm::Function * >;

    // Linear wrt number of functions in module.
    auto semantic_fn( const std::string &isel_name, llvm::Module &in )
        -> std::optional< llvm::Function * >;

    auto isel_map( llvm::Module &in )
        -> std::unordered_map< std::string, llvm::Function * >;


    struct ISem
    {
        // Should all be advices.
        insts_t srcs;
        insts_t dsts;

        using arg_blueprint_t = std::tuple< bool, llvm::Type *, std::size_t >;
        std::vector< arg_blueprint_t > args;

        llvm::Function *_original;
        llvm::Function *_self;

        auto &original() const { check( _original ); return *_original; };
        auto &self()     const { check( _self ); return *_self; };

        static llvm::Instruction *reconstruct_arg( llvm::IRBuilder<> &irb,
                                                   const arg_blueprint_t &bp );
    };

    struct ISemBank : has_ctx_ref
    {
        using parent_t = has_ctx_ref;

        static constexpr inline const char *fn_prefix = "__circ.isem_stub";

        // TODO(lift): Once this is no longer a prototype check if this is needed.
        // More names can point to the same isem.
        using isem_def_t = std::shared_ptr< ISem >;
        using isem_ref_t = const ISem *;
        using rinst_t = remill::Instruction;

      private:

        std::unordered_map< std::string, isem_def_t > cache;
      protected:

        isem_def_t cached( const std::string &isel_name );
        isem_def_t lift( const std::string &isel_name, llvm::Module &from,
                         llvm::Function &into );

      public:

        using parent_t::parent_t;

        // TODO(lift): Right now `from` is the same module, due to how remill initializes
        //             semantics.
        isem_ref_t make( const std::string &isel_name, llvm::Module &from );

        std::string make_name( const std::string &core )
        {
            return fn_prefix + core;
        }

        bool is_isem_stub( llvm::StringRef name )
        {
            return name.startswith( fn_prefix );
        }

        bool is_isem_stub( llvm::Function &fn )
        {
            return fn.hasName() && is_isem_stub( fn.getName() );
        }

        gap::generator< const ISem * > all()
        {
            for ( const auto &[ _, def ] : cache )
                co_yield def.get();
        }

        std::string to_string() const
        {
            std::stringstream out;
            out << "ISemBank contains: " << cache.size() << "\n";
            for ( const auto &[ name, def ] : cache )
                out << "\t" << name << " " << def->self().getName().str() << "\n";
            return out.str();
        }
    };

} // namespace circ::isem
