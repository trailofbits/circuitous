/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Lifter/Instruction.hpp>
#include <circuitous/Util/InstructionBytes.hpp>

#include <string>
#include <vector>

namespace circ
{
    struct Circuit;

    struct owns_context
    {
        Ctx ctx;

        owns_context( const std::string &arch, const std::string &os )
            : ctx( arch, os )
        {}

        owns_context( Ctx &&ctx ) : ctx( std::move( ctx ) ) {}
    };

    struct CircuitSmithy
    {
        using self_t = CircuitSmithy;
        using circuit_ptr_t = std::unique_ptr< Circuit >;

        using batch_t = InstructionBatch;
        // This class owns the lifting context.
        Ctx ctx;

      private:
        batch_t batch;
        circuit_ptr_t circuit;


      public:
        CircuitSmithy(const std::string &arch_name, const std::string &os_name);
        // Take ownership of already existing context.
        // TODO(lukas): It cannot be retrieved back, should it?
        CircuitSmithy(Ctx ctx_);

        self_t &smelt(const std::vector< InstBytes > &insts);
        self_t &smelt(std::string_view raw_bytes);
        self_t &smelt(std::vector< remill::Instruction > &&rinsts);

        // Returns circuit created from all data provided by `smelt`. Will return owning
        // pointer to circuit and resets internal state of `this`.
        circuit_ptr_t forge();
    };

    struct CircuitSmithy_v2 : owns_context
    {
        using self_t = CircuitSmithy_v2;
        using circuit_ptr_t = std::unique_ptr< Circuit >;

        using atom_t = Atom;
        using atoms_t = std::vector< atom_t >;

        using concretes_t = std::vector< atom_t::concrete_t >;

        using unit_t = Unit< atom_t >;
        using worklist_t = Worklist< unit_t >;


        using owns_context::owns_context;

      private:

        worklist_t categorize( atoms_t atoms );

      public:

        auto purify( const std::vector< InstBytes > &insts ) -> concretes_t;
        auto purify( std::string_view raw_bytes ) -> concretes_t;

        auto purify( concretes_t concretes ) { return concretes; }

        auto smelt( concretes_t &&concretes ) -> atoms_t;

        auto forge( atoms_t &&atoms ) -> circuit_ptr_t;

        auto default_forge( auto &&raw )
        {
            return forge( std::move( smelt( purify( raw ) ) ) );
        }

    };
}  // namespace circ
