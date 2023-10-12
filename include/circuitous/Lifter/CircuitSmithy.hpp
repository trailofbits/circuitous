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
    enum class lifter_kind
    {
        v1 = 0,
        v2,
        v3,
        mux_heavy,
        disjunctions
    };

    static inline std::string to_string( lifter_kind kind )
    {
        switch ( kind )
        {
            case lifter_kind::v1 : return "v1";
            case lifter_kind::v2 : return "v2";
            case lifter_kind::v3 : return "v3";
            case lifter_kind::mux_heavy : return "mux_heavy";
            case lifter_kind::disjunctions : return "disjunctions";
        }
    }

    struct Circuit;

    struct owns_context
    {
        Ctx ctx;

        owns_context( const std::string &arch, const std::string &os )
            : ctx( arch, os )
        {}

        owns_context( Ctx &&ctx ) : ctx( std::move( ctx ) ) {}
    };

    namespace exalt
    {
        struct circuit_producer;
    }

    struct CircuitSmithy : owns_context
    {
        using self_t = CircuitSmithy;
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

        auto forge_disjunctions( concretes_t &&concretes ) -> circuit_ptr_t;
        auto forge_mux_heavy( concretes_t &&concretes ) -> circuit_ptr_t;
        auto forge_v3( concretes_t &&concretes ) -> circuit_ptr_t;

        auto forge_common( exalt::circuit_producer &producer,
                           atoms_t &&atoms ) -> circuit_ptr_t;

        template< typename R >
        auto make( lifter_kind kind, R &&raw ) -> circuit_ptr_t
        {
            if ( kind == lifter_kind::disjunctions )
                return forge_disjunctions( purify( std::forward< R >( raw ) ) );
            if ( kind == lifter_kind::mux_heavy )
                return forge_mux_heavy( purify( std::forward< R >( raw ) ) );
            if ( kind == lifter_kind::v3 )
                return forge_v3( purify( std::forward< R >( raw ) ) );

            // Unsupported
            return {};
        }
    };

}  // namespace circ
