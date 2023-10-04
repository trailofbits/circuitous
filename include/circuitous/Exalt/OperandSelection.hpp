/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Exalt/Common.hpp>
#include <circuitous/Exalt/UnitComponents.hpp>

CIRCUITOUS_RELAX_WARNINGS
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ::exalt
{
    struct State;

    // Makes mux from translation map in "a default" way used by this infrastructure.
    // Mapping with partials register are *not* emitted (always the biggest reg is
    // loaded) - it is expected users of the mux will coerce their operands accordingly.
    // Additionally, each register is then extended to `word_size` using `llvm::ZExt`.
    value_t make_mux( builder_t &irb, State &state, const shadowinst::TM_t &tm );

    struct TM_cache : has_ctx_ref
    {
        using base = has_ctx_ref;
        using base::base;

        using translation_map_t = shadowinst::TM_t;
        using storage_t = std::vector< translation_map_t >;

        storage_t storage;

        /* queries */

        bool is_contained( const translation_map_t &tm ) const
        {
            for ( const auto &item : storage )
                if ( tm.is_subset_of( item ) )
                    return true;
            return false;
        }

        /* build methods */

        translation_map_t upcast( const translation_map_t &tm );

        void add_sat( const translation_map_t &tm );
        void add_unsat( const translation_map_t &tm );

        void build_from( const auto &worklist )
        {
            for ( const auto &u : worklist )
                for ( const auto &a : u )
                    build_sat( a.abstract );

            for ( const auto &u : worklist )
                for ( const auto &a : u )
                    build_unsat( a.abstract );
        }

        void build_sat( const shadowinst::Instruction &s_int );
        void build_unsat( const shadowinst::Instruction &s_int );

        /* utility */

        std::string to_string() const;
    };

    // Used internally to build up.
    struct operand_selector
    {
      protected:
        // Actual mux that does selection - while this object
        // is live, the first operand is a dummy value.
        value_t mux;
        // We are building the selector separately to avoid
        // having to mutate the `mux` everytime a new user is added.
        // Mapping is same as `irops::Option` - `[ value, conditions ]`
        std::unordered_map< value_t, value_set_t > selectors;

      public:

        operand_selector( builder_t &irb, State &state, const shadowinst::TM_t &tm );

        void add_user( value_t condition, value_t value );
        value_t update_mux( builder_t &irb );
    };

    struct TM_allocator
    {
        TM_cache storage;

        // Concrete materialization of given select.
        // Key is the index into `storage` (which can be used to
        // spawn a new value).
        std::map< std::size_t, values_t > read_map;
        std::map< std::size_t, values_t > write_map;

    };
} // namespace circ::exalt
