/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Exalt/Common.hpp>
#include <circuitous/Exalt/Interfaces.hpp>
#include <circuitous/Exalt/States.hpp>
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

        auto map_idx( const translation_map_t &tm ) const
            -> std::optional< std::size_t >
        {
            auto generic = upcast( tm );
            for ( std::size_t i = 0; i < storage.size(); ++i )
                if ( generic.is_subset_of( storage[ i ] ) )
                    return { i };
            return {};
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

        value_t operator*() { return mux; };
    };

    // Used by operand lifter to request operands based on shadow regs or translation
    // maps.
    // TODO( next ): Formulate requirements on state/lifetime.
    // TODO( next ): Formulate requirements on construction - I do not see how this
    //               works without `CtxRef` and `State` as they are required to
    //               materialize translation maps.
    struct requester_base : has_ctx_ref, component_base
    {
        using translation_map_t = shadowinst::TM_t;
        using shadow_reg_t = shadowinst::Reg;

        State &state;
        builder_t &irb;

        requester_base( CtxRef ctx_ref, State &state, builder_t &irb )
            : has_ctx_ref( ctx_ref ),
              state( state ),
              irb( irb )
        {}

        virtual ~requester_base() = default;

        // Callback to be invoked on every value in the mux excluding the selector.
        using coerce_function_t = std::function< value_t( value_t ) >;
        virtual value_t request( const translation_map_t &tm, bool is_read ) = 0;
        virtual value_t request( const shadow_reg_t &s_reg, bool is_read ) = 0;

        value_t request( const auto &thing, bool is_read )
        {
            return this->request( irb, thing, is_read, []( auto val ) { return val; } );
        }

        // TODO( next ): This needs a finalizer.
    };

    struct operand_allocator_base;

    struct requester_proxy : requester_base
    {
        value_t ctx_cond;
        operand_allocator_base &allocator;

        // TODO( exalt ): Should have separate idxs for read/write?
        // Maps `idx` into `storage` into count.
        std::map< std::size_t, std::size_t > used;

        requester_proxy( CtxRef ctx_ref, State &state,
                         builder_t &irb,
                         value_t ctx_cond,
                         operand_allocator_base &allocator )
            : requester_base( ctx_ref, state, irb ),
              ctx_cond( ctx_cond ),
              allocator( allocator )
        {}

        value_t request( const shadow_reg_t &s_reg, bool is_read ) override;
        value_t request( const translation_map_t &tm, bool is_read ) override;
    };


    struct operand_allocator_base : has_ctx_ref, component_base
    {
        using translation_map_t = typename requester_base::translation_map_t;
        using coerce_function_t = typename requester_base::coerce_function_t;

        virtual ~operand_allocator_base() = default;

        virtual std::size_t map_idx( const translation_map_t & ) = 0;

        virtual auto allocate( builder_t &, const translation_map_t &,
                               State &state,
                               std::size_t idx )
            -> operand_selector & = 0;


        using requester_ptr = std::unique_ptr< requester_base >;
        virtual requester_ptr get_requester( builder_t &irb, State &state,
                                             value_t ctx_cond );

        /* `component_base` */
        bool is_persistent() const override { return true; }
    };

    struct TM_allocator : operand_allocator_base
    {
        using base = operand_allocator_base;
        using translation_map_t = typename base::translation_map_t;
        using coerce_function_t = typename base::coerce_function_t;
        using shadow_reg_t = typename requester_base::shadow_reg_t;

        // TODO( XXX ): In ctor initialize `read_map` with nothings.
        TM_cache storage;

        // Concrete materialization of given select.
        // Key is the index into `storage` (which can be used to
        // spawn a new value).
        std::map< std::size_t, std::vector< operand_selector > > read_map;

        // TODO( exalt ): Currently we are only handling the read operands (output
        //                operands are not always lifted this way - see difference
        //                between let's say `mux-heavy` and `disjunctions`). We
        //                eventually want to support both (or really any number as
        //                long as each has a state with it).

        std::size_t map_idx( const translation_map_t & ) override;

        auto allocate( builder_t &, const translation_map_t &,
                       State &state,
                       std::size_t idx )
            -> operand_selector & override;
    };

    // TODO( exalt:design ): Maybe something like `mux_[maker,materializer]`
    //                       could be passed in?
    //                       Would allow us to bind `coerce` and `irb`?
    // ... initialization ...
    // auto cache = TM_cache.compute( worklist );
    // pcs.add< TM_allocator >( ctx_ref, cache );
    //
    // ... later in make_semantic_call ...
    // auto tm_allocator = pcs.get< operand_allocator_base >();
    // auto operand_lifter = OperandLifter( tm_allocator );
    // ... in OperandLifter ...
    //
    // auto req = tm_allocator.get_requester( irb(), state, ctx_cond );
    // auto tm = req.request( ... );
    //
    // ... at the circuit end ...
    // cache.finalize();


} // namespace circ::exalt
