/*
 * Copyright (c) 2023 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Exalt/Components.hpp>
#include <circuitous/Exalt/UnitComponents.hpp>
#include <circuitous/Exalt/Interfaces.hpp>

#include <circuitous/Util/Warnings.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <tuple>
#include <type_traits>

namespace circ::exalt
{
    // TODO( exalt ): Figure out if we even need this separation.
    struct has_components_base
    {
        virtual ~has_components_base() = default;

        virtual component_list_t &components() = 0;
        virtual const component_list_t &components() const = 0;

        /* Initialisation */

        exalted_value_buckets init( unit_t &unit )
        {
            exalted_value_buckets out;

            for ( auto &c : components() )
                merge_to( out, c->init( unit ) );

            return out;
        }

        void init()
        {
            for ( auto &c : components() )
                c->init();
        }

        /* Access components */

        template< typename T >
        T &fetch_or_die()
        {
            for ( auto &c : components() )
                if ( auto casted = dynamic_cast< T * >( c.get() ) )
                    return *casted;
            log_kill() << "Could not fetch desired interface from unit_components.";
        }

        auto get_decoder() -> decoder_base &
        {
            log_dbg() << "[exalt:ucs]:" << "Fetching decoder.";
            return fetch_or_die< decoder_base >();
        }

        auto get_isem_lifter() -> isem_lifter_base &
        {
            log_dbg() << "[exalt:ucs]:" << "Fetching isem_lifter.";
            return fetch_or_die< isem_lifter_base >();
        }

        /* Dispatch */

        exalted_values_gen_t after_isem( unit_t &unit, isem_range_t isem_range )
        {
            for ( auto &c : components() )
            {
                auto casted = dynamic_cast< unit_component_base * >( c.get() );
                if ( !casted )
                    continue;

                for ( auto v : casted->after_isem( unit, isem_range ) )
                    co_yield v;
            }
        }
    };

    // Owns the components stored in it.
    // Does not have to be the *exclusive* owner (as some components may be
    // stored elsewhere for persistency) but since they are `shared_ptr` this
    // makes no difference.
    struct component_storage : has_components_base
    {
        builder_context &b_ctx;
        component_list_t _components;

        component_storage( builder_context &b_ctx )
            : b_ctx( b_ctx )
        {}

        component_storage( builder_context &b_ctx, component_list_t &&components )
            : b_ctx( b_ctx ), _components( std::move( components ) )
        {}

        component_storage( builder_context &b_ctx, const has_components_base &other )
            : b_ctx( b_ctx )
        {
            copy_persistent_components( other );
        }

        void copy_persistent_components( const has_components_base &other )
        {
            for ( auto &c : other.components() )
                if ( c->is_persistent() )
                    // We are using `shared_ptr` so we can copy and
                    // *not* destroy `other`.
                    _components.push_back( c );
        }

        /* `has_components_base` */

        component_list_t &components() override { return _components; }
        const component_list_t &components() const override { return _components; }

        /* Construction */
        template< is_unit_component T >
        component_base &emplace()
        {
            _components.push_back( std::make_shared< T >( b_ctx ) );
            return *_components.back();
        }

        void add( component_t component )
        {
            _components.push_back( std::move( component ) );
        }

        template< typename ... Ts >
        static auto make( builder_context &b_ctx )
        {
            auto self = component_storage( b_ctx );
            ( self.emplace< Ts >(), ... );
            return self;
        }

        static auto make_default( builder_context &b_ctx )
        {
            return make< simple_unit_decoder, memory_checks, error_bit >( b_ctx );
        }
    };

    // Handy accesor to pass around & query some collection of components.
    // This is *not* exactly light-weight object.
    struct component_view : has_components_base
    {
        component_list_t _components;

        /* `has_components_base` */

        component_list_t &components() override { return _components; }
        const component_list_t &components() const override { return _components; }
    };


}  // namespace circ
