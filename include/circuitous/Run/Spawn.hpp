/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/Trace.hpp>

#include <circuitous/Run/Base.hpp>
#include <circuitous/Run/Queue.hpp>
#include <circuitous/Run/State.hpp>
#include <circuitous/Run/Result.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <deque>
#include <unordered_map>

namespace circ::run
{

    template< typename Semantics, typename Queue >
    struct SpawnBase : StateOwner
    {
        using semantics_t = Semantics;
        using result_t = result_t;

        circuit_ref_t circuit;
        NodeState node_state;

        Semantics semantics;

        using queue_ptr = std::unique_ptr< TodoQueue >;
        queue_ptr todo;


      protected:
        SpawnBase( circuit_ref_t circuit, queue_ptr todo, NodeState node_state )
            : circuit( circuit ),
              node_state( std::move( node_state ) ),
              semantics( this, circuit ),
              todo( std::move( todo ) )
        {}

      public:

        SpawnBase( circuit_ref_t circuit,
                   NodeState node_state )
            : circuit( circuit ),
              node_state( std::move( node_state ) ),
              semantics( this, circuit ),
              todo( std::make_unique< Queue >() )
        {}

        virtual ~SpawnBase() = default;

        // NOTE(lukas): `semantics` are holding a pointer to `this` -> therefore if it is
        //              decided that move/copy ctor is needed, keep that in mind.
        SpawnBase( const SpawnBase & ) = delete;
        SpawnBase( SpawnBase && ) = delete;

        SpawnBase &operator=( SpawnBase ) = delete;

        static std::string to_string( const value_type &val )
        {
            if ( !val )
                return "{}";
            return llvm::toString( *val, 16, false );
        }

        // If `op` already has a value different than `val` hard error is hit -> this most
        // likely means a bug occurred.
        // NOTE(lukas): The best option would be if we never try to set *any* value to `op`
        //              that already has some. Unfortunately, this would require some hack
        //              elsewhere, as when deriving a value -> parent sets value to child ->
        //              notification is fired again and adds parent to queue. One option
        //              is to never add again things into queue that were processes once,
        //              but I think it exposes a bigger surface for bugs than current approach.
        void set_node_val( Operation *op, const value_type &val ) override
        {
            log_dbg() << "[spawn]:" << pretty_print< false >( op )
                                    << "<-" << to_string( val );
            if ( node_state.has_value( op ) )
            {
                // Helpful formatter to report error.
                auto fmt = []( auto what ) -> std::string
                {
                    if ( !what )
                        return "( no value )";
                    std::stringstream ss;
                    ss << "[ "<< what->getBitWidth()
                       << "b: " << llvm::toString( *what, 16, false )
                       << " ]";
                    return ss.str();
                };
                check( node_state.get( op ) == val, [ & ]()
                {
                    std::stringstream ss;
                    ss << pretty_print( op ) << " already has value "
                       << fmt( node_state.get( op ) )
                       << " yet we try to set "
                       << fmt( val );
                       return ss.str();
                });
                log_dbg() << "Assign:" << pretty_print< false >( op )
                          << "value was already set.";
                return;
            }

            this->node_state.set( op, val );
            notify_from( op );
        }

        /* StateOwner interface */

        value_type get_node_val( Operation *op ) const override { return node_state.get( op ); }
        bool has_value( Operation *op ) const override { return node_state.has_value( op ); }


        void visit( Operation *op )
        {
            semantics.dispatch( op );
        }

        virtual void dispatch( Operation *op )
        {
            semantics.dispatch( op );
        }

        // If context does not have a value -> something is blocking it.
        // Report a trace that highlights which nodes are not interpreted and what is blocking
        // them.
        void no_value_reached_witness( Operation *root )
        {
            std::stringstream ss;

            auto fmt = [ & ]( auto what, auto &prefix )
            {
                ss << prefix << pretty_print< false >( what )
                   << " : " << this->todo->status( what ) << "\n";
            };

            auto rec_print = [ & ]( auto what, std::size_t indent, auto &rec ) -> void
            {
                std::string prefix( indent * 2, ' ' );
                if ( has_value( what ) )
                {
                    ss << prefix << " * \n";
                    return;
                }

                fmt( what, prefix );
                for ( auto op : what->operands() )
                    rec( op, indent + 1, rec );
            };

            rec_print( root, 0, rec_print );
            log_dbg() << ss.str();
        }

        auto is_in_current_ctx()
        {
            return [ & ]( const auto &op )
            {
                return true;
            };
        }

        void notify_from( Operation *op )
        {
            todo->notify_from( op, is_in_current_ctx() );
        }

        void derive( const std::unordered_set< Operation * > &ops )
        {
            for ( auto op : ops )
            {
                if ( is_in_current_ctx()( op ) )
                {
                    semantics.to_derive( op->operand( 1 ), op );
                    todo->notify_self( op );
                }
            }
        }

        void init()
        {
            log_dbg() << "[spawn]:" << "Initializing semantics.";
            semantics.init();
            for ( const auto &[ op, _ ] : node_state.node_values )
            {
                log_dbg() << "[spawn]:" << "Operation with pre-set value:"
                                        << pretty_print< false >( op );
                notify_from( op );
            }
        }

        // `[ current, next ]`.
        // In `next` only values "visible" by this transition are updated.
        std::tuple< std::string, std::string > to_traces() const
        {
            auto trace = Trace::make( circuit );
            std::string current_trace( trace.total_size, '0' );
            std::string next_trace( trace.total_size, '0' );

            auto inject = [ & ]( auto &where, auto op, auto &field )
            {
                const auto &[ start, size, _ ] = field;
                if ( !node_state.has_value( op ) )
                {
                    check( is_one_of< Advice, circ::Memory >( op ) );
                    return;
                }
                auto maybe_value = node_state.get( op );
                check( maybe_value );

                auto val_as_str = llvm::toString( *maybe_value, 2, false );
                if ( val_as_str.size() < size )
                {
                    auto diff = size - val_as_str.size();
                    val_as_str.insert( 0, std::string( diff, '0' ) );
                }

                where.replace( start, size, val_as_str );
            };

            for ( auto &[ op, field ] : trace.parse_map )
            {
                if ( is_one_of( op, input_leaves_ts{} ) )
                    inject( current_trace, op, *field );
                else if ( is_one_of( op, output_leaves_ts{} ) )
                    inject( next_trace, op, *field );
                else
                    unreachable() << "Unreachable.";
            }
            check( current_trace.size() == trace.total_size );
            check( next_trace.size() == trace.total_size );
            return { current_trace, next_trace };
        }

        result_t run() {
            log_dbg() << "[spawn]:" << "Running on circuiut";
            check( circuit && todo );
            init();

            // Set constants first, as they are never blocked by anything.
            for ( const auto &constant : circuit->attr< Constant >() )
                this->dispatch( constant );

            log_dbg() << "[spawn]: constant initialization done!";
            while ( !todo->empty() )
            {
                auto x = todo->pop();
                this->dispatch( x );
            }

            if ( !node_state.has_value( circuit->root ) )
            {
                log_dbg() << "[spawn]:" << "Value is not reached! Witness:\n";
                no_value_reached_witness( circuit->root );
                return result_t::value_not_reached;
            }

            log_dbg() << node_state.to_string();
            if ( auto res = node_state.get( circuit->root ) )
                return ( *res == semantics.true_val() )
                       ? result_t::accepted
                       : result_t::rejected;

            unreachable() << "Spawn::run() did not reach any result!";
        }

        void store( uint64_t addr, const raw_value_type &data ) override
        {
            log_kill() << "Unimplemented!";
        }

        value_type load( uint64_t addr, std::size_t size ) const override
        {
            log_kill() << "Unimplemented!";
        }

        bool defined( uint64_t addr, std::size_t size ) const override
        {
            log_kill() << "Unimplemented!";
        }

        auto get_derived_mem() const
        {
            std::vector< Memory::Parsed > out;
            for ( auto op : this->circuit->template attr< circ::Memory >() )
            {
                if ( this->node_state.has_value( op ) )
                    out.push_back( Memory::deconstruct( *( this->node_state.get( op ) ),
                                                        circuit->ptr_size ) );
            }
            check( out.size() == this->circuit->template attr< circ::Memory >().size() );
            return out;
        }

    };

    template< typename Semantics >
    struct DerivingSpawn : SpawnBase< Semantics, QueueWithMemOrder >
    {
        using base_t = SpawnBase< Semantics, QueueWithMemOrder >;

        const CtxCollector &ctx_info;
        VerifyInstruction *current;
        Memory memory;

        DerivingSpawn( Circuit *circuit, VerifyInstruction *current,
                       const CtxCollector &ctx_info,
                       const NodeState &node_state, const Memory &memory )
        : base_t( circuit,
                  std::make_unique< QueueWithMemOrder >(
                      MemoryOrdering( circuit, ctx_info, current ) ),
                  node_state ),
          ctx_info( ctx_info ),
          current( current ),
          memory( memory )
        {}

        // NOTE(lukas): `semantics` are holding a pointer to `this` -> therefore if it is
        //              decided that move/copy ctor is needed, keep that in mind.
        DerivingSpawn( const DerivingSpawn & ) = delete;
        DerivingSpawn( DerivingSpawn && ) = delete;

        DerivingSpawn &operator=( DerivingSpawn ) = delete;


        Memory take_memory() { return std::move( memory ); }

        void dispatch( Operation *op ) override
        {
            this->semantics.dispatch( op );
        }

        void store( uint64_t addr, const raw_value_type &data ) override
        {
            memory.store( addr, data );
        }

        value_type load( uint64_t addr, std::size_t size ) const override
        {
            return memory.load( addr, size );
        }

        bool defined( uint64_t addr, std::size_t size ) const override
        {
            return memory.defined( addr, size );
        }

        template< typename T >
        auto get_derived() const { return this->semantics.template get_derived< T >(); }

        auto get_derived_mem() const
        {
            std::vector< Memory::Parsed > out;
            for ( auto op : this->circuit->template attr< circ::Memory >() )
            {
                if ( this->node_state.has_value( op ) )
                    out.push_back( memory.deconstruct( *( this->node_state.get( op ) ) ) );
            }
            return out;
        }

    };

    using spawn_verifier = SpawnBase< verifier_semantics, TodoQueue >;

    static_assert(valid_interpreter< verifier_semantics >());
    static_assert(valid_interpreter< typename DerivingSpawn< Base >::semantics_t >());

} // namespace circ::run
