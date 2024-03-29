/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/IR/Serialize.hpp>

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/IR/Memory.hpp>
#include <circuitous/IR/Visitors.hpp>

#include <circuitous/Support/Log.hpp>
#include <circuitous/Support/Check.hpp>

#include <gap/core/ranges.hpp>

#include <cstdint>
#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>

namespace circ
{
    struct FileConfig
    {
        enum class Selector : uint8_t
        {
            Operation = 0x0,
            LeafOperation = 0x1,
            Metadatum = 0x3,
            Invalid = 0x4,
            Storage = 0x5,
            EndOfFile = 0x6,
            Reference = 0xff
        };

        using raw_op_code_t = std::underlying_type_t< Operation::kind_t >;
        using raw_id_t = uint64_t;

        static std::string to_string( const Selector &sel )
        {
            switch( sel )
            {
                case Selector::Storage   :     return "Storage";
                case Selector::EndOfFile :     return "EndOfFile";
                case Selector::Operation :     return "Operation";
                case Selector::LeafOperation : return "LeafOperation";
                case Selector::Invalid   :     return "Invalid";
                case Selector::Metadatum :     return "Metadatum";
                case Selector::Reference :     return "Reference";
            }
        }

        static bool is_valid_sel( const Selector &sel )
        {
            switch( sel )
            {
                case Selector::Invalid: return false;
                default:                return true;
            }
        }

        static bool is_op_definition( const Selector &sel )
        {
            return sel == Selector::Operation || sel == Selector::LeafOperation;
        }
    };

    struct SerializeVisitor : public Visitor< SerializeVisitor >, FileConfig
    {
        using Selector = FileConfig::Selector;

        std::ostream &os;
        std::unordered_set< uint64_t > written;

        ~SerializeVisitor()
        {
            os.flush();
        }

        explicit SerializeVisitor( std::ostream &os_ ) : os( os_ ) {}

        void serialize( circuit_ref_t circuit )
        {
            Write( Selector::Storage );
            Write( circuit->ptr_size );

            if ( circuit->root )
                Write( circuit->root );
        }

        void finish()
        {
            Write( Selector::EndOfFile );
        }

        Selector get_selector( Operation *op )
        {
            if ( op->operands_size() == 0 )
                return Selector::LeafOperation;
            return Selector::Operation;
        }

        void Write( Operation *op )
        {
            if ( !written.count( op->id() ) )
                return write_new_entry( op );
            return write_reference( op );
        }

        void write_new_entry( Operation *op )
        {
            auto sel = get_selector( op );
            // Write generic for all.
            Write( sel );
            Write( op->id() );
            Write( op->op_code );
            written.insert( op->id() );
            // Write special attributes based on runtime type.
            this->dispatch( op );

            // If the operation is not a leaf, write operands.
            if ( sel != Selector::LeafOperation )
            {
                Write( static_cast< std::size_t >( op->operands_size() ) );
                Write( op->operands() );
            }
        }

        void write_reference( Operation *op )
        {
            Write( Selector::Reference );
            Write< raw_id_t >( op->id() );
        }

        void Write( Selector sel )
        {
            check( is_valid_sel( sel ), [&]() { return "Trying to write invalid selector!"; } );
            Write( static_cast< std::underlying_type_t< Selector > >( sel ) );
        }

        void Write( Operation::kind_t kind )
        {
            Write( util::to_underlying( kind ) );
        }

        void Write( const std::string &str )
        {
            Write< uint32_t >( static_cast< uint32_t >( str.size() ) );
            for ( auto ch : str )
                Write( ch );
        }

        template< typename I > requires ( std::is_integral_v< I > )
        void Write( I data )
        {
            auto bytes = reinterpret_cast< const uint8_t * >( &data );
            for ( auto i = 0ull; i < sizeof( data ); ++i )
                os << ( static_cast< uint8_t >( bytes[i] ) );
        }

        template< gap::ranges::range Ops >
            requires ( !std::is_same_v< std::remove_cvref_t< Ops >, std::string > )
        void Write( Ops &&ops )
        {
            for ( auto op : ops )
                Write( op );
        }

        template< typename ...Args >
        void write( Args &&... args ) { ( Write( std::forward< Args >( args ) ), ... ); }

        void write_metadata( Operation *op )
        {
            // TODO(lukas): Was excluded (most likely dead node). However, we want to serialize
            //              *all* nodes, rewrite as check once serializer is fixed.
            if ( !written.count( op->id() ) )
                return;

            for ( auto &[key, val] : op->meta )
            {
                Write( Selector::Metadatum );
                Write( op->id() );
                Write( key );
                Write( val );
            }
        }

        void visit( Operation *op ) { write( op->size ); }

        void visit( InputRegister *op ) { write( op->reg_name, op->size ); }
        void visit( OutputRegister *op ) { write( op->reg_name, op->size ); }

        void visit( Constant *op ) { write( op->bits, op->size ); }

        void visit( Extract *op ) { write( op->low_bit_inc, op->high_bit_exc ); }
        void visit( Select *op ) { write( op->bits, op->size ); }
        void visit( Memory *op ) { write( op->size, op->mem_idx ); }
        void visit( Advice *op ) { write( op->size, op->advice_idx ); }

    };

    template< typename T, typename ... Args >
    struct SDef {};

    template< typename ... Args >
    struct bind_args
    {
        template< typename T >
        using type = SDef< T, Args ... >;
    };

    using bind_size = bind_args< unsigned >;


    namespace detail
    {

        template< typename D, typename T >
        struct inject
        {
            Operation *visit( T *, uint64_t id )
            {
                auto &self = static_cast< D & >( *this );
                return self.template make_op< T >( id, self.template read< unsigned >() );
            }
        };

        template< typename D, typename T, typename ... Args >
        struct inject< D, SDef< T, Args ... > >
        {
            static_assert( sizeof ... ( Args ) != 0 );

            Operation *visit( T *, uint64_t id )
            {
                auto &self = static_cast< D & >( *this );
                return self.template make_op< T >( id, self.template read< Args ... >() );
            }
        };

        template< typename D, typename ... Ts >
        struct unfolder {};

        template< typename D, typename T >
        struct unfolder< D, T > : inject< D, T > { using inject< D, T >::visit; };

        template< typename D, typename T, typename ...Ts >
        struct unfolder< D, T, Ts... > : inject< D, T >, unfolder< D, Ts... >
        {
            using inject< D, T >::visit;
            using unfolder< D, Ts... >::visit;
        };

        template< typename D, typename L > struct inject_visitors {};
        template< typename D, typename ... Ts >
        struct inject_visitors< D, tl::TL< Ts ... > > : unfolder< D, Ts ... > {};

    } // namespace detail

    // Inject deserializers of former llvm-operation
    template< typename D >
    using DeserializeComputational = detail::inject_visitors< D, llvm_ops_t >;

    using other_ops_ts = tl::TL<
                                SDef< Select, unsigned, unsigned >,
                                SDef< Extract, unsigned, unsigned >,
                                SDef< Advice, unsigned, std::size_t >,
                                SDef< Memory, unsigned, unsigned >,
                                SDef< InputRegister, std::string, unsigned >,
                                SDef< OutputRegister, std::string, unsigned >,
                                SDef< Constant, std::string, unsigned >
                               >;

    using sized_ops_t = tl::TL<
                               InputTimestamp,
                               OutputTimestamp,

                               InputErrorFlag,
                               OutputErrorFlag,

                               InputInstructionBits,
                               InputImmediate,

                               Undefined,
                               Not,
                               Concat,
                               PopulationCount,
                               CountLeadingZeroes,
                               CountTrailingZeroes,

                               Switch,
                               Option
                              >;

    using conditions_ts = tl::merge< bool_ops_ts, constraint_opts_ts, tl::TL< Parity > >;

    using with_size = tl::apply< tl::merge< sized_ops_t, conditions_ts >, bind_size >;
    using all_binded_ts = tl::merge< with_size, other_ops_ts >;


    template< typename D >
    using DeserializeSimple = detail::inject_visitors< D, all_binded_ts >;

    struct DeserializeVisitor : FileConfig, DVisitor< DeserializeVisitor >,
                                DeserializeComputational< DeserializeVisitor >,
                                DeserializeSimple< DeserializeVisitor >
    {
        using Selector = FileConfig::Selector;

        using DeserializeComputational< DeserializeVisitor >::visit;
        using DeserializeSimple< DeserializeVisitor >::visit;

        std::istream &is;
        std::unordered_map< uint64_t, Operation * > id_to_op;
        circuit_owner_t circuit;

        explicit DeserializeVisitor( std::istream &is ) : is( is ) {}

        circuit_ref_t get_circuit()
        {
            check( circuit );
            return circuit.get();
        }

        circuit_owner_t take_circuit()
        {
            check( circuit );
            return std::move( circuit );
        }

        // TODO(lukas): Move to ctor?
        // Returns false is reading of header failed.
        [[ nodiscard ]] bool deserialize_storage()
        {
            auto [ sel ] = read< Selector >();
            if ( sel != Selector::Storage )
                return false;

            auto [ ptr_size ] = read< Circuit::ptr_size_t >();
            circuit = std::make_unique< Circuit >( ptr_size );
            return true;
        }

        void Read( Selector &sel )
        {
            std::underlying_type_t< Selector > out;
            Read( out );
            sel = static_cast< Selector >( out );
        }

        void Read( std::string &str )
        {
            uint32_t size = 0u;
            Read( size );
            str.resize( size );
            for ( auto i = 0u; i < size; ++i )
                Read( str[i] );
        }

        template< typename I > requires ( std::is_integral_v< I > )
        void Read( I &data )
        {
            auto bytes = reinterpret_cast< uint8_t * >( &data );
            for ( auto i = 0ull; i < sizeof( data ); ++i )
                is >> bytes[i];
        }

        void Read( Operation::kind_t &kind )
        {
            raw_op_code_t raw = 0;
            Read( raw );
            auto maybe_kind = reconstruct_kind( raw );
            // TODO(lukas): We cannot recover right now.
            check( maybe_kind,
                   [&]() { return "Cannot deserialize operation kind "
                                  + std::to_string( raw ); } );
            kind = *maybe_kind;
        }

        template< typename ...Args >
        std::tuple< Args ... > read()
        {
            std::tuple< Args ... > out;

            auto read_ = [&]( Args &... args ) { ( Read( args ), ... ); };
            std::apply( read_, out );
            return out;
        }


        void read_operands( Operation *elems )
        {
            auto [size] = read< std::size_t >();
            for ( auto i = 0u; i < size; ++i )
                elems->add_operand( read_operation() );
        }

        Operation *read_new_op( Selector sel )
        {
            // Same for all.
            auto [ id, op_code]  = read< raw_id_t, Operation::kind_t >();

            // Op specific.
            auto op = this->dispatch( op_code, id );
            id_to_op[ id ] = op;

            // Operands last.
            if ( sel == Selector::Operation )
                read_operands( op );

            return op;
        }

        Operation *read_operation()
        {
          auto [ sel ] = read< Selector >();

          if ( is_op_definition( sel ) )
                return read_new_op( sel );

          if ( sel == Selector::Reference )
              return read_ref();

          unreachable() << "Expected operation def or ref, got:"
                        << this->to_string( sel ) << "instead";
        }

        Operation *read_ref()
        {
            auto [ id ] = read< raw_id_t >();
            auto op_it = id_to_op.find( id );
            check( op_it != id_to_op.end() ) << "Could not reference with id: " << id;
            return op_it->second;
        }

        // Returns `true` if it did not reach the end of file
        bool read_metadatum()
        {
            auto [ sel ] = read< Selector >();

            if ( sel == Selector::EndOfFile )
                return false;

            check( sel == Selector::Metadatum ) << "Expected metadata, found"
                                                << to_string( sel ) << "instead";

            read_metadatum_entry();
            return true;
        }

        void read_metadatum_entry()
        {
              auto [id, key, val] = read< raw_id_t, std::string, std::string >();
              check( id_to_op.count( id ) )
                  << "Trying to attach metadata [ " << key << ": " << val
                  << "] to operation with id" << id << "that is not present.";
              id_to_op[id]->set_meta( std::move( key ), std::move( val ) );
        }


        void deserialize_metadata()
        {
            while ( read_metadatum() ) {}
        }

        template< typename T >
        Operation *visit( T *op, uint64_t id )
        {
            unreachable() << "Cannot deserialize "
                          << op_code_str( T::kind )
                          << ". Most likely cause is missing impl.";
        }

        template< typename T, typename ...Args >
        auto make_op( uint64_t id, std::tuple< Args... > &&args )
        {
            auto make = [&]( Args &&... args ) {
                return circuit->adopt< T >( id, std::forward< Args >( args )... );
            };
            return std::apply( make, std::forward< std::tuple< Args ... > >( args ) );
        }
    };


    void serialize( std::ostream &os, Circuit *circuit )
    {
        SerializeVisitor vis( os );
        vis.serialize( circuit );

        auto write_metadata = [&]( auto op ) { vis.write_metadata( op ); };
        circuit->for_each_operation( write_metadata );

        vis.finish();

        os.flush();
    }

    void serialize( std::filesystem::path filename, Circuit *circuit )
    {
        std::ofstream file( filename, std::ios::binary | std::ios::trunc );
        check( file );
        return serialize( file, circuit );
    }

    auto deserialize( std::istream &is ) -> circuit_ptr_t
    {
        // TODO(lukas): Configurable.
        DeserializeVisitor vis( is );

        auto old_flags = is.flags();
        is.unsetf( std::ios::skipws );

        // Something went really wrong
        if (!vis.deserialize_storage())
        {
            log_error() << "Cannot deserialize circuit header!";
            return {};
        }
        vis.get_circuit()->root = vis.read_operation();
        vis.deserialize_metadata();

        is.flags( old_flags );
        return vis.take_circuit();
    }

    auto deserialize( std::filesystem::path filename ) -> circuit_ptr_t
    {
        std::ifstream file( std::string{filename}, std::ios::binary );
        check( file ) << "Failed to open file:" << filename;
        return deserialize( file );
    }

}  // namespace circ
