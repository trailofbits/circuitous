/*
 * Copyright (c) 2021-present Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Support/Check.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/Warnings.hpp>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/Support/JSON.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
CIRCUITOUS_UNRELAX_WARNINGS

#include <fstream>

#include <circuitous/Run/Trace.hpp>
#include <circuitous/Run/Execute.hpp>

#include <circuitous/Lifter/CircuitSmithy.hpp>
#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Lifter/Decoder.hpp>

namespace circ::run::trace
{
    auto do_decode( circ::Ctx &ctx )
    {
        return [ & ]( const std::string &data )
        {
            circ::InstBytes converted;
            for ( int i = static_cast< int >( data.size() - 8); i >= 0; i -= 8 )
            {
                auto substr = data.substr( static_cast< unsigned long >( i ), 8 );
                converted.push_back( static_cast< char >( std::strtoul( substr.data(),
                                                                        nullptr, 2 ) ) );
            }

            auto maybe_inst = circ::Decoder( ctx ).decode_first( converted );
            circ::check( maybe_inst ) << "Decoder failed!";
            return std::move( *maybe_inst );
        };
    }

    // TODO(run:trace): Someone can play and make this composable, but I do not think
    //                  it is worth the boilerplate.
    auto raw_size_decoder( circ::Ctx &ctx )
    {
        return [ & ]( const std::string &data ) -> std::size_t
        {
            return do_decode( ctx )( data ).bytes.size();
        };
    }

    auto capturing_size_decoder( circ::Ctx &ctx, std::vector< remill::Instruction > &into )
    {
        return [ & ]( const std::string &data ) -> std::size_t
        {
            auto inst = do_decode( ctx )( data );
            auto out = inst.bytes.size();
            into.push_back( std::move( inst ) );
            return out;
        };
    }

    struct loader_base
    {
      protected:
        circ::Ctx ctx{ "macos", "x86" };

      public:
        loader_base() = default;

      protected:

        template< typename Fn, typename ... Args >
        auto parse_alien_trace( const std::string &source_file,
                                Fn &&mk_decoder, Args && ... args )
        {
            auto decoder = mk_decoder( ctx, std::forward< Args >( args ) ... );
            return mttn::load( source_file, decoder );
        }
    };

    struct alien_loader : loader_base
    {
        using loader_base::loader_base;

        auto parse_alien_trace( const std::string &source_file )
        {
            return loader_base::parse_alien_trace( source_file, raw_size_decoder );
        }
    };

    struct with_reconstructor : loader_base
    {
        using loader_base::loader_base;

        std::vector< remill::Instruction > seen;

        auto parse_alien_trace( const std::string &source_file )
        {
            return loader_base::parse_alien_trace( source_file,
                                                   capturing_size_decoder, seen );
        }

        auto reconstruct() &&
        {
            auto k = lifter_kind::disjunctions;
            return CircuitSmithy( std::move( ctx ) ).make( k, std::move( seen ) );
        }
    };

    template< typename T >
    concept loader_with_circuit_ctor =  requires (T loader)
    {
        { std::move( loader ).reconstruct() } -> std::same_as< circuit_owner_t >;
    };

    // TODO(run:trace): This is relatively cold path, so maybe use `virtual`?

    template< typename self_t >
    struct collector_base
    {
        auto get_collector(auto &to_export)
        {
            return [ & ]( const auto &result_spawn_pairs )
            {
                for ( auto &[ status, spawn ] : result_spawn_pairs )
                    if ( circ::run::accepted( status ) )
                    {
                        auto [ current , next ] = spawn->to_traces();
                        // We need to also include the first entry,
                        // which will never be the first
                        // item of the `to_traces` as it is never an input.
                        if ( to_export.empty() )
                            to_export.push_back( std::move( current ) );

                        to_export.push_back( std::move( next ) );
                        return;
                    }
                static_cast< self_t & >( *this ).on_error( result_spawn_pairs );
            };
        }
    };

    struct killing_collector : collector_base< killing_collector >
    {
        void on_error(const auto &)
        {
            circ::log_kill() << "[run::trace]:" << "No spawn was successful.";
        }
    };

    struct reporting_collector : collector_base< reporting_collector >
    {
        bool failure = false;

        void on_error(const auto &)
        {
            circ::log_info() << "[run::trace]:" << "No spawn was successful.";
            failure = true;
        }
    };

    template< typename collector_t = reporting_collector >
    struct trace_converter : collector_t
    {
        using self_t = trace_converter;

        std::vector< std::string > to_export;

        auto convert_trace( const auto &traces, circuit_ref_t circuit ) -> self_t &
        {
            circ::log_dbg() << "[run::trace]:" << "Permutating memory hints!";
            auto collect = collector_t::get_collector( to_export );
            // TODO(run:trace): What to do with results?
            std::ignore = circ::run::StatelessControl().test( circuit, traces, collect );

            circ::log_info() << "[run::trace]:" << "Conversion done.";
            return *this;
        }

        auto dump( const std::string &path ) -> self_t &
        {
            std::ofstream ofile( path );
            check( ofile );
            ofile << *this;

            return *this;
        }

        template< typename stream >
        friend stream &operator<<( stream &out, const self_t &self )
        {
            for ( const auto &entry : self.to_export )
                out << entry << "\n";
            return out;
        }
    };

} // namespace circ::run::trace
