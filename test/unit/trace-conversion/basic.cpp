/*
 * Copyright (c) 2023, Trail of Bits, Inc.
 * All rights reserved.
 *
 * This source code is licensed in accordance with the terms specified in
 * the LICENSE file found in the root directory of this source tree.
 */

#include <doctest/doctest.h>

#include <circuitous/Run/TraceConversion.hpp>

#include <filesystem>

namespace circ::run::test
{
    void setup_loggers()
    {
        circ::add_sink< circ::severity::kill >(std::cerr);
        circ::add_sink< circ::severity::error >(std::cerr);
    }

    bool execute( const std::filesystem::path &fname )
    {
        setup_loggers();

        trace::with_reconstructor loader;
        auto traces = loader.parse_alien_trace( fname );
        auto circuit = std::move( loader ).reconstruct();

        trace::trace_converter< trace::reporting_collector > c;
        c.convert_trace( traces, circuit.get() );
        return !c.failure;
    }

    std::filesystem::path mk_path( const std::string &test_name )
    {
        return std::filesystem::path("trace-conversion") /
               "inputs" /
               (test_name + ".trace.txt");

    }

    #define MK_TEST(name) \
        TEST_CASE( name ) { CHECK( execute( mk_path( name ) ) ); }

    TEST_SUITE( "run::basic" )
    {
        MK_TEST( "alu_adc" );
        MK_TEST( "jmp" );
        MK_TEST( "lea" );
        MK_TEST( "push_pop" );

        // These are TODO, as the test inputs do behave inconsistently
        // w.r.t DF register.
        //MK_TEST( "memops" );
        //MK_TEST( "alu_add" );
        //MK_TEST( "alu_add_neg" );
        //MK_TEST( "push_pop2" );
    } // test suite: run::basic

} // namespace circ::run::test
