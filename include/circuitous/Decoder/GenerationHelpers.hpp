#pragma once

#include "circuitous/Decoder/DecodeAST.hpp"

namespace circ::decoder
{
    Id generate_tuple_of_visitor_type(int size);

    struct Tuple
    {
        explicit Tuple( size_t size );

        Type get_type();
        FunctionCall Construct(std::vector< Expr > values);
        FunctionCall get( Expr tuple, size_t index );

        size_t size;
    };

    struct select_emission_helper
    {
        size_t node_count;
        size_t hash_func_decl_without_name;
        FunctionDeclaration fd;
    };

    struct IndependentSelectEmissionHelper
    {
        std::pair< size_t, FunctionDeclaration > get_function( Select *select,
                                                            VerifyInstruction *vi );
        std::vector< select_emission_helper > cache;

    private:
        select_emission_helper create_helper( Select *select, VerifyInstruction *vi );
        void register_if_not_in_cache( const select_emission_helper &sel );
    };
}
