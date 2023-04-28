#pragma once

#include "circuitous/Decoder/DecodeAST.hpp"

namespace circ::decoder
{
    Id generate_tuple_of_visitor_type(int size);
    struct Tuple
    {
        explicit Tuple( size_t size );

        Id get_type();
        FunctionCall Construct(std::vector< Expr > values);
        FunctionCall get( Expr tuple, size_t index );

        size_t size;
    };
}
