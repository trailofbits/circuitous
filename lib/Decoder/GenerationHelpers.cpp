#include "circuitous/Decoder/GenerationHelpers.hpp"

#include "circuitous/Decoder/DecodeAST.hpp"

namespace circ::decoder
{
    Tuple::Tuple( size_t size ) : size( size ) { }

    FunctionCall Tuple::Construct( std::vector< Expr > values )
    {
        circ::check( values.size() == size )
            << "Trying to construct a tuple with mismatching number of parameters, expected: "
            << size << " got: " << values.size();

        return FunctionCall( "std::tuple", values,
                             std::vector< Expr >( values.size(), Id( "VisitorReturnType" ) ) );
    }

    FunctionCall Tuple::get( Expr tuple, size_t index )
    {
        circ::check( index <= size - 1 ) << "index is out of bounds";
        return FunctionCall( "std::get", { tuple }, { Uint64 { index } } );
    }

    Type Tuple::get_type()
    {
        return Type( "std::tuple",
                     std::vector< Expr >( size, Id( "VisitorReturnType" ) ) );
    }
};
