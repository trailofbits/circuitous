#include "circuitous/Decoder/GenerationHelpers.hpp"

#include "circuitous/Decoder/DecodeAST.hpp"
#include "circuitous/Decoder/SEGGraph.hpp"
#include "circuitous/IR/Shapes.hpp"

#include <gap/core/graph.hpp>

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

    std::pair< size_t, FunctionDeclaration > IndependentSelectEmissionHelper::get_function( Select *select, VerifyInstruction *vi )
    {
        auto helper = create_helper( select, vi );
        register_if_not_in_cache( helper );
        return { helper.node_count, helper.fd };
    }


    bool are_trees_isomorphic( const std::vector< Operation * > &ops )
    {
        if ( ops.size() < 2 )
            return true;

        print::CompactPrinter cp;
        std::string first_hash = cp.Hash( ops[ 0 ] );
        return std::all_of( ops.begin() + 1, ops.end(),
                            [ & ]( Operation *op ) {
//                                std::cout << "comparing: " << cp.Hash(op) << " = " << first_hash << std::endl;
                                return cp.Hash( op ) == first_hash; } );
    }



    select_emission_helper
    IndependentSelectEmissionHelper::create_helper( Select *sel, VerifyInstruction *vi )
    {
        FunctionDeclarationBuilder fdb;

        auto select_values = freeze<std::vector>(sel->operands());
        select_values.erase(select_values.begin(), select_values.begin());

        if ( !are_trees_isomorphic( select_values ) )
            circ::unreachable() << "adding select helper for non-isomorphic select";

        decoder::Var select_index = decoder::Var( "index", Type( "uint64_t" ) );
        fdb.arg_insert( decoder::VarDecl( select_index ) );

        // start at 1 to account for selector
        size_t block_size = 0;
        for ( std::size_t i = 1; i < sel->operands_size(); i++ )
        {
            std::vector< decoder::Expr > block;
            auto start_op_ptr = std::make_shared< nodeWrapper >( sel->operand( i ) );
            for ( auto x : gap::graph::dfs< gap::graph::yield_node::on_open >( start_op_ptr ) )
            {
                block.push_back( decoder::Id( x->op->name() ) );
            }

            decoder::Tuple tuple( block.size() );

            // TODO(sebas) turn if statements into switch
            auto if_expr = decoder::If(
                decoder::Equal( select_index, decoder::Int( static_cast< int64_t >( i ) ) ),
                decoder::Return( tuple.Construct( block ) ) );
            fdb.body_insert( if_expr );
            fdb.retType( tuple.get_type() );
            block_size = block.size();
        }

        std::stringstream ss;
        ExpressionPrinter ep( ss );
        ep.print( fdb.make() );
        auto hash = std::hash< std::string > {}( ss.str() );
        fdb.name( "select_hash_" + std::to_string( hash ) );

        return select_emission_helper { block_size, hash, fdb.make() };
    }

    void IndependentSelectEmissionHelper::register_if_not_in_cache( const select_emission_helper &sel )
    {
        if(std::none_of( cache.begin(), cache.end(),
                      [ & ]( const select_emission_helper &p ) {
                          return p.hash_func_decl_without_name
                                 == sel.hash_func_decl_without_name;
                      } ))
            cache.push_back(sel);
    }
};
