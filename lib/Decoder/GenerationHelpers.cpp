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
                             std::vector< Expr >( values.size(), get_value_type().name ) );
    }

    FunctionCall Tuple::get( Expr tuple, size_t index )
    {
        circ::check( index <= size - 1 ) << "index is out of bounds";
        return FunctionCall( "std::get", { tuple }, { Uint64 { index } } );
    }

    Type Tuple::get_type()
    {
        return Type( "std::tuple",
                     std::vector< Expr >( size, get_value_type() ) );
    }

    std::pair< size_t, FunctionDeclaration > IndependentSelectEmissionHelper::get_function(
        Select *select, VerifyInstruction *vi )
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



    GeneratedSelectHelper
    IndependentSelectEmissionHelper::create_helper( Select *sel, VerifyInstruction *vi )
    {
        FunctionDeclarationBuilder fdb;

        auto select_values = freeze<std::vector>(sel->operands());
        select_values.erase(select_values.begin(), select_values.begin());

        if ( !are_trees_isomorphic( select_values ) )
            circ::unreachable() << "adding select helper for non-isomorphic select";

        for(auto& arg : inner_func_args)
            fdb.arg_insert( arg );

        // start at 1 to account for selector
        size_t block_size = 0;

        SimpleDecodeTimeCircToExpressionVisitor decode_time_decoder(
            vi, inner_func_arg1, inner_func_arg2, extract_helper_function_name );

        auto selector = Var("indx", Type("uint64"));
        auto selector_value = decode_time_decoder.dispatch( sel->selector() );
        auto selector_init = Assign(VarDecl(selector), selector_value );
        fdb.body_insert( Statement( selector_init ) );
        Switch sw( selector );
        for ( std::size_t i = 1; i < sel->operands_size(); i++ )
        {
            std::vector< decoder::Expr > block;
            auto start_op_ptr = std::make_shared< nodeWrapper >( sel->operand( i ) );
            for ( auto x : gap::graph::dfs< gap::graph::yield_node::on_open >( start_op_ptr ) )
            {
                block.push_back( decoder::Id( x->op->name() ) );
            }
            //TODO(sebas): Change emission to a direct type instead of tuple if we only have a single value
            auto c = Equal( selector, decoder::Int( static_cast< int64_t >( i ) ) );
            Return return_val( Id( "" ) );
            Type return_type( Id( "" ) );
            if ( block.size() > 1 )
            {
                decoder::Tuple tuple( block.size() );
                return_val = Return( tuple.Construct( block ) );
                return_type = tuple.get_type();
            }
            else
            {
                return_val = Return( block[ 0 ] );
                return_type = get_value_type();
            }
            sw.cases.push_back( { c, return_val } );
            fdb.retType( return_type );
            block_size = block.size();
        }

        fdb.body_insert(sw);
        std::stringstream ss;
        ExpressionPrinter ep( ss );
        ep.print( fdb.make() );
        auto hash = std::hash< std::string > {}( ss.str() );
        fdb.name( "select_hash_" + std::to_string( hash ) );

        return GeneratedSelectHelper { block_size, hash, fdb.make() };
    }

    void IndependentSelectEmissionHelper::register_if_not_in_cache( const GeneratedSelectHelper &sel )
    {
        if(std::none_of( cache.begin(), cache.end(),
                      [ & ]( const GeneratedSelectHelper &p ) {
                          return p.hash_func_decl_without_name
                                 == sel.hash_func_decl_without_name;
                      } ))
            cache.push_back(sel);
    }

    Type get_value_type()
    {
        return Type("VisRetType");
    }

    std::string to_string( Expr expr )
    {
        std::stringstream ss;
        ExpressionPrinter ep( ss );
        ep.print( expr );
        return ss.str();
    }
};
