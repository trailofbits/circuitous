#pragma once

#include "circuitous/Decoder/DecodeAST.hpp"
#include "circuitous/IR/Visitors.hpp"

namespace circ::decoder
{
    const Var inner_func_arg1( "first8bytes", Type("uint64_t"));
    const Var inner_func_arg2( "second8bytes", Type("uint64_t"));
    inline static const std::array<Var,2> inner_func_args = {inner_func_arg1, inner_func_arg2};

    static constexpr const auto extract_helper_function_name = "extract_helper";

    Operation *get_op_attached_to_advice_in_vi( Advice *advice, VerifyInstruction *vi );
    Type get_value_type();
    std::string to_string(Expr expr);

    struct Tuple
    {
        explicit Tuple( size_t size );

        Type get_type();
        FunctionCall Construct(std::vector< Expr > values);
        FunctionCall get( Expr tuple, size_t index );

        size_t size;
    };

    struct SimpleDecodeTimeCircToExpressionVisitor : Visitor<SimpleDecodeTimeCircToExpressionVisitor>
    {
        explicit SimpleDecodeTimeCircToExpressionVisitor( VerifyInstruction *vi,
                                                          const decoder::Var &first8Bytes,
                                                          const decoder::Var &second8Bytes,
                                                          const std::string &extractHelper ) :
            vi( vi ),
            first_8_bytes( first8Bytes ), second_8_bytes( second8Bytes ),
            extract_helper( extractHelper )
        {
        }

        decoder::Expr visit( Advice *advice )
        {
            return dispatch(get_op_attached_to_advice_in_vi( advice, vi ));
        }

        decoder::Expr visit( Concat *concat )
        {
            check( concat->operands_size() > 0 ) << "concat cannot be a leaf";
            auto first_child = concat->operand(concat->operands_size() - 1);
            decoder::Expr tmp = dispatch(first_child);
            auto size_offset = first_child->size;
            for(long i = static_cast< long >( concat->operands_size() - 2 ); i >= 0; i--)
            {
                auto child = concat->operand( static_cast< size_t >( i ) );
                auto new_val = dispatch(child);
                tmp = decoder::Plus( tmp, decoder::Shfl( new_val, decoder::Int(size_offset) ) );
                size_offset = concat->operand( static_cast< size_t >( i ) )->size;
            }
            return tmp;
        }

        decoder::Expr visit( Extract* extract)
        {
            check(extract->operands_size() == 1) << "extracting from multiple nodes is not supported";
            check( isa<InputInstructionBits>( extract->operand(0) ) ) << "Requires to extract from input bytes for now";
            auto low = decoder::Int(extract->low_bit_inc);
            auto high = decoder::Int(extract->high_bit_exc);
            return decoder::FunctionCall(extract_helper, { first_8_bytes, second_8_bytes}, { low, high } );
        }

        decoder::Expr visit( Operation *op )
        {
            circ::unreachable() << "Not supported: " << op->name();
        }

        VerifyInstruction *vi;
        const decoder::Var first_8_bytes;
        const decoder::Var second_8_bytes;
        const std::string extract_helper;
    };

    struct GeneratedSelectHelper
    {
        size_t node_count;
        size_t hash_func_decl_without_name;
        FunctionDeclaration fd;
    };

    struct IndependentSelectEmissionHelper
    {
        std::pair< size_t, FunctionDeclaration > get_function( Select *select,
                                                               VerifyInstruction *vi );
        std::vector< GeneratedSelectHelper > cache;

    private:
        GeneratedSelectHelper create_helper( Select *select, VerifyInstruction *vi );
        void register_if_not_in_cache( const GeneratedSelectHelper &sel );
    };

    //TODO(sebas): Move this to somewhere in IR?
    bool are_trees_isomorphic( Operation *lhs, Operation *rhs );
    bool are_trees_isomorphic( const std::vector< Operation * > &ops );

}
