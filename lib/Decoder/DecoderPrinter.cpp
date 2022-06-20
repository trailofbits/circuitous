#include <circuitous/Decoder/DecoderPrinter.hpp>
#include <circuitous/Printers.hpp>

#include <circuitous/IR/Shapes.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <cstdlib>

namespace circ::decoder {
    const Var DecoderPrinter::innerFuncArg1 = Var("first8bytes", "uint64_t");
    const Var DecoderPrinter::innerFuncArg2 = Var("second8bytes", "uint64_t");

    bool operator<(const DTI &x, const DTI &y) {
        std::size_t min_x = 0;
        std::size_t min_y = 0;
        if ( x.zeros.size() < x.ones.size())
            min_x = x.zeros.size();
        else
            min_x = x.ones.size();

        if ( y.zeros.size() < y.ones.size())
            min_y = y.zeros.size();
        else
            min_y = y.ones.size();

        return min_x < min_y;
    };

    // Assumes lsb is placed first
    uint64_t it_ignored_bits_to_uint(std::array< InputType, 64 > input) {
        uint64_t val = 0;
        for (std::size_t i = 0; i < 64; i++) {
            if ( input[ i ] == InputType::ignore )
                val |= to_val( InputType::ignore ) << i;
            else
                val |= to_val_negated( InputType::ignore ) << i;
        }
        return val;
    }

    uint64_t it_to_uint(std::array< InputType, 64 > input) {
        uint64_t val = 0;
        for (std::size_t i = 0; i < 64; i++) {
            if ( input[ i ] == InputType::ignore )
                val |= to_val_negated( InputType::ignore ) << i;
            else
                val |= to_val( input[ i ] ) << i;
        }
        return val;
    }


    Expr DecoderPrinter::print_decoder_func(const ExtractedVI &evi) {
        FunctionDeclaration funcDecl;
        funcDecl.function_name = evi.generated_name;
        funcDecl.retType = "static bool";

        funcDecl.args = {VarDecl( innerFuncArg1 ), VarDecl( innerFuncArg2 )};

        auto args = get_decode_context_function_args( evi );
        funcDecl.body.emplace_back(
                get_decode_context_function_body( args.first, args.second ));
        return funcDecl;
    }

    std::vector< std::array< InputType, 8>>
    DecoderPrinter::convert_circIR_to_input_type_array(const ExtractedVI &evi) {
        std::vector< std::array< InputType, 8 >> input_checks;
        for (std::size_t i = 0; i < 16; i++) {
            auto a = std::array< InputType, 8 >{InputType::ignore, InputType::ignore,
                                                InputType::ignore, InputType::ignore,
                                                InputType::ignore, InputType::ignore,
                                                InputType::ignore, InputType::ignore};
            for (auto &n: evi.decodeConditions) {
                auto lhsn = dynamic_cast<circ::Constant *>(n->operands[ 0 ]);
                auto extract = dynamic_cast<circ::Extract *>(n->operands[ 1 ]);
                auto low = extract->low_bit_inc;
                auto high_inc = extract->high_bit_exc - 1;

                // out of range of considered byte
                if ( low > (i + 1) * 8 || high_inc < i * 8 || high_inc == 119 ) {
                    continue;
                }

                for (std::size_t c = 0; c < 8; c++) {
                    auto bit_index = (i * 8) + c;
                    if ( low <= bit_index && bit_index <= high_inc ) {
                        auto new_val = [&]() {
                            if ( lhsn->bits[ bit_index - low ] == '1' )
                                return InputType::one;
                            else return InputType::zero;
                        }();
                        a[ c ] = new_val;
                    }
                }
            }
            input_checks.push_back( a );
        }
        return input_checks;
    }


    void DecoderPrinter::print_file() {
        extract_evi();

        os << "#include <array>" << std::endl;
        os << "#include <stdint.h>" << std::endl << std::endl;
        for (auto &evi: extractedVIs) {
            printExpr(print_decoder_func( evi ), os);
        }
        printExpr(print_circuit_decoder(), os);
        os << std::endl;
    }

    void DecoderPrinter::extract_evi() {
        SubtreeCollector< VerifyInstruction > sc;
        auto VIs = sc.Run( circuit.get()->operands[ 0 ] ).collected;
        for (auto &vi: VIs) {
            SubtreeCollector< DecodeCondition > dc_collector;
            dc_collector.Run( vi->operands );
            std::unordered_multiset< DecodeCondition * > decNodes = dc_collector.collected;
            auto x =
                    std::find_if( decNodes.begin(), decNodes.end(), [&](DecodeCondition *dc) {
                        auto rhs = dynamic_cast<circ::Extract *>(dc->operands[ 1 ]);
                        return rhs->high_bit_exc == 120;
                    } );
            if ( x == decNodes.end()) {
                throw std::invalid_argument( "No decode condition that specifies end" );
            }
            auto rhs = dynamic_cast<circ::Extract *>((*x)->operands[ 1 ]);
            auto encoding_length = floor( rhs->low_bit_inc / 8 );
            if ( encoding_length > 15 ) {
                throw std::invalid_argument( "Instruction is longer than 15 bytes" );
            }
            extractedVIs.emplace_back( ExtractedVI(
                    vi, "generated_decoder_prefix_" + std::to_string( vi->id()),
                    static_cast<uint8_t>(encoding_length),
                    std::move( decNodes )));
        }
    }

    Expr DecoderPrinter::print_circuit_decoder() {
        FunctionDeclaration funcDecl;
        funcDecl.function_name = circuit_decode_function_name;
        funcDecl.retType = "int";
        funcDecl.args.emplace_back(Var(bytes_input_variable, "std::array<uint8_t,15>"));

        funcDecl.body.emplace_back(print_conversion_input_to_uints64());

        std::vector< ExtractedVI * > to_split;
        for (auto &evi: extractedVIs) {
            to_split.push_back( &evi );
        }
        auto tree = print_split( to_split, std::vector< std::pair< std::size_t, int>>(), 0 );
        funcDecl.body.emplace_back(tree);

        std::cout << "max depth during gen: " << max_depth << " for size of: "
                  << std::to_string( extractedVIs.size()) << std::endl;

        funcDecl.body.emplace_back(Return(Int(-1)));
        return funcDecl;
    }

    Expr DecoderPrinter::print_conversion_input_to_uints64() {
        auto array_input = Var( bytes_input_variable);
        StatementBlock b;
        convert_array_input_to_uint64( array_input, innerFuncArg1, b, false );
        convert_array_input_to_uint64( array_input, innerFuncArg2, b, true );
        return b;
    }

    void DecoderPrinter::convert_array_input_to_uint64(const Var &array_input, const Var &arg,
                                                       StatementBlock &b, bool second_uint) {
        b.emplace_back(Statement(Assign(VarDecl(arg),Int(0))));
        for (uint i = 0; i < 8; i++) {
            auto indexdVar = IndexVar( array_input, i);
            if(second_uint){
                indexdVar = IndexVar( array_input, i+8); // index 8 further
            }
            auto set =Parenthesis(CastToUint64(Shfl(indexdVar, Int( static_cast<const int>(8 * i)))));
            b.emplace_back(Statement(Assign(arg, Plus(arg, set))));
        }
    }


    bool DecoderPrinter::contains_ignore_bit(const std::array< InputType, 64 > &byte) {
        for (auto &b: byte) {
            if ( b == InputType::ignore ) {
                return true;
            }
        }
        return false;
    }

    bool DecoderPrinter::contains_only_ignore_bit(const std::array< InputType, 64 > &byte) {
        for (auto &b: byte) {
            if ( b != InputType::ignore ) {
                return false;
            }
        }
        return true;
    }

    std::string print_chosen_bits(std::vector< std::pair< std::size_t, int>> chosen) {
        std::stringstream ss;
        for (std::size_t i = 0; i < 120; i++) {
            auto it = std::find_if( chosen.begin(), chosen.end(),
                                    [&](std::pair< std::size_t, int > p) {
                                        return p.first == i;
                                    } );
            if ( it != chosen.end()) {
                // ss << std::hex << std::to_string((*it).second);
                ss << "x";
            } else {
                ss << "~";
            }
        }
        return ss.str();
    }

    Expr
    DecoderPrinter::get_decode_context_function_body(const decode_context_function_arg &arg1,
                                                     const decode_context_function_arg &arg2) {
        StatementBlock block;
        block.emplace_back( print_ignore_bits( arg1 ));
        block.emplace_back( print_ignore_bits( arg2 ));

        std::vector< Expr> compare_exprs;
        if ( !contains_only_ignore_bit( arg1.byte ))
            compare_exprs.push_back( get_comparison( arg1 ));

        if ( !contains_only_ignore_bit( arg2.byte ))
            compare_exprs.push_back( get_comparison( arg2 ));

        if ( compare_exprs.size() == 1 )
            block.emplace_back( Return( compare_exprs[ 0 ] ));
        else
            block.emplace_back( Return( And( compare_exprs[ 0 ], compare_exprs[ 1 ] )));

        return block;
    }

    Expr DecoderPrinter::get_comparison(const decode_context_function_arg &arg ) {
        auto uint = Uint64(it_to_uint( arg.byte ));
        auto lhs = CastToUint64( Parenthesis( BitwiseXor( arg.var.name, BitwiseNegate( uint ))));

        // negation of ignore bits are the bits we care about
        auto neg_ignore = Uint64(~(it_ignored_bits_to_uint( arg.byte )));
        return  Equal( lhs, neg_ignore );
    }

    Expr DecoderPrinter::print_ignore_bits(const decode_context_function_arg &arg) {
        if ( contains_ignore_bit( arg.byte ) && !contains_only_ignore_bit( arg.byte )) {
            auto name = arg.var.name;
            auto ignoringValues = Uint64( it_ignored_bits_to_uint( arg.byte ));
            return Statement( Assign( name, BitwiseOr( name, ignoringValues )));
        }
        return Empty();
    }

    std::string get_bits_string(ExtractedVI *evi) {
        std::stringstream ss;
        for (std::size_t i = 0; i < 120; i++) {
            auto val = InputType::ignore;
            for (auto &n: evi->decodeConditions) {
                auto lhsn = dynamic_cast<circ::Constant *>(n->operands[ 0 ]);
                auto extract = dynamic_cast<circ::Extract *>(n->operands[ 1 ]);
                auto low = extract->low_bit_inc;
                auto high_inc = extract->high_bit_exc - 1;


                // out of range of considered byte
                if ( low > i || high_inc < i || high_inc == 119 ) {
                    continue;
                }

                if ( lhsn->bits[ i - low ] == '0' )
                    val = InputType::zero;
                else
                    val = InputType::one;
            }
            if ( val == InputType::zero )
                ss << "0";
            else if ( val == InputType::one )
                ss << "1";
            else
                ss << "~";
        }
        return ss.str();
    }

    /*
     * prints split in os
     * either if(check bit == 1) { split(ones) } else split(zeros)
     * or if split.size() == 1 :return evi(input)
     */
    /*
        * ok were gonna do this optimally
        *
        * we will consider for each context the bit string that represents the instruction
        * with this we will count at every index if they prefer a 1 or 0
        * so we now have
        *
        * index 0 1 2 3 4 5 6
        * zeros 3 0 2 3 1 4 5
        * ones  2 3 4 0 2 3 4
        *
        * take pairs (0,1) for each the same index
        * we take the maximum of the minimum value of a pair (0,1) and use that as split point for
        * binary search, note that the sums of each pair do not need to sum to the same amount due to dont cares
        *
        * best case: log(f) with f #decode functions
        * worse case: f (?)
        */

    Expr DecoderPrinter::print_split(std::vector< ExtractedVI * > to_split,
                                            std::vector< std::pair< std::size_t, int>> already_chosen_bits,
                                            int depth) {
        if ( this->max_depth < depth )
            max_depth = depth;
        std::array< DTI, 120 > indice_values;

        if ( to_split.empty() ) {
            return Return(Int(-1));
        }

        if ( to_split.size() == 1 ) {
            return Return( evi_call( *to_split[ 0 ] ));
        }

        for (std::size_t i = 0; i < 120; i++) {
            for (auto &evi: to_split) {
                if ( std::find_if( already_chosen_bits.begin(), already_chosen_bits.end(),
                                   [&](std::pair< std::size_t, int > p) {
                                       return p.first == i;
                                   } ) != already_chosen_bits.end()) {
                    continue;
                }

                auto val = InputType::ignore;
                for (auto &n: evi->decodeConditions) {
                    auto lhsn = dynamic_cast<circ::Constant *>(n->operands[ 0 ]);
                    auto extract = dynamic_cast<circ::Extract *>(n->operands[ 1 ]);
                    auto low = extract->low_bit_inc;
                    auto high_inc = extract->high_bit_exc - 1;


                    // out of range of considered byte
                    if ( low > i || high_inc < i || high_inc == 119 ) {
                        continue;
                    }

                    if ( lhsn->bits[ i - low ] == '0' )
                        val = InputType::zero;
                    else
                        val = InputType::one;
                }
                /*
                 * The bit which gets checked should only be in a single decode condition
                 * otherwise multiple decode conditions checking over other ranges would add
                 * way to many ignores
                 */

                if ( val == InputType::zero )
                    indice_values[ i ].zeros.push_back( evi );
                else if ( val == InputType::one )
                    indice_values[ i ].ones.push_back( evi );
                else
                    indice_values[ i ].ignores.push_back( evi );
            }
        }

        auto max = std::max_element( indice_values.begin(), indice_values.end());
        auto ci = static_cast<std::size_t>(std::distance( indice_values.begin(), max ));
        already_chosen_bits.push_back( std::make_pair( ci, depth ));


        /*
         * The encodings with dont care on the candidate index can both be a 0 or 1
         * Hence they need to be in both candidates sets from this point onwards
         */
        for (auto &ignored: indice_values[ ci ].ignores) {
            indice_values[ ci ].zeros.push_back( ignored );
            indice_values[ ci ].ones.push_back( ignored );
        }



        auto ci_byte = static_cast<uint32_t>(ci / 8);
        auto lhs = IndexVar(Var(bytes_input_variable), ci_byte);
        auto rhs2 =   Parenthesis(Shfl(Int(1), Int(ci % 8)));
        auto c = BitwiseAnd( lhs, rhs2); // var & (1<< ci)

        std::vector< ExtractedVI * > ones = indice_values[ ci ].ones;
        std::vector< ExtractedVI * > zeros = indice_values[ ci ].zeros;

        if ((ones.size() == 1 && zeros.size() > 3)) {
            std::cout << "imbalance at depth " << depth << " size in other tree: "
                      << std::to_string( zeros.size()) << std::endl;
        }
//
        if ((zeros.size() == 1 && ones.size() > 3)) {
            std::cout << "----------------" << std::endl;

            std::cout << "imbalance at depth " << depth << " size in other tree: "
                      << std::to_string( ones.size()) << " candidate index: " << ci
                      << std::endl;
            std::vector< std::pair< std::size_t, int>> a;
            a.push_back( std::make_pair( ci, 0 ));
            std::cout << "index:    " << print_chosen_bits( a ) << std::endl;
            std::cout << "chosen:   " << print_chosen_bits( already_chosen_bits ) << std::endl;
            std::cout << "zeros[0]: " << get_bits_string( zeros[ 0 ] ) << std::endl;
            for (std::size_t i = 0; i < ones.size(); i++) {
                std::cout << "ones[" + std::to_string( i ) + "]:  "
                          << get_bits_string( ones[ i ] ) << std::endl;
            }
            std::cout << "----------------" << std::endl << std::endl;
        }
        //if one of the branches is zero, dont create an if else statement, just create an if
        //TODO I think there is a bug with that still allows this to be reached where both ones and zeros is empty
        if ( ones.size() == 0 && zeros.size() == 0 ) {
            return Return(Int(-1));
        }

        auto lhs_split =print_split( ones, already_chosen_bits, depth + 1 );
        auto rhs_split =print_split( zeros, already_chosen_bits, depth + 1 );

        return IfElse(c,lhs_split, rhs_split);
    }

    Expr DecoderPrinter::evi_call(const ExtractedVI &evi) {
        FunctionCall fc;
        fc.function_name = evi.generated_name;
        fc.args.emplace_back(innerFuncArg1);
        fc.args.emplace_back(innerFuncArg2);
        return fc;
    }


    std::pair< decode_context_function_arg, decode_context_function_arg >
    DecoderPrinter::get_decode_context_function_args(const ExtractedVI &evi) {
        std::vector< std::array< InputType, 8>> input_checks = convert_circIR_to_input_type_array(
                evi );
        std::array< InputType, 64 > val;
        for (std::size_t i = 0; i < 64; i++) {
            val[ i ] = input_checks[ i / 8 ][ i % 8 ];
        }
        std::array< InputType, 64 > val2;
        for (std::size_t i = 0; i < 64; i++) {
            val2[ i ] = input_checks[ 8 + i / 8 ][ i % 8 ];
        }
        auto arg1 = decode_context_function_arg( val, innerFuncArg1 );
        auto arg2 = decode_context_function_arg( val2, innerFuncArg2 );
        return std::pair< decode_context_function_arg, decode_context_function_arg >( arg1,
                                                                                      arg2 );
    }

    uint64_t to_val(const InputType &ty) {
        switch (ty) {
            case InputType::zero:
                return 0;
            case InputType::one:
                return 1;
            case InputType::ignore:
                return 1;
        }
    }

    uint64_t to_val_negated(const InputType &ty) {
        switch (ty) {
            case InputType::zero:
                return 1;
            case InputType::one:
                return 0;
            case InputType::ignore:
                return 0;
        }
    }

}  // namespace circ::disassm