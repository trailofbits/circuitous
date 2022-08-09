#include <circuitous/Decoder/DecoderPrinter.hpp>
#include <circuitous/Printers.hpp>

#include <circuitous/IR/Shapes.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <cstdlib>

namespace circ::decoder {
    bool operator<(const Decode_Requires_Group &x, const Decode_Requires_Group &y) {
        std::size_t min_x = std::min(x.zeros, x.ones).size();
        std::size_t min_y = std::min(y.zeros, y.ones).size();

        return min_x < min_y;
    };

    Expr DecoderPrinter::create_context_decoder_function(const ExtractedCtx &ctx) {
        FunctionDeclarationBuilder fdb;
        fdb.name(ctx.generated_name);
        fdb.retType("int");
        for(auto& arg : inner_func_args){
            fdb.arg_insert(arg);
        }

        auto args = get_decode_context_function_args( ctx );
        fdb.body({ get_decode_context_function_body( args, ctx.encoding_size_in_bytes )});
        return fdb.make();
    }

    std::vector< OptionalBitArray<8> > ExtractedCtx::convert_circIR_to_input_type_array() const {
        std::vector< OptionalBitArray<8> > input_checks;
        for (std::size_t i = 0; i < MAX_ENCODING_LENGTH/8; i++) {
            auto val = OptionalBitArray<8>();
            for(uint32_t j = 0; j< 8; j++){
                val[j] = get_input_type_at_index(static_cast<std::size_t>((i*8)+j));
            }
            input_checks.push_back( val );
        }
        return input_checks;
    }

    InputType ExtractedCtx::get_input_type_at_index(std::size_t i) const {
        auto val = InputType::ignore;
        for (auto &dec: decodeConditions) {
            auto low = dec.low_bit_inc;
            auto high_inc = dec.high_bit_exc - 1;

            // out of range of considered byte
            if ( low > i || high_inc < i || high_inc == (MAX_ENCODING_LENGTH -1) ) {
                continue;
            }
            val = ctoit(dec.bits[ i - low ]);
        }
        return val;
    }


    void DecoderPrinter::print_file() {
        print_file_headers();

        ExpressionPrinter ep(os);
        for (auto &ctx: extracted_ctxs) {
            ep.print( create_context_decoder_function( ctx ));
        }
        ep.print( create_top_level_function());
        os << std::endl;
    }

    void DecoderPrinter::extract_ctx() {
        for (auto &vi: GetContexts( circuit.get()->operand(0) )) {
            SubtreeCollector< DecodeCondition > dc_collector;
            dc_collector.Run( vi->operands() );

            std::unordered_multiset< DecodeCondition * > decNodes = dc_collector.collected;

            auto encoding_length = get_encoding_length( decNodes );

            std::vector<ExtractedDecodeCondition> dec;
            for(auto decCond : decNodes){
                auto lhs = dynamic_cast<circ::Constant *>((*decCond).operand( 0 ));
                auto rhs = dynamic_cast<circ::Extract *>((*decCond).operand( 1 ));
                check(lhs && rhs) <<  "decoder condition malformed";

                dec.emplace_back(rhs->low_bit_inc, rhs->high_bit_exc, lhs->bits);
            }

            extracted_ctxs.emplace_back(
                    "generated_decoder_prefix_" + std::to_string( vi->id()),
                    static_cast<uint8_t>(encoding_length),
                    std::move( dec ));
        }
    }

    uint8_t DecoderPrinter::get_encoding_length(
            const std::unordered_multiset< DecodeCondition * > &decNodes) const {
        auto is_ending_check = [&](DecodeCondition *dc) {
            auto rhs = dynamic_cast<circ::Extract *>(dc->operand(1));
            check(rhs) << "invalid cast to Extract";
            return rhs->high_bit_exc == MAX_ENCODING_LENGTH;
        };
        auto endingEncoding = std::find_if( decNodes.begin(), decNodes.end(),is_ending_check);
        check(endingEncoding != decNodes.end()) << "No decode condition that specifies end" ;


        auto rhs = dynamic_cast<circ::Extract *>((*endingEncoding)->operand(1));
        check(rhs) << "invalid cast to Extract";

        auto encoding_length= std::floor( rhs->low_bit_inc / 8 );
        check(encoding_length < 15 ) << "Instruction is longer than 15 bytes" ;

        return static_cast<uint8_t>(encoding_length);
    }

    Expr DecoderPrinter::create_top_level_function() {
        FunctionDeclarationBuilder fdb;
        fdb.name(circuit_decode_function_name);
        fdb.retType("int");
        fdb.arg_insert( Var( bytes_input_variable, "std::array<uint8_t,15>" ));

        fdb.body_insert( get_top_level_arg_conversion());

        std::vector< ExtractedCtx * > to_split;
        for (auto &ctx: extracted_ctxs) {
            to_split.push_back( &ctx );
        }
        auto tree = generate_decoder_selection_tree( to_split,
                                                     std::vector< std::pair< std::size_t, int>>(),
                                                     0 );
        fdb.body_insert(tree);

        circ::log_dbg() << "max depth during gen: " << max_depth << " for size of: "
                        << std::to_string( extracted_ctxs.size());

        fdb.body_insert(Return(Int(-1)));
        return fdb.make();
    }

    Expr DecoderPrinter::get_top_level_arg_conversion() {
        auto array_input = Var( bytes_input_variable );
        StatementBlock b;
        for(std::size_t i = 0; i < inner_func_args.size(); i++){
            auto arg = top_to_inner_level_args( array_input, inner_func_args[ i ], i );
            b.push_back(arg);
        }

        return b;
    }

    std::vector< Expr >
    DecoderPrinter::top_to_inner_level_args(const Var &array_input, const Var &arg,
                                            size_t offset) {
        std::vector< Expr > st;
        st.emplace_back(Statement(Assign(VarDecl(arg),Int(0))));
        for (uint i = 0; i < 8; i++) {
            auto indexdVar = IndexVar( array_input, Int( static_cast<int64_t>(offset * 8 + i)));
            auto rhs = CastToUint64( Shfl( CastToUint64(indexdVar), Int( static_cast<const int>(8 * i))));
            st.emplace_back(Statement(Assign(arg, Plus( arg, rhs))));
        }
        return st;
    }

    Expr DecoderPrinter::get_decode_context_function_body(const decode_func_args &args,
                                                          int encoding_size) {
        StatementBlock block;
        for(auto& arg: args){
            block.emplace_back( create_ignore_bits_setter( arg ));
        }

        std::vector< Expr > compare_exprs;
        for(auto& arg: args){
            if ( !arg.byte.contains_only_ignore_bits())
                compare_exprs.push_back( get_comparison( arg ));
        }

        if ( compare_exprs.size() == 1 )
            block.emplace_back( Return( Mul( compare_exprs[ 0 ], Int( encoding_size ))));
        else
            block.emplace_back( Return( Mul( (And( compare_exprs[ 0 ], compare_exprs[ 1 ] )),
                                             Int( encoding_size ))));

        return block;
    }

    Expr DecoderPrinter::get_comparison(const decode_context_function_arg &arg ) {
        auto arg_value = Uint64( arg.byte.to_uint64());
        auto lhs = CastToUint64( BitwiseXor( arg.var.name, BitwiseNegate( arg_value )));

        // negation of ignore bits are the bits we care about
        auto neg_ignore = Uint64(~(arg.byte.ignored_bits_to_uint64()));
        return  Equal( lhs, neg_ignore );
    }

    Expr DecoderPrinter::create_ignore_bits_setter(const decode_context_function_arg &arg) {
        if ( !arg.byte.contains_ignore_bit() || arg.byte.contains_only_ignore_bits())
            return Empty();

        auto name = arg.var.name;
        auto ignoringValues = Uint64( arg.byte.ignored_bits_to_uint64() );
        return Statement( Assign( name, BitwiseOr( name, ignoringValues )));
    }

    /*
     * This function is meant to find the optimal amount of checks required to identify an encoding
     * Instead of calling every <decode_encoding> function once, we do a BST like search
     * to first find the best closest candidate and calling that.
     *
     * This algorithm looks like a regular backtracking algorithm from your standard algos class
     * but it doesn't have any backtracking since the input size can be 1000+,
     * and leaving it naively like this gives decent results (depth 17 instead of the optimal 12)
     *
     * How it works:
     *
     * we will consider for each encoding/context the bit string that represents the instruction
     * with this we will count at every index if the encoding _requires_ a 1 or 0.
     * Lastly, we will make an BST tree that at each check eliminates as many possible encodings
     *
     * Example:
     * index 0 1 2 3 4 5 6
     * zeros 3 0 2 3 1 4 5
     * ones  2 3 4 0 2 3 4
     *
     * We gain the most information by checking what bit is located at index 4
     * The best heuristic I found was looking at a pair of zeros/ones at an index
     * which had the maximal of the minimal value, as this would prevent cases weird cases
     * where you'd had a lot of imbalance.
     *
     * Note that the sum of zeros and ones do not need to sum the total amount of encodings
     * as some encodings accept both a 1 and 0 (don't cares).
     *
     * best case: log(f) with f #decode functions
     * worse case: f
     */
    Expr DecoderPrinter::generate_decoder_selection_tree(
            const std::vector< ExtractedCtx * > &to_split,
            std::vector< std::pair< std::size_t, int>> already_chosen_bits,
            int depth) {
        if ( this->max_depth < depth )
            max_depth = depth;

        if ( to_split.empty() ) {
            return Return(Int(-1));
        }

        if ( to_split.size() == 1 ) {
            auto ctx= *to_split[ 0 ];
            auto args = std::vector<Expr>(inner_func_args.begin(), inner_func_args.end());
            return Return( FunctionCall( ctx.generated_name, args));
        }

        auto indice_values = get_decode_requirements_per_index( to_split, already_chosen_bits );
        auto max = std::max_element( indice_values.begin(), indice_values.end());
        auto candidate_index = static_cast<std::size_t>(std::distance( indice_values.begin(), max ));
        already_chosen_bits.emplace_back( candidate_index, depth );

        /*
         * The encodings with dont care on the candidate index can both be a 0 or 1
         * Hence they need to be in both candidates sets from this point onwards
         */
        for (auto &ignored: indice_values[ candidate_index ].ignores) {
            indice_values[ candidate_index ].zeros.push_back( ignored );
            indice_values[ candidate_index ].ones.push_back( ignored );
        }

        auto ones = std::move(indice_values[ candidate_index ].ones);
        auto zeros = std::move(indice_values[ candidate_index ].zeros);

        if ( ones.empty() && zeros.empty() ) {
            return Return(Int(-1));
        }

        auto ci_byte = static_cast<uint32_t>(candidate_index / 8);
        auto lhs = IndexVar(Var(bytes_input_variable), Int(ci_byte));
        auto rhs = Shfl(Int(1), Int( candidate_index % 8));
        auto condition = BitwiseAnd( lhs, rhs); // var & (1<< ci)


        auto lhs_split = generate_decoder_selection_tree( ones, already_chosen_bits, depth + 1 );
        auto rhs_split = generate_decoder_selection_tree( zeros, already_chosen_bits,
                                                          depth + 1 );

        return IfElse( condition, lhs_split, rhs_split);
    }

    std::array< Decode_Requires_Group, MAX_ENCODING_LENGTH >
    DecoderPrinter::get_decode_requirements_per_index(
            const std::vector< ExtractedCtx * > &to_split,
            const std::vector< std::pair< std::size_t, int>> &already_chosen_bits) {

        std::array< Decode_Requires_Group, MAX_ENCODING_LENGTH > indice_values;
        auto index_is_already_chosen = [&](std::size_t i){
            return std::find_if( already_chosen_bits.begin(), already_chosen_bits.end(),
                                 [&](std::pair< std::size_t, int > p) {
                                     return p.first == i;
                                 } ) != already_chosen_bits.end();
        };

        for (std::size_t i = 0; i < MAX_ENCODING_LENGTH; i++) {
            if ( index_is_already_chosen(i) ) {
                continue;
            }

            for (auto &ctx: to_split) {
                auto it = ctx->get_input_type_at_index( i);
                indice_values[i].insert( it, ctx);
            }
        }
        return indice_values;
    }

OptionalBitArray< 64 >
convert_bytes_to_uints64(const std::vector< OptionalBitArray< 8>> &bytes,
                         std::size_t offset) {
    OptionalBitArray< 64 > val;

    for (std::size_t i = 0; i < 64; i++) {
        auto bit_offset = offset*8 + i;
        if(bit_offset >= MAX_ENCODING_LENGTH){
            val[i] = InputType::ignore;
        }
        else{
            val[ i ] = bytes[ offset + (i / 8) ][ i % 8 ];
        }
    }
    return val;
}

auto DecoderPrinter::get_decode_context_function_args(const ExtractedCtx& ectx) -> decode_func_args{
    auto input_checks = ectx.convert_circIR_to_input_type_array();
    std::vector<decode_context_function_arg> args;
    for(std::size_t i =0; i < inner_func_args.size(); i++){
        auto arg1 =  convert_bytes_to_uints64(input_checks, i*8);
        args.emplace_back(arg1, inner_func_args[i]);
    }

    return args;
}

void DecoderPrinter::print_include(const std::string &name) {
    os << "#include <" << name << ">" << std::endl;
}

void DecoderPrinter::print_file_headers() {
    print_include("array");
    print_include("stdint.h");
    os << std::endl;
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

uint64_t negate(uint64_t value) {
    return value == 0 ? 1 : 0;
}

InputType ctoit(const char c){
    switch (c) {
        case '0': return InputType::zero;
        case '1': return InputType::one;
        case '~': return InputType::ignore;
        default: circ::unreachable() << "Converting invalid value to InputType";
    }
}

}  // namespace circ::disassm