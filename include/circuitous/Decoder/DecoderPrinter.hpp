#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/Transforms/PassBase.hpp>
#include <ostream>
#include <vector>
#include <string>
#include "DecodeAST.hpp"
template <int L> concept SupportedEncodingSize =  L <= 64;

namespace circ::decoder {

    enum class InputType : uint32_t {
        zero = 0, one = 1, ignore = 2
    };

    uint64_t to_val(const InputType &ty);
    uint64_t negate(uint64_t value);
    InputType ctoit(const char c); //char to input type

    /*
     * Data structure representing a group of bits that can also be an ignore/don't care bit
     */
    template<int L> requires SupportedEncodingSize<L>
    struct OptionalBitArray : std::array< InputType, L >{
        uint64_t ignored_bits_to_uint64() const;
        uint64_t to_uint64() const;

        bool contains_ignore_bit() const;
        bool contains_only_ignore_bits() const;

    private:
        uint64_t convert_to_uint64(auto if_ignore, auto if_not_ignore) const;
    };

    struct ExtractedDecodeCondition {
        ExtractedDecodeCondition(uint32_t low_incl, uint32_t high_excl,
                                 const std::string& bits) : low_bit_inc(
                low_incl ), high_bit_exc( high_excl ), bits( bits ) {};

        uint32_t low_bit_inc;
        uint32_t high_bit_exc;
        const std::string bits;
    };

    struct ExtractedCtx {
        ExtractedCtx(const std::string &name, uint8_t size,
                     const std::vector<ExtractedDecodeCondition> &decodeConditions) :
                generated_name( name ),
                encoding_size_in_bytes( size ),
                decodeConditions( decodeConditions ) {};

        std::string generated_name;
        uint8_t encoding_size_in_bytes;
        std::vector<ExtractedDecodeCondition> decodeConditions; // Extract info we need from this

        std::vector< OptionalBitArray<8> > convert_circIR_to_input_type_array() const;
        InputType get_input_type_at_index(std::size_t i) const;
    };


    /*
     * Represents at a single bit offset which ExtractedCtx require the bit to be what value.
     *
     * So if we have 4, 2bit instructions:
     * index    01
     * -----------
     * enc_1    00
     * enc_2    01
     * enc_3    11
     * enc_4    1x -- x is an ignore/don't care bit
     * Then this data structure can represent the second index by
     * ones -> enc_1, enc_3
     * zeros -> enc_1,
     * ignores -> enc_4
     */
    struct Decode_Requires_Group {
        std::vector< ExtractedCtx * > ones;
        std::vector< ExtractedCtx * > zeros;
        std::vector< ExtractedCtx * > ignores;

        void insert(InputType type, ExtractedCtx * ctx){
            if ( type == InputType::zero )
                zeros.push_back( ctx );
            else if ( type == InputType::one )
                ones.push_back( ctx );
            else
                ignores.push_back( ctx );
        }
    };

    struct decode_context_function_arg {
        decode_context_function_arg(const OptionalBitArray<64> &byte,
                                    const Var &var) : byte( byte ), var( var ) {};

        const OptionalBitArray<64> byte;
        const Var &var;
    };

    // Currently only x86 is supported, 120 bits = 15 bytes
    const static std::size_t MAX_ENCODING_LENGTH = 120; // first = 1

    /*
     * This class is the primary printer for the generated instruction decoder.
     * It first extracts out all relevant aspects of a circuit into internal datastructures
     * that contain only the bare minimum of the information to this aspect lightly decoupled of the rest.
     *
     * Afterwards it builds up an Expr type that will be printed through by a simple catamorphism (visitor pattern)
     *
     */
    class DecoderPrinter {
    public:
        DecoderPrinter(const circ::CircuitPtr &circ) : circuit( circ ), os( std::cout ) {
            extract_ctx();
        }
        DecoderPrinter(const circ::CircuitPtr &circ, std::ostream &os) : circuit( circ ),
                                                                         os( os ) {
            extract_ctx();
        }

        void print_file();
    private:
        static constexpr const auto bytes_input_variable = "input";
        static constexpr const auto circuit_decode_function_name = "circuit_decode";
        inline static const Var inner_func_arg1 = Var( "first8bytes", "uint64_t");
        inline static const Var inner_func_arg2 = Var( "second8bytes", "uint64_t");
        inline static const std::array<Var,2> inner_func_args = {inner_func_arg1, inner_func_arg2};

        const circ::CircuitPtr &circuit;
        std::ostream &os;
        std::vector< ExtractedCtx > extracted_ctxs;
        int max_depth = 0; // used to measure performance of selection tree generation

        // funcs for conversion
        void extract_ctx();
        uint8_t
        get_encoding_length(const std::unordered_multiset< DecodeCondition * > &decNodes) const;


        // general printing
        void print_file_headers();
        void print_include(const std::string& name);

        // user facing decoder function printing
        Expr create_top_level_function();
        static Expr get_top_level_arg_conversion();
        static std::vector< Expr >
        top_to_inner_level_args(const Var &array_input, const Var &arg,
                                size_t offset);


        // internal functions printing
        using decode_func_args =  std::vector<decode_context_function_arg>;
        static decode_func_args get_decode_context_function_args(const ExtractedCtx& ctx);
        Expr create_context_decoder_function(const ExtractedCtx &ctx);
        static Expr get_decode_context_function_body(const decode_func_args& args, int encoding_size);
        Expr generate_decoder_selection_tree(const std::vector< ExtractedCtx * > &to_split,
                                             std::vector< std::pair< std::size_t, int>> already_chosen_bits,
                                             int depth);

        static std::array< Decode_Requires_Group, MAX_ENCODING_LENGTH >
        get_decode_requirements_per_index(const std::vector< ExtractedCtx * > &to_split,
                                          const std::vector< std::pair< std::size_t, int>> &already_chosen_bits);

        static Expr create_ignore_bits_setter(const decode_context_function_arg &arg);
        static Expr get_comparison(const decode_context_function_arg &arg);
    };
}

#include "DecoderPrinter.tpp"