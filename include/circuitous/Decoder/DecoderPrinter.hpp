#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/Transforms/PassBase.hpp>
#include <ostream>
#include <vector>
#include <string>
#include "DecodeAST.hpp"

namespace circ::decoder {
    enum class InputType : uint32_t {
        zero = 0, one = 1, ignore = 2
    };
    uint64_t to_val(const InputType &ty);
    uint64_t to_val_negated(const InputType &ty);

    struct ExtractedVI {
        ExtractedVI(VerifyInstruction *VI, std::string name, uint8_t size,
                    std::unordered_multiset< DecodeCondition * > decodeConditions) :
                VI( VI ),
                generated_name( std::move( name )),
                encoding_size_in_bytes( size ),
                decodeConditions( decodeConditions ) {};

        VerifyInstruction *VI;
        std::string generated_name;
        uint8_t encoding_size_in_bytes;
        std::unordered_multiset< DecodeCondition * > decodeConditions;
    };

    struct DTI {
        std::vector< ExtractedVI * > ones;
        std::vector< ExtractedVI * > zeros;
        std::vector< ExtractedVI * > ignores;
    };


    struct decode_context_function_arg {
        decode_context_function_arg(std::array< InputType, 64 > &byte,
                                    const Var &var) : byte( byte ), var( var ) {};

        const std::array< InputType, 64 > &byte;
        const Var &var;
    };

    class DecoderPrinter {

        /*
         * Eventually needs two different files, one for header and one for body?
         */
    public:
        DecoderPrinter(const circ::CircuitPtr &circ) : circuit( circ ), os( std::cout ) {}
        DecoderPrinter(const circ::CircuitPtr &circ, std::ostream &os) : circuit( circ ),
                                                                         os( os ) {}

        void print_file(); // TODO remove os from constructor?


    private:
        static constexpr const auto bytes_input_variable = "input";
        static constexpr const auto circuit_decode_function_name = "circuit_decode";
        static const Var innerFuncArg1;
        static const Var innerFuncArg2;

        using funcArgsPair =  std::pair< decode_context_function_arg, decode_context_function_arg >;


        const circ::CircuitPtr &circuit;
        std::ostream &os;
        std::vector< ExtractedVI > extractedVIs;

        Expr print_decoder_func(const ExtractedVI &evi);
        Expr print_circuit_decoder();
        static Expr get_decode_context_function_body(const decode_context_function_arg &arg1,
                                              const decode_context_function_arg &arg2);
        funcArgsPair get_decode_context_function_args(const ExtractedVI &evi);


        Expr print_split(std::vector< ExtractedVI * > to_split,
                         std::vector< std::pair< std::size_t, int>> already_chosen_bits,
                         int depth);

        Expr print_conversion_input_to_uints64();
        Expr evi_call(const ExtractedVI &evi);

        static std::vector< std::array< InputType, 8>>
        convert_circIR_to_input_type_array(const ExtractedVI &evi) ;
        static void convert_array_input_to_uint64(const Var &array_input, const Var &arg,
                                           StatementBlock &b, bool second_uint) ;


        // Debug tree
        int max_depth = 0;
        static bool contains_ignore_bit(const std::array< InputType, 64 > &byte) ;
        static bool contains_only_ignore_bit(const std::array< InputType, 64 > &byte) ;
        static Expr print_ignore_bits(const decode_context_function_arg &arg);
        static Expr get_comparison(const decode_context_function_arg &arg) ;


        void extract_evi();
    };
}




