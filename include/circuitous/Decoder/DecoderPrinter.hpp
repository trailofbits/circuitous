#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/Transforms/PassBase.hpp>
#include <ostream>
#include <vector>
#include <string>

namespace circ::decoder{
  /*
   * Mirrors decoding condition which consts of comparing a CONST to an extract node
   */
  struct InputCheck{
    InputCheck(const std::string &bits, uint low, uint high) : bits(std::string(bits)), low(low), high(high){}
    // bits are in stored in reverse order, so _least_ significant bit is at bits[|bits|]
    std::string bits; // currently, a string to make easier conversion from CONSTANTS, and it's easy to use in the generator
    uint low; // the first bit from a potential _encoding_ that will be checked against bits[0]
    uint high; // same as low, but exclusive
  };

    enum class InputType : uint32_t
    {
        zero = 0, one = 1, ignore = 2
    };

    
    std::string to_str(const InputType& ty);

    std::string to_str_negated(const InputType& ty);

    struct ExtractedVI{
        ExtractedVI(VerifyInstruction* VI, std::string name, uint8_t size, std::unordered_multiset<DecodeCondition *> decodeConditions): VI(VI), generated_name(std::move(name)), encoding_size_in_bytes(size), decodeConditions(decodeConditions){}
        VerifyInstruction* VI;
        std::string generated_name;
        uint8_t encoding_size_in_bytes;
        std::unordered_multiset<DecodeCondition *> decodeConditions;
  };

  struct PaddingBits{
      PaddingBits(uint8_t lsb, uint8_t msb) : lsb(lsb), msb(msb){};
      uint8_t lsb;
      uint8_t msb;
  };

  struct innerFunctionArguments {
      innerFunctionArguments(std::array< InputType, 64 > &byte, const std::string &input_name) :byte(byte), input_name(input_name){}
      const std::array< InputType, 64 > &byte;
      const std::string &input_name;
  };

  class DecoderPrinter{
    /*
     * Eventually needs two different files, one for header and one for body?
     */
    public: DecoderPrinter(const circ::CircuitPtr & circ) : circuit(circ), os(std::cout){}
    public: DecoderPrinter(const circ::CircuitPtr & circ, std::ostream& os) : circuit(circ), os(os){}
    void print_file(); // TODO remove os from constructor?, no this whole class should be just functions


    //TODO Create functions which exports names of generated functions
  private:
    static constexpr const auto circuitous_decoder_name_prefix = "circ__";
    static constexpr const auto max_length_variable_name = "max_length";
    static constexpr const auto bytes_input_variable = "input";
    static  const std::string uint64_input1;
    static  const std::string uint64_input2;
    static constexpr const auto circuit_decode_function_name = "circuit_decode";

    const circ::CircuitPtr & circuit;
    std::ostream& os;
    std::vector<ExtractedVI> extractedVIs;

    void print_decoder_func(const ExtractedVI &evi);
    void print_circuit_decoder();

//    void print_decoder_condition(InputCheck check, const std::string &name_output_var,
//                                 const std::string &name_fuc_input);
      void print_decoder_condition(const std::vector< std::array< InputType, 8 >> &input,
                                   const std::string &name_fuc_input);

      void new_print(const innerFunctionArguments &arg1,
                     const innerFunctionArguments &arg2, int encoding_length);
    std::string array_index(const uint index);

    std::string swap_endian(const std::string &input);

    void ignore_bits(const PaddingBits& padding, const std::string& variable_name);

    void print_padding(const uint startByte, const uint endByte,
                       const std::string &input_name,
                       const uint8_t padding_len_lsb, const uint8_t padding_len_msb);

    bool contains_ignore_bit(const std::array< InputType, 64 > &byte) const;
    bool contains_only_ignore_bit(const std::array< InputType, 64 > &byte) const;

    void print_ignore_bits(const innerFunctionArguments &arg);

    std::string get_comparison(const innerFunctionArguments &arg) const;

      void print_conversion_input_to_uints64();
  };
}




