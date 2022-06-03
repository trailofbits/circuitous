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

  struct ExtractedVI{
    ExtractedVI(VerifyInstruction* VI, std::string name): VI(VI), generated_name(std::move(name)){}
    VerifyInstruction* VI;
    std::string generated_name;
  };

  struct PaddingBits{
      PaddingBits(uint8_t lsb, uint8_t msb) : lsb(lsb), msb(msb){};
      uint8_t lsb;
      uint8_t msb;
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
    static constexpr const auto function_parameter_name = "input";
    static constexpr const auto circuit_decode_function_name = "circuit_decode";

    const circ::CircuitPtr & circuit;
    std::ostream& os;
    std::vector<ExtractedVI> extractedVIs;

    void print_decoder_func(const ExtractedVI &evi);
    void print_circuit_decoder();

    void print_decoder_condition(InputCheck check, const std::string &name_output_var,
                                 const std::string &name_fuc_input);
    std::string array_index(uint index);

    std::string swap_endian(const std::string &input);

    void flip_bits_to_dont_care(const PaddingBits& padding, const std::string& variable_name);

    void print_padding(uint startByte, uint endByte,
                         const std::string &input_name,
                         uint8_t padding_len_lsb, uint8_t padding_len_msb);
  };
}




