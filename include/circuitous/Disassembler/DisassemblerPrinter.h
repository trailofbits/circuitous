#pragma once

#include <circuitous/IR/Circuit.hpp>
#include <circuitous/Transforms/PassBase.hpp>
#include <ostream>
#include <vector>
#include <string>

namespace circ::disassm{
  /*
   * Mirrors decoding condition which consts of comparing a CONST to an extract node
   */
  struct InputCheck{
    InputCheck(std::string bits, uint low, uint high) : bits(std::string(bits)), low(low), high(high){}
    // bits are in stored in reverse order, so _least_ significant bit is at bits[|bits|]
    std::string bits; // currently, a string to make easier conversion from CONSTANTS, and it's easy to use in the generator
    uint low; // the first bit from a potential _encoding_ that will be checked against bits[0]
    uint high; // same as low, but exclusive
  };

  struct ExtractedVI{
    ExtractedVI(VerifyInstruction* VI, std::string name): VI(VI), generated_name(name){}
    VerifyInstruction* VI;
    std::string generated_name;
  };

  class DisassemblerPrinter{
    /*
     * Eventually needs two different files, one for header and one for body?
     */
   public: DisassemblerPrinter(const circ::CircuitPtr & circ) : circuit(circ), os(std::cout){}
    void print_file(); // TODO remove os from constructor?, no this whole class should be just functions


    //TODO Create functions which exports names of generated functions
  private:

    const std::string circuitous_decoder_name_prefix = "circ__";
    const std::string function_parameter_name = "input";
    const std::string circuit_decode_function_name = "circuit_decode";

    const circ::CircuitPtr & circuit;
    std::ostream& os;
    std::vector<ExtractedVI> extractedVIs;

    void print_decoder_func(ExtractedVI evi);
    void print_circuit_decoder();

    std::string reserve_name(std::string preferred_name, bool prefix = true);
    std::vector<std::string> used_generated_names;
    void printInputCheck(InputCheck check, std::string name_output_var,
                         std::string name_fuc_input);
    std::string array_index(uint index);

    std::string swap_endian(std::string input);

    void flip_lsb_to_dont_care(uint len_lsb_to_flip,
                               const std::string variable);
    void flip_msb_to_dont_care(uint len_msb_to_flip, std::string variable_name);
  };
}




