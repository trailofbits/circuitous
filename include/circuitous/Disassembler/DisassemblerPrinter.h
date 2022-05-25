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

  class DisassemblerPrinter{
   public: DisassemblerPrinter(const circ::CircuitPtr & circ) : circuit(circ), os(std::cout){}
    void print_file(); // TODO remove os from constructor?, no this whole class should be just functions


    //TODO Create functions which exports names of generated functions

   private:
    const std::string CIRCUITOUS_DECODER_NAME_PREFIX = "circ__";

    const circ::CircuitPtr & circuit;
    std::ostream& os;
    std::vector<VerifyInstruction*> extractedVIs;

    void printVerifyInstructions();
    void printCanCircuitDecode();
    void print_give_context_id_which_can_decode();
    void print_can_context_decode(int context_id);

    std::string reserve_name(std::string preferred_name, bool prefix = true);
    std::vector<std::string> used_generated_names;
    void printInputCheck(InputCheck check, std::string name_output_var,
                         std::string name_fuc_input);
    std::string array_index(uint index);

    std::string swap_endian(std::string input);

  };
}




