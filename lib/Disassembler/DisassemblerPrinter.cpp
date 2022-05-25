#include <circuitous/Disassembler/DisassemblerPrinter.h>
#include <circuitous/IR/Visitors.hpp>
#include <cstdlib>

namespace circ::disassm {

std::string DisassemblerPrinter::array_index(uint index) {
  return "[" + std::to_string(index) + "]";
}

class DCNodeExtractor : public circ::UniqueVisitor<DCNodeExtractor> {
 public:
  std::vector<DecodeCondition*> DecodeConditions;
  void Visit(Operation* op){
    op->Traverse(*this);
  }

  void Visit(DecodeCondition *op) {
    DecodeConditions.push_back(op);
    op->Traverse(*this);
  }
};

std::string DisassemblerPrinter::reserve_name(std::string preferred_name, bool prefix){
  std::string candidate = preferred_name;
  if(prefix){
    candidate = CIRCUITOUS_DECODER_NAME_PREFIX + preferred_name;
  }

  if(std::ranges::none_of(this->used_generated_names, [&](std::string s){ return s == preferred_name; })){
    this->used_generated_names.push_back(candidate);
    return candidate;
  }
  else{
    /*
     * We do not care about the quality of randomness, and ideally we would
     * change this to some word bases system instead of numbers
     */
    auto new_candidate = candidate + std::to_string(rand() %10);
    return reserve_name(new_candidate, false);
  }
}

std::string DisassemblerPrinter::swap_endian(std::string input) {return std::string(input.rbegin(), input.rend());}
void DisassemblerPrinter::printInputCheck(InputCheck check,
                                          std::string name_output_var,
                                          std::string name_fuc_input) {
  // for check all relevant bytes for check
  // relevant bytes are from indices floor(x)/8 to floor(y)/8
  auto startByte = check.low / 8;  //integer div <==> floor(x)/8
  auto endByte = (check.high -1) / 8; //exclusive so need to subtract one

  /*
   * we will need the check.bits in little endian otherwise we reverse the _entire_ constant
   * but we will need to swap the endianness on byte level, so keep this like now
   */
  auto littleEndianBits = std::string(check.bits.begin(), check.bits.end());
  os << "// Generating for const: " << littleEndianBits
     << " start: " << std::to_string(check.low) << " end: " << std::to_string(check.high) << std::endl;

  auto input_name = reserve_name(name_output_var + "_", false);

  os << "auto " << input_name << " = std::vector<uint8_t>(" << name_fuc_input << ");" << std::endl;

  uint padding_len_lsb = (check.low % 8);
  uint padding_len_msb = (8- (check.high % 8)) % 8;

  // byte we currently consider in our check
  auto currentByte = input_name + array_index(startByte);
  if(padding_len_lsb != 0){
    os << currentByte << " = " << currentByte << " | (0b"
    << std::string(8 - padding_len_lsb, '0')
    << std::string(padding_len_lsb, '1')
    << ");" << std::endl;
  }

  currentByte = input_name + array_index(endByte);
  if (padding_len_msb != 0) {
    os << currentByte << " = " << currentByte << " | (0b"
    << std::string(padding_len_msb, '1')
    << std::string(8 - padding_len_msb, '0')
    << ");" << std::endl;
  }

  // if start==end ==> only 1 byte to check
  if (startByte == endByte) {
    os << "bool " << name_output_var << " = (uint8_t (" << currentByte << " ^~(0b"
    << std::string(padding_len_msb, '0')
       << swap_endian(littleEndianBits)
       << std::string(padding_len_lsb, '0')
       << "))) == 0b"
       << std::string(padding_len_msb, '0')
       << std::string(8 - padding_len_lsb - padding_len_msb, '1')
       << std::string(padding_len_lsb, '0') << ";"
       << std::endl;
  }

    if(endByte > startByte){
      os << "bool " << name_output_var << " = (";
      for(auto currByte = startByte; currByte <= endByte; currByte++){
          currentByte = input_name + array_index(currByte);
          auto bytesDeepIntoExtract = currByte-startByte-1;
          auto startOffset = 8- padding_len_lsb +(bytesDeepIntoExtract*8);

          if(currByte == startByte){
           os << "((uint8_t) (" << currentByte << " ^~(0b"
           << swap_endian(littleEndianBits.substr(0,8- padding_len_lsb))
           << std::string(padding_len_lsb, '0')
            << "))) == 0b"
               << std::string(8- padding_len_lsb, '1')
               << std::string(padding_len_lsb, '0');
           os << " && ";
          }
          if(currByte != startByte && currByte != endByte){
            os << "((uint8_t) " << currentByte << " ^~(0b"
            << swap_endian(
                      littleEndianBits.substr(startOffset, startOffset + 8))
               << ")) == 0b"
               << std::string(8, '1');
            os << " && ";
          }
          if(currByte == endByte){
            os << "((uint8_t) (" << currentByte << " ^~(0b"
            << std::string(padding_len_msb, '0')
            << swap_endian(
                      littleEndianBits.substr(startOffset,8- padding_len_msb))
            << "))) == 0b"
              << std::string(padding_len_msb, '0')
              << std::string(8- padding_len_msb, '1');
          }

      }
      os << ");" << std::endl;
      }
}


void DisassemblerPrinter::printVerifyInstructions() {
 std::vector<VerifyInstruction*> VIs;
  auto extract_VI_ops = [&](circ::Operation* op) {
    check(op);
    if (auto vi = dynamic_cast<circ::VerifyInstruction*>(op)){
      VIs.push_back(vi);
    }
  };

  circuit->ForEachOperation(extract_VI_ops);
  for (auto& vi : VIs) {
    std::cout << "Generating function for VI: " << vi->id() << std::endl;
    auto inputName = this->reserve_name("input", false);
    os << "bool context_decode_" << vi->id() << "(std::vector<uint8_t> " << inputName << ") {" << std::endl;

    std::vector<InputCheck> checks;
    DCNodeExtractor decNodes;
    std::vector<std::string> boolCheckNames;
    vi->Traverse(decNodes);

    auto x= std::find_if(decNodes.DecodeConditions.begin(), decNodes.DecodeConditions.end(), [&](DecodeCondition* dc) {
      auto rhs = dynamic_cast<circ::Extract *>(dc->operands[1]);
      return rhs->high_bit_exc == 120;
    });
    if(x == decNodes.DecodeConditions.end()){
      //throw
      std::cout << "big error" << "didn't find last element" << std::endl;
    }
    else{
      auto rhs = dynamic_cast<circ::Extract *>((*x)->operands[1]);
      os << "\tif(" <<inputName << ".size() != " << ceil(rhs->low_bit_inc/8.0) << ") { " << std::endl << "\t\treturn false; " << std::endl << "\t}" << std::endl;
    }

    for(auto& decOp : decNodes.DecodeConditions){
      auto lhs = dynamic_cast<circ::Constant *>(decOp->operands[0]);
      auto rhs = dynamic_cast<circ::Extract *>(decOp->operands[1]);

      if (lhs == NULL || rhs == NULL) {
        //TODO Throw exception here
        std::cout
        << "Decoder condition not  in the form of const == extract, error"
        << std::endl;
      } else {
        if (rhs->high_bit_exc != 120) {
          auto ret_val_name = reserve_name(std::to_string(decOp->id()));
          boolCheckNames.push_back(ret_val_name);
          this->printInputCheck(
              InputCheck(lhs->bits, rhs->low_bit_inc, rhs->high_bit_exc),
              ret_val_name, inputName);
          os << std::endl;
        }
      }
    }

    os << "return ";
    for(ulong i =0; i < boolCheckNames.size(); i++){
      os << boolCheckNames[i];
      // don't write && for last item
      if(i != boolCheckNames.size() -1) {
        os << " && ";
      }
    }
    os << ";" << std::endl;
    os << "}" << std::endl;
  }

  return;
}
void DisassemblerPrinter::printCanCircuitDecode(){

}
void DisassemblerPrinter::print_file(){
  printVerifyInstructions();
};


}  // namespace circ::disassm
