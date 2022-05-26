#include <circuitous/Disassembler/DisassemblerPrinter.h>
#include <circuitous/Printers.h>

#include <circuitous/IR/Shapes.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <cstdlib>
namespace circ::disassm {

const char dont_care_bit =
    '1';  // value that indicates a don't care inside the input_byte
const char dont_care_bit_neg = '0';  // negated value of don't care

namespace CodeGen {
std::string default_index = std::string(4, ' ');
std::string castToUint8(std::string expr) {
  return "(uint8_t) " + expr + "";
}
std::string xorInputWithNegatedTarget(std::string variable,
                                      std::string targetVal, uint pad_msb,
                                      uint pad_lsb) {
  return variable + " ^~(0b" + std::string(pad_msb, dont_care_bit_neg) +
         targetVal + std::string(pad_lsb, dont_care_bit_neg) + ")";
}

std::string prepareInputByte(std::string variable, std::string targetVal,
                             uint pad_msb, uint pad_lsb) {
  return castToUint8(
      xorInputWithNegatedTarget(variable, targetVal, pad_msb, pad_lsb));
}
//  std::string transformInput
std::string getTarget(uint pad_msb, uint pad_lsb) {
  return "0b" + std::string(pad_msb, dont_care_bit_neg) +
         std::string(8 - pad_msb - pad_lsb, dont_care_bit) +
         std::string(pad_lsb, dont_care_bit_neg);
}

std::string compare(std::string lhs, std::string rhs) {
  return "(" + lhs + " == " + rhs + ")";
}

std::string assignStatement(std::string lhs, std::string rhs) {
  return default_index + lhs + " = " + rhs + ";";
}

std::string declareStatement(std::string typeName, std::string variableName,
                             std::string expr) {
  return default_index + typeName + " " + variableName + " = " + expr + ";";
}

std::string concatExprs(std::vector<std::string> exprs, std::string delimiter){
  std::stringstream s;
  for (ulong i = 0; i < exprs.size(); i++) {
    s << exprs[i];
    if (i != exprs.size() - 1) {
      s << delimiter;
    }
  }
  return s.str();
}


std::string andExpressions(std::vector<std::string> exprs) {
  return concatExprs(exprs, " && ");
}


std::string orExpressions(std::vector<std::string> exprs) {
  return concatExprs(exprs, " || ");
}

// assumes rhs_bits is a string of consecutive bits with 0b in the string
std::string bitwiseOR(std::string lhs, std::string rhs_bits) {
  return lhs + " | (0b" + rhs_bits + ")";
}

std::string returnStatement(std::string expr) {
  return default_index + "return " + expr + ";";
}

std::string callFunction(std::string funcName, std::string parameters){
  return funcName + "(" + parameters + ")";
}
};  // namespace CodeGen

std::string DisassemblerPrinter::array_index(uint index) {
  return "[" + std::to_string(index) + "]";
}

std::string DisassemblerPrinter::reserve_name(std::string preferred_name,
                                              bool prefix) {
  std::string candidate = preferred_name;
  if (prefix) {
    candidate = circuitous_decoder_name_prefix + preferred_name;
  }

  if (std::ranges::none_of(this->used_generated_names, [&](std::string s) {
        return s == preferred_name;
      })) {
    this->used_generated_names.push_back(candidate);
    return candidate;
  } else {
    /*
     * We do not care about the quality of randomness, and ideally we would
     * change this to some word bases system instead of numbers
     */
    auto new_candidate = candidate + std::to_string(rand() % 10);
    return reserve_name(new_candidate, false);
  }
}

std::string DisassemblerPrinter::swap_endian(std::string input) {
  return std::string(input.rbegin(), input.rend());
}

void DisassemblerPrinter::printInputCheck(InputCheck check,
                                          std::string name_output_var,
                                          std::string name_fuc_input) {
  // for check all relevant bytes for check
  // relevant bytes are from indices floor(x)/8 to floor(y)/8
  auto startByte = check.low / 8;  //integer div <==> floor(x)/8
  auto endByte = (check.high - 1) / 8;  //exclusive so need to subtract one

  /*
   * we will need the check.bits in little endian otherwise we reverse the _entire_ constant
   * but, we will need to swap the endianness on byte level, so keep this like now
   */
  auto littleEndianBits = std::string(check.bits.begin(), check.bits.end());
  os << "\t // Generating for const: " << littleEndianBits
     << " start: " << std::to_string(check.low)
     << " end: " << std::to_string(check.high) << std::endl;

  // local variable inside a function that we control, so this won't give conflicts
  auto input_name = name_output_var + "_copy";
  os << CodeGen::declareStatement(
            "auto", input_name, "std::vector<uint8_t>(" + name_fuc_input + ")")
     << std::endl;

  uint padding_len_lsb = (check.low % 8);
  uint padding_len_msb = (8 - (check.high % 8)) % 8;

  // flip the bits that are allowed to be anything to a special constant that we do not check for at the end
  flip_lsb_to_dont_care(padding_len_lsb, input_name + array_index(startByte));
  flip_msb_to_dont_care(padding_len_msb, input_name + array_index(endByte));

  // if start==end ==> only 1 byte to check
  if (startByte == endByte) {
    auto consideredInputByte = input_name + array_index(startByte);
    auto preparedInput = CodeGen::prepareInputByte(
        consideredInputByte, swap_endian(littleEndianBits), padding_len_msb,
        padding_len_lsb);
    auto compExpression = CodeGen::compare(
        preparedInput, CodeGen::getTarget(padding_len_msb, padding_len_lsb));
    os << CodeGen::declareStatement("bool", name_output_var, compExpression);
  } else {  // check spans multiple bytes
    std::vector<std::string> exprs;
    for (auto currByte = startByte; currByte <= endByte; currByte++) {
      auto considerdByte = input_name + array_index(currByte);
      auto bytesDeepIntoExtract = currByte - startByte - 1;
      auto startOffset = 8 - padding_len_lsb + (bytesDeepIntoExtract * 8);

      std::string target = "";
      std::string preparedInput = "";
      if (currByte == startByte) {
        auto checkBytes =
            swap_endian(littleEndianBits.substr(0, 8 - padding_len_lsb));
        preparedInput = CodeGen::prepareInputByte(considerdByte, checkBytes, 0, padding_len_lsb);
        target = CodeGen::getTarget(0, padding_len_lsb);
      } else if (currByte != startByte && currByte != endByte) {
        auto checkBytes =
            swap_endian(littleEndianBits.substr(startOffset, startOffset + 8));
        preparedInput =
            CodeGen::prepareInputByte(considerdByte, checkBytes, 0, 0);
        target = CodeGen::getTarget(0, 0);
      } else {  //currByte == endByte
        auto checkBytes = swap_endian(
            littleEndianBits.substr(startOffset, 8 - padding_len_msb));
        preparedInput = CodeGen::prepareInputByte(considerdByte, checkBytes, padding_len_msb, 0);
        target = CodeGen::getTarget(padding_len_msb, 0);
      }

      auto compExpression = CodeGen::compare(preparedInput, target);
      exprs.push_back(compExpression);
    }

    os << CodeGen::declareStatement("bool", name_output_var,
                                    CodeGen::andExpressions(exprs));
  }
}
void DisassemblerPrinter::flip_msb_to_dont_care(uint len_msb_to_flip,
                                                std::string variable_name) {
  if (len_msb_to_flip != 0) {
    auto bitTarget = std::string(len_msb_to_flip, dont_care_bit) +
                     std::string(8 - len_msb_to_flip, dont_care_bit_neg);
    os << CodeGen::assignStatement(variable_name,
                                   CodeGen::bitwiseOR(variable_name, bitTarget))
       << std::endl;
  }
}
void DisassemblerPrinter::flip_lsb_to_dont_care(uint len_lsb_to_flip,
                                                const std::string variable) {
  if (len_lsb_to_flip != 0) {
    auto bitTarget = std::string(8 - len_lsb_to_flip, dont_care_bit_neg) +
                     std::string(len_lsb_to_flip, dont_care_bit);
    os << CodeGen::assignStatement(variable,
                                   CodeGen::bitwiseOR(variable, bitTarget))
       << std::endl;
  }
}


void DisassemblerPrinter::print_decoder_func(ExtractedVI evi) {
  auto vi = evi.VI;
  std::cout << "// Generating function for VI: " << vi->id()
            << " name: " << evi.generated_name << std::endl;
  auto inputName = function_parameter_name;
  os << "bool " << evi.generated_name << "(std::vector<uint8_t> " << inputName
     << ") {" << std::endl;

  std::vector<InputCheck> checks;
  SubtreeCollector<DecodeCondition> dc_collector;
  dc_collector.Run(vi->operands);
  auto decNodes = dc_collector.collected;


  std::vector<std::string> boolCheckNames;

  auto x =
      std::find_if(decNodes.begin(), decNodes.end(), [&](DecodeCondition *dc) {
        auto rhs = dynamic_cast<circ::Extract *>(dc->operands[1]);
        return rhs->high_bit_exc == 120;
      });
  if (x == decNodes.end()) {
    throw std::invalid_argument(
        "Invalid decoder structure, missing node indicating end");
  } else {
    auto rhs = dynamic_cast<circ::Extract *>((*x)->operands[1]);
    os << "\tif(" << inputName << ".size() != " << ceil(rhs->low_bit_inc / 8.0)
       << ") { " << std::endl
       << "\t\treturn false; " << std::endl
       << "\t}" << std::endl;
  }

  for (auto &decOp : decNodes) {
    auto lhs = dynamic_cast<circ::Constant *>(decOp->operands[0]);
    auto rhs = dynamic_cast<circ::Extract *>(decOp->operands[1]);

    if (lhs == NULL || rhs == NULL) {
      throw std::invalid_argument(
          "Decoder Condition does not consist of const//extract");
    }
    if (rhs->high_bit_exc != 120) {
      auto ret_val_name = reserve_name(std::to_string(decOp->id()));
      boolCheckNames.push_back(ret_val_name);
      this->printInputCheck(
          InputCheck(lhs->bits, rhs->low_bit_inc, rhs->high_bit_exc),
          ret_val_name, inputName);
      os << std::endl;
      os << std::endl;
    }
  }

  os << CodeGen::returnStatement(CodeGen::andExpressions(boolCheckNames))
     << std::endl;
  os << "}" << std::endl;
  return;
}


void DisassemblerPrinter::print_file() {
  SubtreeCollector<VerifyInstruction> sc;
  auto VIs = sc.Run(circuit.get()->operands[0]).collected;
  for (auto &vi : VIs) {
    extractedVIs.push_back(ExtractedVI(
        vi, "generated_decoder_prefix_" + std::to_string(vi->id())));
  }

  os << "#pragma once" << std::endl << std::endl;
  for (auto &evi : extractedVIs) {
    print_decoder_func(evi);
    os << std::endl;
  }

  print_circuit_decoder();
}

void DisassemblerPrinter::print_circuit_decoder() {
  os << "bool " << circuit_decode_function_name << "(std::vector<uint8_t> "
     << function_parameter_name << ") {" << std::endl;

  std::vector<std::string> funcCalls;
  for(auto& evi : extractedVIs){
    funcCalls.push_back(CodeGen::callFunction(evi.generated_name, function_parameter_name));
  }

  os << CodeGen::returnStatement(CodeGen::orExpressions(funcCalls)) << std::endl;
  os << "}" << std::endl;
}

}  // namespace circ::disassm
