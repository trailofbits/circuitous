#include <circuitous/Decoder/DecoderPrinter.hpp>
#include <circuitous/Printers.hpp>

#include <circuitous/IR/Shapes.hpp>
#include <circuitous/IR/Visitors.hpp>
#include <cstdlib>
namespace circ::decoder {

const char ignore_magic_const =
    '1';  // value that indicates a don't care inside the input_byte
const char ignore_magic_const_neg = '0';  // negated value of don't care

namespace CodeGen {
std::string default_index = std::string(4, ' ');


std::string cast_to_int16_t(const std::string &expr) {
  return "(int16_t) " + expr;
}

std::string wrap_in_quotes(const std::string& s){
    return "(" + s + ")";
}

std::string xor_variable_with_negated_target(const std::string &variable,
                                             const std::string &targetVal, uint pad_msb,
                                             uint pad_lsb) {
  return wrap_in_quotes( variable + " ^~(0b" + std::string(8,'1') + std::string( pad_msb, ignore_magic_const_neg) +
                         targetVal + std::string( pad_lsb, ignore_magic_const_neg) + ")");
}

std::string prepareInputByte(const std::string &variable, const std::string &targetVal,
                             uint pad_msb, uint pad_lsb) {
  return cast_to_int16_t(
          xor_variable_with_negated_target( variable, targetVal, pad_msb, pad_lsb ));
}
//  std::string transformInput
std::string getTarget(uint pad_msb, uint pad_lsb) {
  return "0b" + std::string(8,'0') + std::string( pad_msb, ignore_magic_const_neg) +
         std::string(8 - pad_msb - pad_lsb, ignore_magic_const) +
         std::string( pad_lsb, ignore_magic_const_neg);
}

std::string compare(std::string lhs, std::string rhs) {
  return wrap_in_quotes(lhs + " == " + rhs);
}

std::string assignStatement(const std::string &lhs, const std::string &rhs) {
  return default_index + lhs + " = " + rhs + ";";
}

std::string declareStatement(const std::string &typeName, const std::string &variableName,
                             const std::string &expr) {
  return default_index + typeName + " " + variableName + " = " + expr + ";";
}

std::string concatExprs(const std::vector< std::string > &exprs, const std::string &delimiter){
  std::stringstream s;
  for (std::size_t i = 0; i < exprs.size(); i++) {
    s << exprs[i];
    if (i != exprs.size() - 1) {
      s << delimiter;
    }
  }
  return s.str();
}


std::string andExpressions(const std::vector< std::string > &exprs) {
  return concatExprs(exprs, " && ");
}


std::string orExpressions(const std::vector< std::string > &exprs) {
  return concatExprs(exprs, " || ");
}

// assumes rhs_bits is a string of consecutive bits with 0b in the string
std::string bitwiseOR(const std::string &lhs, const std::string &rhs_bits) {
  return lhs + " | " + wrap_in_quotes("0b" + std::string(8,'0') +rhs_bits);
}

std::string returnStatement(const std::string &expr) {
  return default_index + "return " + expr + ";";
}


std::string ifStatement(const std::string& condition_string, const std::string& body){
    return "if (" + condition_string + ") {\n" + body + "\n}\n";
}


std::string forLoop(const std::string& condition_string, const std::string& body){
    return "for (" + condition_string + ") {\n" + body + "\n}\n";
}

std::string callFunction(const std::string &funcName, const std::string &parameters){
  return funcName + "(" + parameters + ")";
}
};  // namespace CodeGen

std::string DecoderPrinter::array_index(const uint index) {
  return "[" + std::to_string(index) + "]";
}


std::string DecoderPrinter::swap_endian(const std::string &input) {
  return std::string(input.rbegin(), input.rend());
}

void DecoderPrinter::print_decoder_condition(InputCheck check,
                                                  const std::string &name_output_var,
                                                  const std::string &name_fuc_input) {
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
            "auto", input_name, "std::array<int16_t,15>(" + name_fuc_input + ")")
       << std::endl;

    uint8_t padding_len_lsb = (check.low % 8);
    uint8_t padding_len_msb = (8 - (check.high % 8)) % 8;

    print_padding(startByte, endByte, input_name, padding_len_lsb, padding_len_msb);

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
                preparedInput = CodeGen::prepareInputByte(considerdByte, checkBytes, 0,
                                                          padding_len_lsb);
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
                preparedInput = CodeGen::prepareInputByte(considerdByte, checkBytes,
                                                          padding_len_msb, 0);
                target = CodeGen::getTarget(padding_len_msb, 0);
            }

            auto compExpression = CodeGen::compare(preparedInput, target);
            exprs.push_back(compExpression);
        }

        os << CodeGen::declareStatement("bool", name_output_var,
                                        CodeGen::andExpressions(exprs));
    }
}

void DecoderPrinter::print_padding(const uint startByte, const uint endByte,
                                   const std::string& input_name,
                                   const uint8_t padding_len_lsb, const uint8_t padding_len_msb) {
    if(startByte == endByte){
        ignore_bits( PaddingBits( padding_len_lsb, padding_len_msb ),
                     input_name + array_index( startByte ));
    }
    else {
        ignore_bits( PaddingBits( padding_len_lsb, 0 ), input_name + array_index( startByte ));
        ignore_bits( PaddingBits( 0, padding_len_msb ), input_name + array_index( endByte ));
    }
}

void DecoderPrinter::ignore_bits(const PaddingBits& padding, const std::string& variable_name) {
    if(padding.msb == 0 && padding.lsb == 0){
        return;
    }

    auto bitTarget = std::string( padding.msb, ignore_magic_const)
            + std::string(8 - padding.lsb - padding.msb, ignore_magic_const_neg)
            + std::string( padding.lsb, ignore_magic_const);

    os << CodeGen::assignStatement(variable_name,
                                   CodeGen::bitwiseOR(variable_name, bitTarget))
                                   << std::endl;
}


void DecoderPrinter::print_decoder_func(const ExtractedVI &evi) {
  auto vi = evi.VI;
  std::cout << "// Generating function for VI: " << vi->id()
            << " name: " << evi.generated_name << std::endl;
  auto inputName = function_parameter_name;
  os << "bool " << evi.generated_name << "(std::array<int16_t,15> " << inputName
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
    auto size = ceil(rhs->low_bit_inc / 8.0);
    auto check_if_invalid_val = CodeGen::ifStatement(inputName + "[i] < 0", CodeGen::returnStatement("false"));
    os << CodeGen::forLoop("auto i =0; i < " + std::to_string(size) + ";i++", check_if_invalid_val);
  }

  for (auto &decOp : decNodes) {
    auto lhs = dynamic_cast<circ::Constant *>(decOp->operands[0]);
    auto rhs = dynamic_cast<circ::Extract *>(decOp->operands[1]);

    if (lhs == NULL || rhs == NULL) {
      throw std::invalid_argument(
          "Decoder Condition does not consist of const//extract");
    }
    if (rhs->high_bit_exc != 120) {
      auto ret_val_name = "dc_" + std::to_string(decOp->id());
      boolCheckNames.push_back(ret_val_name);
        this->print_decoder_condition(
                InputCheck( lhs->bits, rhs->low_bit_inc, rhs->high_bit_exc ),
                ret_val_name, inputName );
      os << std::endl;
      os << std::endl;
    }
  }

  os << CodeGen::returnStatement(CodeGen::andExpressions(boolCheckNames))
     << std::endl;
  os << "}" << std::endl;
  return;
}


void DecoderPrinter::print_file() {
  SubtreeCollector<VerifyInstruction> sc;
  auto VIs = sc.Run(circuit.get()->operands[0]).collected;
  for (auto &vi : VIs) {
    extractedVIs.push_back(ExtractedVI(
        vi, "generated_decoder_prefix_" + std::to_string(vi->id())));
  }

    os << "#include <array>" << std::endl << std::endl;
    os << "#include <stdint.h>" << std::endl << std::endl;

  for (auto &evi : extractedVIs) {
    print_decoder_func(evi);
    os << std::endl;
  }

  print_circuit_decoder();
}

void DecoderPrinter::print_circuit_decoder() {
  os << "bool " << circuit_decode_function_name << "(std::array<int16_t,15> "
     << function_parameter_name << ") {" << std::endl;

  std::vector<std::string> funcCalls;
  for(auto& evi : extractedVIs){
    funcCalls.push_back(CodeGen::callFunction(evi.generated_name, function_parameter_name));
  }

  os << CodeGen::returnStatement(CodeGen::orExpressions(funcCalls)) << std::endl;
  os << "}" << std::endl;
}

}  // namespace circ::disassm
