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

std::string include(const std::string &expr) {
    return "#include <" + expr + ">\n";
}

std::string cast_to_int16_t(const std::string &expr) {
  return "(uint64_t) " + expr;
}

std::string wrap_in_quotes(const std::string& s){
    return "(" + s + ")";
}

std::string xor_variable_with_negated_target(const std::string &variable,
                                             const std::string &targetVal, uint pad_msb,
                                             uint pad_lsb) {
  return wrap_in_quotes( variable + " ^~(0b" + std::string( pad_msb, ignore_magic_const_neg) +
                         targetVal + std::string( pad_lsb, ignore_magic_const_neg) + ")");
}

std::string prepareInputByte(const std::string &variable, const std::string &targetVal,
                             uint pad_msb, uint pad_lsb) {
  return cast_to_int16_t(
          xor_variable_with_negated_target( variable, targetVal, pad_msb, pad_lsb ));
}
//  std::string transformInput
std::string getTarget(uint pad_msb, uint pad_lsb) {
  return "0b" +  std::string( pad_msb, ignore_magic_const_neg) +
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

std::string sumExpressions(const std::vector< std::string > &exprs) {
    return concatExprs(exprs, " + ");
}

// assumes rhs_bits is a string of consecutive bits with 0b in the string
std::string bitwiseOR(const std::string &lhs, const std::string &rhs_bits) {
  return lhs + " | " + wrap_in_quotes("0b" + rhs_bits);
}

std::string returnStatement(const std::string &expr) {
  return default_index + "return " + expr + ";";
}

std::string mul(const std::string &lhs, const std::string &rhs ) {
    return CodeGen::wrap_in_quotes(lhs + " * " + rhs);
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

const std::string DecoderPrinter::uint64_input1 = "input1";
const std::string DecoderPrinter::uint64_input2 = "input2";

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
//    auto inputName = bytes_input_variable;
    os << "static int " << evi.generated_name << "(uint64_t " << uint64_input1 << ", uint64_t " << uint64_input2 << ") {" << std::endl;

    // TODO size init?
    std::vector< std::array< InputType, 8 >> input_checks;
//    std::vector< std::array< InputType, 8 >> input_checks( evi.encoding_size_in_bytes );
    for (std::size_t i = 0; i < 16; i++) {
        auto a = std::array< InputType, 8 >{InputType::ignore, InputType::ignore,
                                            InputType::ignore, InputType::ignore,
                                            InputType::ignore, InputType::ignore,
                                            InputType::ignore, InputType::ignore};
        for (auto &n: evi.decodeConditions) {
            auto lhsn = dynamic_cast<circ::Constant *>(n->operands[ 0 ]);
            auto extract = dynamic_cast<circ::Extract *>(n->operands[ 1 ]);
            auto low = extract->low_bit_inc;
            auto high_inc = extract->high_bit_exc - 1;

            // out of range of considered byte
            if ( low > (i + 1) * 8 || high_inc < i * 8 || high_inc == 119 ) {
                continue;
            }

            for (std::size_t c = 0; c < 8; c++) {
                auto bit_index = (i * 8) + c;
                if ( low <= bit_index && bit_index <= high_inc ) {
                    auto new_val = [&]() {
                        if ( lhsn->bits[ bit_index - low ] == '1' )
                            return InputType::one;
                        else return InputType::zero;
                    }();
                    a[ c ] = new_val;
                }
            }
        }
        input_checks.push_back( a );
    }
    std::array< InputType, 64 > val;
    for(std::size_t i = 0; i < 64; i++){
        val[i] = input_checks[i/8][i % 8];
    }
    auto arg1 = innerFunctionArguments(val, uint64_input1) ;
    std::array< InputType, 64 > val2;
    for(std::size_t i = 0; i < 64; i++){
        val2[i] = input_checks[8+i/8][i % 8];
    }
    auto lval2 = std::string(uint64_input2);
    auto arg2 = innerFunctionArguments(val2, uint64_input2);


    new_print( arg1, arg2, evi.encoding_size_in_bytes );
    os << std::endl << std::endl;
    os << "}" << std::endl;
}


void DecoderPrinter::print_file() {
  SubtreeCollector<VerifyInstruction> sc;
  auto VIs = sc.Run(circuit.get()->operands[0]).collected;
  for (auto &vi : VIs) {
      SubtreeCollector<DecodeCondition> dc_collector;
      dc_collector.Run(vi->operands);
      std::unordered_multiset<DecodeCondition *> decNodes = dc_collector.collected;
      auto x =
              std::find_if(decNodes.begin(), decNodes.end(), [&](DecodeCondition *dc) {
                  auto rhs = dynamic_cast<circ::Extract *>(dc->operands[1]);
                  return rhs->high_bit_exc == 120;
              });
      if(x == decNodes.end()){
          throw std::invalid_argument("No decode condition that specifies end");
      }
      auto rhs = dynamic_cast<circ::Extract *>((*x)->operands[1]);
      auto encoding_length = floor(rhs->low_bit_inc /8);
      if(encoding_length >15){
          throw std::invalid_argument("Instruction is longer than 15 bytes");
      }
      extractedVIs.push_back(ExtractedVI(
        vi, "generated_decoder_prefix_" + std::to_string(vi->id()), static_cast<uint8_t>(encoding_length),
        std::move(decNodes)));
  }

    os << CodeGen::include("array") << std::endl;
    os << CodeGen::include("stdint.h") << std::endl << std::endl;

    for (auto &evi : extractedVIs) {
        print_decoder_func( evi );
    }
    print_circuit_decoder();
    os << std::endl;
}

void DecoderPrinter::print_circuit_decoder() {
    os << "int " << circuit_decode_function_name << "(std::array<uint8_t,15> "
       << bytes_input_variable << ", int " << max_length_variable_name << " = 15 ) {"
       << std::endl;

    print_conversion_input_to_uints64();

    std::vector< std::string > funcCalls;
    for (auto &evi: extractedVIs) {
        funcCalls.push_back( CodeGen::callFunction( evi.generated_name,uint64_input1 + ", " + uint64_input2));
    }

//    std::unordered_multimap<int, ExtractedVI*> group_by_enc_size;
//    for (auto &evi: extractedVIs) {
//        group_by_enc_size.insert( std::make_pair(evi.encoding_size_in_bytes, &evi));
//    }
//
//    for(int i = 1; i <= 15; i++){
//        std::vector< std::string > funcCalls;
//        if(group_by_enc_size.contains(i)){
//            auto range = group_by_enc_size.equal_range(i);
//            for (auto it = range.first; it != range.second; ++it) {
//                funcCalls.push_back( CodeGen::callFunction( (*it).second->generated_name,uint64_input1 + ", " + uint64_input2));
//            }
//
//            os << CodeGen::ifStatement(CodeGen::orExpressions( funcCalls ), CodeGen::returnStatement(std::to_string(i)));
//        }
//    }


    os << CodeGen::returnStatement(CodeGen::sumExpressions( funcCalls)) << std::endl;

    os << "}" << std::endl;
}

    void DecoderPrinter::print_conversion_input_to_uints64() {
        std::stringstream retrieveStuff;
        for(int i =0; i < 8; i++){
            retrieveStuff << "((uint64_t)" << bytes_input_variable << "[" << i << "] << (8*" << i << "))";
            if (i != 7){
                retrieveStuff << " + ";
            }
        }
        os << CodeGen::declareStatement( "uint64_t", uint64_input1, retrieveStuff.str());

        std::stringstream retrieveStuff2;
        for(int i =0; i < 8; i++){
            retrieveStuff2 << "((uint64_t)" << bytes_input_variable << "[8+" << i << "] << (8*" << i << "))";
            if (i != 7){
                retrieveStuff2 << " + ";
            }
        }
        os << CodeGen::declareStatement( "uint64_t", uint64_input2, retrieveStuff2.str());
    }

// TODO what endian do I expect as input?
// it takes an array of bytes, so byte[0] lsb?
// and bit[0][7] msb?
// and byte[1][0] lsb?
// TODO if we only need to check bytes > 2, don't check byte 1
void DecoderPrinter::print_decoder_condition(
    const std::vector< std::array< InputType, 8 >> &input8,
    const std::string &name_fuc_input) {
    std::vector< std::array< InputType, 64 >> input;
    if(input8.size() != 16){
        throw std::invalid_argument("expected 8*i8");
    }

    std::array< InputType, 64 > val;
    for(std::size_t i = 0; i < 64; i++){
        val[i] = input8[i/8][i % 8];
    }
    input.push_back(val);

    for(std::size_t i = 0; i < 64; i++){
        val[i] = input8[8+i/8][i % 8];
    }
    input.push_back(val);

    std::vector<std::string> exprs;
    for(std::size_t byte_index = 0; byte_index < input.size(); byte_index++) {
        //TODO if we only have ignored bits, then we don't need to emit a check and hence not pad it
        if(contains_ignore_bit(input[byte_index]) && !contains_only_ignore_bit(input[byte_index])) {
            auto current_byte = name_fuc_input + array_index( static_cast<uint>(byte_index));
            std::stringstream ss;

            for(auto i = input[byte_index].rbegin(); i != input[byte_index].rend(); i++){
                if(*i == InputType::ignore)
                    ss << to_str(InputType::ignore);
                else
                    ss << to_str_negated(InputType::ignore);
            }
            os << CodeGen::assignStatement(current_byte, CodeGen::bitwiseOR(current_byte, ss.str()));
        }
    }
    for(std::size_t byte_index = 0; byte_index < input.size(); byte_index++) {
        if( contains_only_ignore_bit(input[byte_index]))
            continue;

        auto current_byte = name_fuc_input + array_index( static_cast<uint>(byte_index));
        auto byte = input[byte_index];
        std::stringstream ss;
        for(auto i = byte.rbegin(); i != byte.rend(); i++) {
            if(*i == InputType::ignore)
                ss << to_str_negated(InputType::ignore);
            else
                ss << to_str(*i);
        }

        auto s = ss.str();
        auto lhs = CodeGen::cast_to_int16_t(CodeGen::xor_variable_with_negated_target(current_byte, s,0,0));

        std::stringstream output;
        for(auto i = byte.rbegin(); i != byte.rend(); i++) {
            if ( *i == InputType::ignore )
                output << to_str_negated(InputType::ignore);
            else
                output << to_str(InputType::ignore);
        }
        auto rhs = "0b" + output.str();
        exprs.push_back(CodeGen::compare(lhs, rhs));
    }
    os << CodeGen::returnStatement(CodeGen::andExpressions(exprs));
}

bool DecoderPrinter::contains_ignore_bit(const std::array< InputType, 64 > &byte) const {
    for(auto& b: byte){
        if(b == InputType::ignore){
            return true;
        }
    }
    return false;
}

    bool
    DecoderPrinter::contains_only_ignore_bit(const std::array< InputType, 64 > &byte) const {
        for(auto& b: byte){
            if(b != InputType::ignore){
                return false;
            }
        }
        return true;
    }

    void DecoderPrinter::new_print(const innerFunctionArguments &arg1,
                                   const innerFunctionArguments &arg2, int encoding_length) {
        print_ignore_bits( arg1 );
        print_ignore_bits( arg2 );

        std::vector<std::string> compare_exprs;
        if( !contains_only_ignore_bit(arg1.byte))
            compare_exprs.push_back( get_comparison(arg1));

        if( !contains_only_ignore_bit(arg2.byte))
            compare_exprs.push_back( get_comparison(arg2));

        os <<std::endl;
        auto cmps = CodeGen::wrap_in_quotes(CodeGen::andExpressions(compare_exprs));
        os << CodeGen::returnStatement(CodeGen::mul(cmps, std::to_string(encoding_length)));
    }

    std::string DecoderPrinter::get_comparison(
            const innerFunctionArguments &arg) const {
        std::stringstream ss;
        for(auto i = arg.byte.rbegin(); i != arg.byte.rend(); i++) {
            if(*i == InputType::ignore)
                ss << to_str_negated(InputType::ignore);
            else
                ss << to_str(*i);
        }

        auto s = ss.str();
        auto lhs = CodeGen::cast_to_int16_t(CodeGen::xor_variable_with_negated_target(arg.input_name, s,0,0));

        std::stringstream output;
        for(auto i = arg.byte.rbegin(); i != arg.byte.rend(); i++) {
            if ( *i == InputType::ignore )
                output << to_str_negated(InputType::ignore);
            else
                output << to_str(InputType::ignore);
        }
        auto rhs = "0b" + output.str();
        auto cmp = CodeGen::compare(lhs, rhs);
        return cmp;
    }

    void DecoderPrinter::print_ignore_bits(const innerFunctionArguments &arg) {
        if( contains_ignore_bit( arg.byte) && !contains_only_ignore_bit( arg.byte)) {
            auto current_byte = arg.input_name;
            std::stringstream ss;

            for(auto i = arg.byte.rbegin(); i != arg.byte.rend(); i++){
                if(*i == InputType::ignore)
                    ss << to_str(InputType::ignore);
                else
                    ss << to_str_negated(InputType::ignore);
            }
            os << CodeGen::assignStatement( current_byte, CodeGen::bitwiseOR( current_byte, ss.str()));
        }
    }


    std::string to_str(const InputType& ty){
    switch (ty) {
        case InputType::zero: return "0";
        case InputType::one: return "1";
        case InputType::ignore: return "1";
    }
}

std::string to_str_negated(const InputType& ty){
    switch (ty) {
        case InputType::zero: return "1";
        case InputType::one: return "0";
        case InputType::ignore: return "0";
    }
}

}  // namespace circ::disassm