#include <iostream>
#include <vector>
#include <stdint.h>
#include <bitset>
#include <array>
#include <fstream>
#include <sstream>
#include <algorithm>

extern int circuit_decode(std::array<uint8_t, 15> input);

std::array<uint8_t, 15> get_input_bytes(const std::string &line) {
    std::stringstream ss(line);
    std::array<uint8_t, 15> out;
    std::fill(out.begin(), out.end(), 0);

    // fill 15 bytes or we are eof
    for (auto i = 0; !ss.eof() && i < 15; i++) {
        uint16_t val;
        ss >> std::hex >> val;
        out[i] = val;
    }
    return out;
}

void print_error(bool before_marker, std::array<uint8_t, 15> &input_bytes) {
    std::cerr << "error:: should ";
    std::cerr << (before_marker ? "accept" : "fail");
    std::cerr << " but did not: ";
    for (auto &b: input_bytes) {
        std::cerr << std::hex << b;
    }
    std::cerr << std::endl;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        std::cerr << "invalid arguments, requires a text file as input" << std::endl;
        return 1;
    }
    auto filename = argv[1];
    std::ifstream myfile(filename);
    auto should_reject = true;
    auto success = true;

    if (!myfile.is_open())
        throw std::invalid_argument("can't open file");

    for (std::string line; std::getline(myfile, line);) {
        if (line[0] == 'A') {
            should_reject = false;
            continue;
        }
        else if (line[0] == 'R'){
            should_reject = true;
            continue;
        }
        else{
            throw std::invalid_argument("incorrect input format, expected: [A|R] <encoding>");
        }

        auto input_bytes = get_input_bytes(line.substr(1));

        if ((circuit_decode(input_bytes) > 0) != should_reject) {
            success = false;
            print_error( should_reject, input_bytes);
        }
    }

    std::cout << "success: " << success << std::endl;
    return success;
}