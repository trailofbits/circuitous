#include <iostream>
#include <vector>
#include <stdint.h>
#include <iostream>
#include <bitset>
#include <array>
#include <fstream>
#include <sstream>
extern bool circuit_decode(std::array<int16_t,15> input);

int main(int argc, char** argv){
    if(argc != 2){
        std::cerr << "invalid arguments, requires a text file as input" << std::endl;
        return 1;
    }
    auto filename = argv[1];
    std::ifstream myfile (filename);

    auto before_marker = true;
    auto success = true;
    if (myfile.is_open())
    {
        std::string line;
        while (getline(myfile, line) ) {
            std::array<int16_t, 15> out = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
            if ( line.find("---") != std::string::npos ) {
                before_marker = false;
                continue;
            }
            std::stringstream ss( line );
            for(auto i =0; !ss.eof() && i < 15; i++){
                uint16_t val;
                ss >> std::hex >> val;
                out[i] = val;
            }

            if(circuit_decode(out) != before_marker){
                success = false;
                std::cerr << "error:: should ";
                if(before_marker){
                    std::cerr << "accept";
                }
                else{
                    std::cerr << "fail";
                }
                std::cerr << " but did not: ";
                for(auto& b: out){
                    if(b == -1)
                        break;
                    std::cerr << std::hex << b;
                }
                std::cerr << std::endl;
            }
        }
        myfile.close();
    }

    std::cout << "success: " << success << std::endl;
    return success;
}