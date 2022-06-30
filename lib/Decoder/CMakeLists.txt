# Copyright (c) 2022 Trail of Bits, Inc.

add_headers(Decoder CIRCUITOUS_DISASSEMBLER_HEADERS
    DecoderPrinter.hpp
    DecodeAST.hpp
)

add_circuitous_library( decoder
SOURCES
        DecoderPrinter.cpp
        DecodeAST.cpp
HEADERS
    ${CIRCUITOUS_DISASSEMBLER_HEADERS}
)
