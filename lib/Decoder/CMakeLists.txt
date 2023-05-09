# Copyright (c) 2022 Trail of Bits, Inc.

add_headers(Decoder CIRCUITOUS_DISASSEMBLER_HEADERS
    SEGMultiGraph.hpp
    DecoderPrinter.hpp
    DecodeAST.hpp
    GenerationHelpers.hpp
    SEGGraph.hpp
)

add_circuitous_library( decoder
SOURCES
        DecoderPrinter.cpp
        DecodeAST.cpp
        GenerationHelpers.cpp
        SEGMultiGraph.cpp
HEADERS
    ${CIRCUITOUS_DISASSEMBLER_HEADERS}
)
