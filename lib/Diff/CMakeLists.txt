# Copyright (c) 2022 Trail of Bits, Inc.

add_headers(Diff CIRCUITOUS_DISASSEMBLER_HEADERS
        SemanticsTainter.hpp
)

add_circuitous_library( diff
SOURCES
        SemanticsTainter.cpp
        Diff.cpp
HEADERS
    ${CIRCUITOUS_DISASSEMBLER_HEADERS}
)
