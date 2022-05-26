# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( Lifter/Components CIRCUITOUS_COMPONENTS_HEADERS
  Decoder.hpp
)

add_circuitous_library( components
  SOURCES
    Decoder.cpp
  LINK_LIBS
    circuitous::dbg
  HEADERS
    ${CIRCUITOUS_COMPONENTS_HEADERS}
)
