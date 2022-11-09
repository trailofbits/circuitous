# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( Seg CIRCUITOUS_SEG_HEADERS
  SEGMultiGraph.hpp
)

add_circuitous_library( seg
  SOURCES
        SEGMultiGraph.cpp
  HEADERS
    ${CIRCUITOUS_SUPPORT_HEADERS}
)
