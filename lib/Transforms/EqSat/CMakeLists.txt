# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( Transforms/EqSat CIRCUITOUS_EQSAT_HEADERS
  Graph.hpp
)

add_circuitous_library( eqsat
  SOURCES
    Graph.cpp
  LINK_LIBS
    circuitous::adt
  HEADERS
    ${CIRCUITOUS_EQSAT_HEADERS}
)
