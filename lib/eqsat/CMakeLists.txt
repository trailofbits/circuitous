# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( ../eqsat CIRCUITOUS_EQSAT_HEADERS
  common.hpp
  cost_graph.hpp
  egraph.hpp
  ematch.hpp
  parser.hpp
  rule.hpp
  rule_set.hpp
  saturation_graph.hpp
  substitution.hpp
)

add_circuitous_library( eqsat
  SOURCES
    parser.cpp
  LINK_LIBS
    gap::gap
  HEADERS
    ${CIRCUITOUS_EQSAT_HEADERS}
)
