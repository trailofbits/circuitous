# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( Transforms/eqsat CIRCUITOUS_EQSAT_HEADERS
  cost_graph.hpp
  pattern.hpp
  rule.hpp
  saturation_graph.hpp
)

add_circuitous_header_library( eqsat
  LINK_LIBS
    circuitous::adt
  HEADERS
    ${CIRCUITOUS_EQSAT_HEADERS}
)
