# Copyright (c) 2022 Trail of Bits, Inc.

find_package( spdlog REQUIRED )

add_headers( ../eqsat CIRCUITOUS_EQSAT_HEADERS
  core/common.hpp
  core/cost_graph.hpp
  core/egraph.hpp

  algo/ematch.hpp
  algo/saturation_graph.hpp
  algo/substitution.hpp

  pattern/parser.hpp
  pattern/rewrite_rule.hpp
  pattern/rule_set.hpp
)

add_circuitous_library( eqsat
  SOURCES
    parser.cpp
  LINK_LIBS
    gap::gap
    spdlog::spdlog
  HEADERS
    ${CIRCUITOUS_EQSAT_HEADERS}
)
