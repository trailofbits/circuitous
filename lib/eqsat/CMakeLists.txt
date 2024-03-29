# Copyright (c) 2022 Trail of Bits, Inc.

find_package( spdlog REQUIRED )
find_package( fmt CONFIG REQUIRED )

add_headers( ../eqsat CIRCUITOUS_EQSAT_HEADERS
  core/common.hpp
  core/cost_graph.hpp
  core/egraph.hpp

  algo/apply.hpp
  algo/ematch.hpp
  algo/print.hpp
  algo/saturation.hpp
  algo/substitution.hpp
  algo/synthesis.hpp

  pattern/parser.hpp
  pattern/pattern.hpp
  pattern/rewrite_rule.hpp
  pattern/rule_set.hpp
)

add_circuitous_library( eqsat
  SOURCES
    parser.cpp
    pattern.cpp
    saturation.cpp
  LINK_LIBS
    gap::gap
    fmt::fmt
    spdlog::spdlog
  HEADERS
    ${CIRCUITOUS_EQSAT_HEADERS}
)
