# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( Transforms CIRCUITOUS_TRANSFORMS_HEADERS
  EGraph.hpp
  EqualitySaturation.hpp
  PassBase.hpp
  Passes.hpp
)

add_circuitous_library( transforms
  SOURCES
    EGraph.cpp
    EGraphBuilder.cpp
    EqualitySaturation.cpp
    OverflowFlagFix.cpp
    ConjureALU.cpp
  LINK_LIBS
    circuitous::eqsat
  HEADERS
    ${CIRCUITOUS_TRANSFORMS_HEADERS}
)
