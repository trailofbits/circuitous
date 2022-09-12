# Copyright (c) 2022 Trail of Bits, Inc.

add_subdirectory( EqSat )

add_headers( Transforms CIRCUITOUS_TRANSFORMS_HEADERS
  EqualitySaturation.hpp
  MergeAdvices.hpp
  PassBase.hpp
  Passes.hpp
  Pattern.hpp
)

add_circuitous_library( transforms
  SOURCES
    EqualitySaturation.cpp
    MergeAdvices.cpp
    OverflowFlagFix.cpp
  LINK_LIBS
    circuitous::eqsat
  HEADERS
    ${CIRCUITOUS_TRANSFORMS_HEADERS}
)
