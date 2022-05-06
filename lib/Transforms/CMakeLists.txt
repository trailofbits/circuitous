# Copyright (c) 2022 Trail of Bits, Inc.

add_subdirectory( EqSat )

add_circuitous_library( transforms
  SOURCES
    EqualitySaturation.cpp
    MergeAdvices.cpp
  LINK_LIBS
    circuitous::eqsat
)
