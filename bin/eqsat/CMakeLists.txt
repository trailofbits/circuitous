# Copyright (c) 2022 Trail of Bits, Inc.

add_circuitous_executable( eqsat-vis
  SOURCES
    eqsat-vis.cpp
  LINK_LIBS
    circuitous::eqsat
)
