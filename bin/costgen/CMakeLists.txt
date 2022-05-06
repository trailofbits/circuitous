# Copyright (c) 2022 Trail of Bits, Inc.

add_circuitous_executable( costgen
  SOURCES
    CostGen.cpp
  LINK_LIBS
    circuitous::ir
    circuitous::printers
)
