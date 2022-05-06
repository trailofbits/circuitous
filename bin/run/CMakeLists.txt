# Copyright (c) 2022 Trail of Bits, Inc.

add_circuitous_executable( run
  SOURCES
    Run.cpp
  LINK_LIBS
    gflags
    circuitous::ir
    circuitous::printers
)
