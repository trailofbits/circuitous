# Copyright (c) 2022 Trail of Bits, Inc.

add_circuitous_executable( lift
  SOURCES
    Lift.cpp
  LINK_LIBS
    gflags
    circuitous::lifter
    circuitous::ir
    circuitous::printers
    circuitous::transforms
    circuitous::diff
)
