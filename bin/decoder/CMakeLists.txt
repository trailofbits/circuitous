# Copyright (c) 2022 Trail of Bits, Inc.

add_circuitous_executable( decoder
  SOURCES
        Decoder.cpp
  LINK_LIBS
    gflags
    circuitous::lifter
    circuitous::ir
    circuitous::printers
    circuitous::transforms
    circuitous::decoder
    circuitous::seg
)

