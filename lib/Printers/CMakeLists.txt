# Copyright (c) 2022 Trail of Bits, Inc.

find_package(Z3 CONFIG REQUIRED 4.8.16)

add_circuitous_library( printers
  SOURCES
    DOT.cpp
    JSON.cpp
    Printers.cpp
    Python.cpp
    SMT.cpp
    Topology.cpp
  LINK_LIBS
    z3::libz3
)
