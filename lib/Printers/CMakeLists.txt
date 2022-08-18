# Copyright (c) 2022 Trail of Bits, Inc.

# find_package(Z3 CONFIG REQUIRED 4.8.16)

add_headers( Printers CIRCUITOUS_PRINTERS_HEADERS
  Verilog.hpp
  Dot.hpp
)

add_circuitous_library( printers
  SOURCES
    DOT.cpp
    JSON.cpp
    Printers.cpp
  HEADERS
    ${CIRCUITOUS_PRINTERS_HEADERS}
)
