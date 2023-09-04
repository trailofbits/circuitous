# Copyright (c) 2023 Trail of Bits, Inc.

add_headers( Exalt CIRCUITOUS_EXALT_HEADERS
    Components.hpp
    States.hpp
    Syscall.hpp
)

add_circuitous_library( exalt
  SOURCES
    Components.cpp
    States.cpp
  LINK_LIBS
    circuitous::dbg
  HEADERS
      ${CIRCUITOUS_EXALT_HEADERS}
)
