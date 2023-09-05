# Copyright (c) 2023 Trail of Bits, Inc.

add_headers( Exalt CIRCUITOUS_EXALT_HEADERS
    Common.hpp
    Components.hpp
    Lifter.hpp
    States.hpp
    Syscall.hpp
    UnitComponents.hpp
)

add_circuitous_library( exalt
  SOURCES
    Components.cpp
    Lifter.cpp
    States.cpp
  LINK_LIBS
    circuitous::dbg
  HEADERS
      ${CIRCUITOUS_EXALT_HEADERS}
)
