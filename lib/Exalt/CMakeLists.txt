# Copyright (c) 2023 Trail of Bits, Inc.

add_headers( Exalt CIRCUITOUS_EXALT_HEADERS
    Common.hpp
    Components.hpp
    CSE.hpp
    Interfaces.hpp
    ISemLifters.hpp
    Lifter.hpp
    States.hpp
    Syscall.hpp
    Value.hpp
    UnitComponents.hpp
)

add_circuitous_library( exalt
  SOURCES
    Components.cpp
    ISemLifters.cpp
    Lifter.cpp
    States.cpp
    UnitComponents.cpp
    Value.cpp
  LINK_LIBS
    circuitous::dbg
  HEADERS
      ${CIRCUITOUS_EXALT_HEADERS}
)
