# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( Run CIRCUITOUS_RUN_HEADERS
    Base.hpp
    Base.tpp
    Derive.tpp

    Execute.hpp
    Inspect.hpp
    Interpreter.hpp
    Queue.hpp
    Result.hpp
    Spawn.hpp
    State.hpp
    Trace.hpp
)

add_circuitous_library( run
  SOURCES
    Interpreter.cpp
    Queue.cpp
    State.cpp
    Trace.cpp
  LINK_LIBS
    circuitous::ir
    circuitous::lifter
  HEADERS
    ${CIRCUITOUS_RUN_HEADERS}
)
