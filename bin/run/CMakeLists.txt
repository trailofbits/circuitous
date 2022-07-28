# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( Run CIRCUITOUS_RUN_HEADERS
  Base.hpp
  Base.tpp
  Derive.tpp
  Inspect.hpp
  Interpreter.hpp
  Spawn.hpp
  Trace.hpp
)


add_circuitous_executable( run
  SOURCES
    Run.cpp
  LINK_LIBS
    gflags
    circuitous::ir
    circuitous::printers
    circuitous::diff
    circuitous::run
  HEADERS
    ${CIRCUITOUS_RUN_HEADERS}
)
