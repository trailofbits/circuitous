# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( Fuzz CIRCUITOUS_FUZZ_HEADERS
  DiffResult.hpp
  Husks.hpp
  InstNavigation.hpp
  InstructionFuzzer.hpp
  Permute.hpp
)

add_circuitous_header_library( fuzz
  HEADERS
    ${CIRCUITOUS_FUZZ_HEADERS}
)
