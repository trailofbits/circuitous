# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( Support CIRCUITOUS_SUPPORT_HEADERS
  CLIArgs.hpp
  Check.hpp
  Ciff.hpp
  Log.hpp
)

add_circuitous_header_library( support
  HEADERS
    ${CIRCUITOUS_SUPPORT_HEADERS}
  LINK_LIBS
    circuitous::gap
)
