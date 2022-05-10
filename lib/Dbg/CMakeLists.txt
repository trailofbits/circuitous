# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( Dbg CIRCUITOUS_DBG_HEADERS
  CtxPrint.hpp
)

add_circuitous_header_library( dbg
  HEADERS
    ${CIRCUITOUS_DBG_HEADERS}
)
