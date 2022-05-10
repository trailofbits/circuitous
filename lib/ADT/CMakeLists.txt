# Copyright (c) 2022 Trail of Bits, Inc.

add_headers( ADT CIRCUITOUS_ADT_HEADERS
  EGraph.hpp
  UnionFind.hpp
)

add_circuitous_header_library( adt
  HEADERS
    ${CIRCUITOUS_ADT_HEADERS}
)
