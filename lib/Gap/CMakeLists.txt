# Copyright (c) 2022 Trail of Bits, Inc.

function(add_headers_gap lib var)
  set(headers ${${var}})
  foreach (header ${ARGN})
    set(headers ${headers} ${PROJECT_SOURCE_DIR}/include/gap/${lib}/${header})
  endforeach()
  set(${var} ${headers} PARENT_SCOPE)
endfunction()

add_headers_gap( Support CIRCUITOUS_GAP_HEADERS
  Check.hpp
  Log.hpp
)

add_circuitous_header_library( gap
  HEADERS
    ${CIRCUITOUS_GAP_HEADERS}
  LINK_LIBS
    gap::gap
)
