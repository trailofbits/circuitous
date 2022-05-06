#
# Copyright (c) 2021 Trail of Bits, Inc.
#

find_package(doctest CONFIG REQUIRED)

add_subdirectory(lib)

add_executable( test-circuitous
  main.cpp

  ADT/UnionFind.cpp
  ADT/EGraph.cpp

  Transforms/Pattern.cpp
  Transforms/EqualitySaturation.cpp
)

target_link_libraries( test-circuitous
  PRIVATE
    circuitous::settings
    doctest::doctest
    circuitous::testing
)

add_test(
  NAME test-circuitous
  COMMAND "$<TARGET_FILE:test-circuitous>"
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
)
