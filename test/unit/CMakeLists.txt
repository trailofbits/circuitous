#
# Copyright (c) 2021 Trail of Bits, Inc.
#

add_subdirectory(lib)

add_executable(test_circuitous
  main.cpp

  ADT/UnionFind.cpp
  ADT/EGraph.cpp

  Transforms/Pattern.cpp
)

target_link_libraries(test_circuitous
  PRIVATE
    circuitous_settings
    thirdparty_doctest
    test_support
)

add_test(
  NAME test_circuitous
  COMMAND "$<TARGET_FILE:test_circuitous>"
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
)