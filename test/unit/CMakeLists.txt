#
# Copyright (c) 2021 Trail of Bits, Inc.
#

find_package( doctest CONFIG REQUIRED )
find_package( fmt CONFIG REQUIRED )
find_package( spdlog REQUIRED )

add_subdirectory(lib)

add_executable( test-circuitous
  main.cpp

  Transforms/EqualitySaturation.cpp
)

target_link_libraries( test-circuitous
  PRIVATE
    doctest::doctest
    spdlog::spdlog
    fmt::fmt

    circuitous::settings
    circuitous::ir
    circuitous::testing
)

add_test(
  NAME test-circuitous
  COMMAND "$<TARGET_FILE:test-circuitous>"
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
)

add_executable( test-eqsat
  main.cpp
  eqsat/pattern.cpp
  eqsat/graph.cpp
  eqsat/match.cpp
  eqsat/saturation.cpp
)

target_link_libraries( test-eqsat
  PRIVATE
    doctest::doctest
    spdlog::spdlog
    fmt::fmt

    circuitous::settings
    circuitous::eqsat
    circuitous::testing
)

add_test(
  NAME test-eqsat
  COMMAND "$<TARGET_FILE:test-eqsat>"
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
)

add_executable( test-trace-conversion
  main.cpp
  trace-conversion/basic.cpp
)

target_link_libraries( test-trace-conversion
  PRIVATE
    doctest::doctest
    spdlog::spdlog
    fmt::fmt

    circuitous::settings
    circuitous::eqsat
    circuitous::testing
    circuitous::run
    circuitous::lifter
)

add_test(
  NAME test-trace-conversion
  COMMAND "$<TARGET_FILE:test-trace-conversion>"
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
)

file( COPY trace-conversion/inputs DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/trace-conversion" )
