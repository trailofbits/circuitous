# Copyright (c) 2020 Trail of Bits, Inc.
find_package(remill COMPONENTS VCPKG_DEPS QUIET)

include(cmake/vcpkg_helper.cmake)

project(circuitous)
cmake_minimum_required(VERSION 3.14)
include(GNUInstallDirs)
enable_language(C CXX ASM)

include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/settings.cmake")
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/utils.cmake")

FindAndSelectClangCompiler()

#
# libraries
#
find_package(remill CONFIG REQUIRED)

#
# target settings
#

set(CIRCUITOUS circuitous-${REMILL_LLVM_VERSION})

add_library(circuitous-util STATIC
  lib/Util/UseDef.cpp
)

target_link_libraries(circuitous-util PUBLIC remill_settings remill)
target_include_directories(circuitous-util PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)

add_library(circuitous-ir STATIC
  lib/IR/Hash.cpp
  lib/IR/IR.cpp
  lib/IR/Serialize.cpp
)

target_link_libraries(circuitous-ir PUBLIC remill_settings remill)
target_include_directories(circuitous-ir PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)


add_library(circuitous-lifter STATIC
  lib/Lifter/CircuitBuilder.cpp
  lib/Lifter/Remill.cpp
)

target_link_libraries(circuitous-lifter PUBLIC remill_settings remill)
target_include_directories(circuitous-lifter PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)

add_library(circuitous-printers STATIC 
  lib/Printers/DOT.cpp
  lib/Printers/Python.cpp
  lib/Printers/JSON.cpp
  lib/Printers/Topology.cpp
  lib/Printers/SMT.cpp
)

target_link_libraries(circuitous-printers PUBLIC remill_settings remill)
target_include_directories(circuitous-printers PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)

add_library(circuitous-transforms STATIC
  lib/Transforms/ConvertPopCountToParity.cpp
  lib/Transforms/ExtractCommonTopologies.cpp
  lib/Transforms/MergeHints.cpp
  lib/Transforms/StrengthReducePopulationCount.cpp
)

target_link_libraries(circuitous-transforms PUBLIC remill_settings remill)
target_include_directories(circuitous-transforms PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)

add_executable(circuitous-lift
  bin/lift/Main.cpp
)

add_executable(circuitous-run
  bin/run/Main.cpp
)

target_link_libraries(circuitous-lift PUBLIC
  circuitous-lifter
  circuitous-ir
  circuitous-printers
  circuitous-util
  circuitous-settings
  circuitous-transforms
)

# target_link_libraries(circuitous-run PUBLIC
#     circuitous-ir
# )

#
# install settings
#

if(DEFINED WIN32)
  set(install_folder "${CMAKE_INSTALL_PREFIX}/circuitous")
else()
  set(install_folder "${CMAKE_INSTALL_PREFIX}")
endif()

install(
  TARGETS circuitous-lift circuitous-run
  RUNTIME DESTINATION "${install_folder}/bin"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/circuitousConfig.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/circuitousConfig.cmake"
  @ONLY
)