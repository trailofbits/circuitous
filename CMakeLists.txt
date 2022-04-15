# Copyright (c) 2020-2021 Trail of Bits, Inc.
cmake_minimum_required(VERSION 3.14)

include(cmake/vcpkg_helper.cmake)
find_package(remill COMPONENTS VCPKG_DEPS QUIET)

project(circuitous
  LANGUAGES C CXX ASM
  VERSION 0.1.0
  DESCRIPTION "Binary to circuit lifter tool"
  HOMEPAGE_URL "https://github.com/trailofbits/circuitous.git"
)

include(cmake/prevent_in_source_builds.cmake)

include(GNUInstallDirs)

include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/settings.cmake")
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/utils.cmake")

FindAndSelectClangCompiler()

#
# libraries
#
find_package(remill CONFIG REQUIRED)
find_package(gflags CONFIG REQUIRED)

#
# target settings
#

add_library(circuitous_settings INTERFACE)

include(cmake/compiler_warnings.cmake)
set_project_warnings(circuitous_settings)

# sanitizer options if supported by compiler
include(cmake/sanitizers.cmake)
enable_sanitizers(circuitous_settings)

# allow for static analysis options
include(cmake/static_analyzers.cmake)

# test options
option(ENABLE_TESTING "Enable Test Builds" ON)

# gap library
find_package(gap REQUIRED)

target_link_libraries(circuitous_settings INTERFACE
  remill_settings
  remill
  gflags
  gap::gap
)

target_include_directories(circuitous_settings INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)

#
# library targets
#

add_library(circuitous_ir STATIC
  lib/IR/IR.cpp
  lib/IR/SMT.cpp
  lib/IR/Verify.cpp
  lib/IR/Serialize.cpp
)

target_link_libraries(circuitous_ir PUBLIC circuitous_settings)


add_library(circuitous_lifter STATIC
  lib/Lifter/CircuitBuilder.cpp
  lib/Lifter/Remill.cpp
  lib/Lifter/BaseLifter.cpp
  lib/Lifter/Decoder.cpp
  lib/Lifter/CircuitSmithy.cpp
  lib/Lifter/Instruction.cpp
)

target_link_libraries(circuitous_lifter PUBLIC circuitous_settings)

add_library(circuitous_printers STATIC
  lib/Printers/DOT.cpp
  lib/Printers/Python.cpp
  lib/Printers/JSON.cpp
  lib/Printers/Topology.cpp
  lib/Printers/Printers.cpp
  lib/Printers/SMT.cpp
)

target_link_libraries(circuitous_printers PUBLIC circuitous_settings)

add_library(circuitous_eqsat STATIC
  lib/Transforms/EqSat/Graph.cpp
)

target_link_libraries(circuitous_eqsat PUBLIC circuitous_settings)

add_library(circuitous_transforms STATIC
  lib/Transforms/MergeAdvices.cpp
  lib/Transforms/EqualitySaturation.cpp
)

target_link_libraries(circuitous_transforms PUBLIC
  circuitous_eqsat
  circuitous_settings
)

#
# executable targets
#

add_executable(circuitous-seed
  bin/seed/Seed.cpp
)

target_link_libraries(circuitous-seed PUBLIC circuitous_settings)

add_executable(circuitous-lift
  bin/lift/Lift.cpp
)

target_link_libraries(circuitous-lift PUBLIC
  gflags
  circuitous_lifter
  circuitous_ir
  circuitous_printers
  circuitous_transforms
)

add_executable(circuitous-run
  bin/run/Run.cpp
)

target_link_libraries(circuitous-run PUBLIC
  gflags
  circuitous_ir
  circuitous_printers
)

#
# tests
#

if (ENABLE_TESTING)
  enable_testing()
  add_subdirectory(test)
endif()

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
