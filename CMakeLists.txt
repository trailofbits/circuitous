#
# Copyright (c) 2021-present, Trail of Bits, Inc.
# All rights reserved.
#
# This source code is licensed in accordance with the terms specified in
# the LICENSE file found in the root directory of this source tree.
#

# Find remill first because its config has useful dependency-finding info that
# needs to be found before the CMake `project` declaration
find_package(remill COMPONENTS VCPKG_DEPS QUIET)
include(cmake/vcpkg_helper.cmake)

project(circuitous LANGUAGES C CXX ASM)
cmake_minimum_required(VERSION 3.14)

include("cmake/options.cmake")
include("cmake/settings.cmake")
include("cmake/utils.cmake")

include(GNUInstallDirs)

FindAndSelectClangCompiler()

find_program(ccache_path ccache)
if("${ccache_path}" STREQUAL "ccache_path-NOTFOUND")
  message(STATUS "anvill: ccache was not found")

else()
  message(STATUS "anvill: Enabling ccache support with ${ccache_path}")

  set(CMAKE_CXX_COMPILER_LAUNCHER "${ccache_path}" CACHE FILEPATH "")
  set(CMAKE_C_COMPILER_LAUNCHER "${ccache_path}" CACHE FILEPATH "")
endif()

#
# libraries
#
find_package(remill CONFIG REQUIRED)
find_package(gflags CONFIG REQUIRED)

#
# target settings
#

add_library(circuitous_settings INTERFACE)
target_link_libraries(circuitous_settings INTERFACE
  remill_settings
  remill
  gflags
)

target_include_directories(circuitous_settings INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)

target_compile_options(circuitous_settings INTERFACE
  -Werror
  -Wconversion
  -pedantic 	
  -Wno-unreachable-code-return
)

#
# library targets
#

add_library(circuitous_util STATIC
  lib/Util/UseDef.cpp
)

target_include_directories(circuitous_util PUBLIC circuitous_settings)
target_link_libraries(circuitous_util PUBLIC circuitous_settings)

add_library(circuitous_ir STATIC
  lib/IR/Hash.cpp
  lib/IR/IR.cpp
  lib/IR/Serialize.cpp
)

target_include_directories(circuitous_ir PUBLIC circuitous_settings)
target_link_libraries(circuitous_ir PUBLIC circuitous_settings)


add_library(circuitous_lifter STATIC
  lib/Lifter/CircuitBuilder.cpp
  lib/Lifter/Remill.cpp
)

target_include_directories(circuitous_lifter PUBLIC circuitous_settings)
target_link_libraries(circuitous_lifter PUBLIC circuitous_settings)

add_library(circuitous_printers STATIC 
  lib/Printers/DOT.cpp
  lib/Printers/Python.cpp
  lib/Printers/JSON.cpp
  lib/Printers/Topology.cpp
  lib/Printers/SMT.cpp
)

target_include_directories(circuitous_printers PUBLIC circuitous_settings)
target_link_libraries(circuitous_printers PUBLIC circuitous_settings)

add_library(circuitous_transforms STATIC
  lib/Transforms/ConvertPopCountToParity.cpp
  lib/Transforms/ExtractCommonTopologies.cpp
  lib/Transforms/MergeHints.cpp
  lib/Transforms/StrengthReducePopulationCount.cpp
)

target_include_directories(circuitous_transforms PUBLIC circuitous_settings)
target_link_libraries(circuitous_transforms PUBLIC circuitous_settings)

#
# executable targets
#

add_executable(circuitous-lift
  bin/lift/Main.cpp
)

target_link_libraries(circuitous-lift PUBLIC
  gflags
  circuitous_lifter
  circuitous_ir
  circuitous_printers
  circuitous_util
  circuitous_transforms
)

target_include_directories(circuitous-lift PUBLIC circuitous_settings)

add_executable(circuitous-run
  bin/run/Interpreter.cpp
  bin/run/Main.cpp
)

target_link_libraries(circuitous-run PUBLIC
  gflags
  circuitous_ir
  circuitous_util
  circuitous_printers
)

target_include_directories(circuitous-run PUBLIC circuitous_settings)

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