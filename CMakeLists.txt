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
find_package(gflags CONFIG REQUIRED)

#
# target settings
#

# set(CIRCUITOUS circuitous_${REMILL_LLVM_VERSION})

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
  bin/lift/Lift.cpp
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
  bin/run/Run.cpp
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