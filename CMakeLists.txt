# Copyright (c) 2020 Trail of Bits, Inc.

project(circuitous)
cmake_minimum_required(VERSION 3.2)

include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/settings.cmake")
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/utils.cmake")

FindAndSelectClangCompiler()

#
# libraries
#

# remill
if(NOT TARGET remill)
  if("${PLATFORM_NAME}" STREQUAL "windows")
    set(REMILL_FINDPACKAGE_HINTS HINTS "${CMAKE_INSTALL_PREFIX}/remill/lib")
  endif()

  find_package(remill REQUIRED ${REMILL_FINDPACKAGE_HINTS})
endif()

# z3
set(LLVM_Z3_INSTALL_DIR "${Z3_INSTALL_PREFIX}")
find_package(Z3 4.7.1 REQUIRED MODULE)

#
# tool target settings
#

add_library(circuitous-settings INTERFACE)

target_compile_definitions(circuitous-settings INTERFACE
    ${PROJECT_DEFINITIONS}
)

target_compile_options(circuitous-settings INTERFACE
    ${GLOBAL_CXXFLAGS}
    -Werror -Wconversion -pedantic 
    -Wno-unreachable-code-return
)

target_include_directories(circuitous-settings INTERFACE
   "${CMAKE_CURRENT_SOURCE_DIR}/include"
   ${Z3_INCLUDE_DIR}
)

target_link_libraries(circuitous-settings INTERFACE
    remill
    ${Z3_LIBRARIES}
)

add_library(circuitous-util STATIC
    include/circuitous/Util/UseDef.h
    include/circuitous/Util/BitManipulation.h
    lib/Util/UseDef.cpp
)

target_link_libraries(circuitous-util PUBLIC
    circuitous-settings
)

add_library(circuitous-ir STATIC
    include/circuitous/IR/IR.h
    include/circuitous/IR/Hash.h
    
    lib/IR/Hash.cpp
    lib/IR/IR.cpp
    lib/IR/Serialize.cpp
)

target_link_libraries(circuitous-ir PUBLIC
    circuitous-util
    circuitous-settings
)

add_library(circuitous-lifter STATIC    
    lib/Lifter/CircuitBuilder.h
    lib/Lifter/CircuitBuilder.cpp
    lib/Lifter/Remill.cpp
)

target_link_libraries(circuitous-lifter PUBLIC
    circuitous-ir
    circuitous-settings
)

add_library(circuitous-printers STATIC    
    include/circuitous/Printers.h
    
    lib/Printers/DOT.cpp
    lib/Printers/Python.cpp
    lib/Printers/JSON.cpp
    lib/Printers/Topology.cpp
    lib/Printers/SMT.cpp
)

target_link_libraries(circuitous-printers PUBLIC
    circuitous-ir
    circuitous-settings
)

add_library(circuitous-transforms STATIC    
    include/circuitous/Transforms.h
    
    lib/Transforms/StrengthReducePopulationCount.cpp
    lib/Transforms/ConvertPopCountToParity.cpp
)

target_link_libraries(circuitous-transforms PUBLIC
    circuitous-ir
    circuitous-util
    circuitous-settings
)

add_executable(circuitous
    Main.cpp
)

target_link_libraries(circuitous PUBLIC
    circuitous-lifter
    circuitous-ir
    circuitous-printers
    circuitous-util
    circuitous-settings
    circuitous-transforms
)

#
# install settings
#

if(DEFINED WIN32)
  set(install_folder "${CMAKE_INSTALL_PREFIX}/circuitous")
else()
  set(install_folder "${CMAKE_INSTALL_PREFIX}")
endif()

install(
    TARGETS circuitous
    RUNTIME DESTINATION "${install_folder}/bin")
