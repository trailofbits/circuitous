
# Copyright (c) 2020 Trail of Bits, Inc.

project(circuitous)
cmake_minimum_required(VERSION 3.2)

include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/settings.cmake")
include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/utils.cmake")

FindAndSelectClangCompiler()

# warnings and compiler settings
if(NOT DEFINED WIN32)
  set(PROJECT_CXXFLAGS
    ${GLOBAL_CXXFLAGS} -Werror -Wconversion -pedantic 
    -Wno-unreachable-code-return 
  )
endif()

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

#
# tool target settings
#

add_executable(circuitous
    CircuitBuilder.h
    CircuitBuilder.cpp
    IR.h
    IR.cpp
    UseDef.h
    UseDef.cpp
    Main.cpp)

target_compile_definitions(circuitous PUBLIC
    ${PROJECT_DEFINITIONS})

target_compile_options(circuitous PUBLIC
    ${PROJECT_CXXFLAGS})

target_link_libraries(circuitous PUBLIC remill)


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
