# Copyright (c) 2020-2021 Trail of Bits, Inc.
cmake_minimum_required( VERSION 3.21 )

#
# Use gap library
#
find_package( gap CONFIG REQUIRED )

set_property( GLOBAL PROPERTY USE_FOLDERS ON )

# prevent in source builds check
include( prevent_in_source_builds )

# general project options
include( project_settings )

# check if circuitous is being used directly or via add_subdirectory,
# but allow overriding
if( NOT DEFINED CIRCUITOUS_MASTER_PROJECT )
    if( CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR )
        set( CIRCUITOUS_MASTER_PROJECT ON )
    else()
        set( CIRCUITOUS_MASTER_PROJECT OFF )
    endif()
endif()

list( APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" )

include( vcpkg_setup )
find_package( remill COMPONENTS VCPKG_DEPS QUIET )

# Fix behavior of CMAKE_CXX_STANDARD when targeting macOS.
if ( POLICY CMP0025 )
  cmake_policy( SET CMP0025 NEW )
endif ()

if ( POLICY CMP0116 )
  cmake_policy( SET CMP0116 NEW )
endif ()

project( circuitous
  LANGUAGES C CXX ASM
  VERSION 0.1.0
  DESCRIPTION "Binary to circuit lifter tool"
  HOMEPAGE_URL "https://github.com/trailofbits/circuitous.git"
)

include( find_clang )
FindAndSelectClangCompiler()

#
# libraries
#
find_package( remill CONFIG REQUIRED )
find_package( gflags CONFIG REQUIRED )

#
# standard compilation options
#
add_library( circuitous_project_options INTERFACE )

#
# target settings
#
add_library( circuitous_settings INTERFACE )
target_compile_features( circuitous_settings INTERFACE cxx_std_20 )

target_include_directories( circuitous_settings INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)

# standard compiler warnings
include( compiler_warnings )
set_project_warnings( circuitous_settings )

# sanitizer options if supported by compiler
include( sanitizers )
enable_sanitizers( circuitous_settings )

# sanitizer options if supported by compiler
include( sanitizers )
enable_sanitizers( circuitous_settings )

add_library( cicuitous_dependencies INTERFACE )
target_link_libraries( cicuitous_dependencies INTERFACE
  remill
  gap::gap
)

target_link_libraries( circuitous_settings INTERFACE
  cicuitous_dependencies
)

add_library( circuitous::settings ALIAS circuitous_settings )

include( add_circuitous_target )

#
# install settings
#
option( CIRCUITOUS_INSTALL "Generate the install target." ${CIRCUITOUS_MASTER_PROJECT} )
include( GNUInstallDirs )

#
# library targets
#
add_subdirectory( lib )

#
# executable targets
#
add_subdirectory( bin )

#
# tests
#
option( ENABLE_TESTING "Enable Test Builds" ON )

if ( ENABLE_TESTING )
  enable_testing()
  add_subdirectory( test )
endif()
