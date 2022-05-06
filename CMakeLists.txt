# Copyright (c) 2020-2021 Trail of Bits, Inc.
cmake_minimum_required(VERSION 3.14)

include(cmake/vcpkg_helper.cmake)
find_package(remill COMPONENTS VCPKG_DEPS QUIET)

# Fix behavior of CMAKE_CXX_STANDARD when targeting macOS.
if (POLICY CMP0025)
  cmake_policy(SET CMP0025 NEW)
endif ()

if (POLICY CMP0116)
  cmake_policy(SET CMP0116 NEW)
endif ()

list( APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" )

project( circuitous
  LANGUAGES C CXX ASM
  VERSION 0.1.0
  DESCRIPTION "Binary to circuit lifter tool"
  HOMEPAGE_URL "https://github.com/trailofbits/circuitous.git"
)

#
# Use gap library
#
find_package(gap CONFIG REQUIRED)

set_property( GLOBAL PROPERTY USE_FOLDERS ON )

# prevent in source builds check
include( prevent_in_source_builds )

# general project options
include( project_settings )

include(GNUInstallDirs)

# check if circuitous is being used directly or via add_subdirectory,
# but allow overriding
if(NOT DEFINED CIRCUITOUS_MASTER_PROJECT)
    if(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
        set(CIRCUITOUS_MASTER_PROJECT ON)
    else()
        set(CIRCUITOUS_MASTER_PROJECT OFF)
    endif()
endif()

include( find_clang )
FindAndSelectClangCompiler()

#
# libraries
#
find_package(remill CONFIG REQUIRED)
find_package(gflags CONFIG REQUIRED)

#
# standard compilation options
#
add_library( circuitous_project_options INTERFACE )

#
# target settings
#
add_library( circuitous_settings INTERFACE )
target_compile_features( circuitous_settings INTERFACE cxx_std_20 )

target_include_directories(circuitous_settings INTERFACE
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
  remill_settings
  remill
  gflags
  gap::gap
)

target_link_libraries( circuitous_settings INTERFACE
  cicuitous_dependencies
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
  lib/Lifter/ShadowMat.cpp
  lib/Lifter/Shadows.cpp
  lib/Lifter/CircuitSmithy.cpp
  lib/Lifter/Instruction.cpp

  lib/Lifter/Components/Decoder.cpp
)

target_link_libraries(circuitous_lifter PUBLIC circuitous_settings)

add_library(circuitous_printers STATIC
  lib/Printers/DOT.cpp
  lib/Printers/JSON.cpp
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

target_link_libraries(circuitous-seed PUBLIC circuitous_settings circuitous_lifter)

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
option( ENABLE_TESTING "Enable Test Builds" ON )

if ( ENABLE_TESTING )
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

option(CIRCUITOUS_INSTALL "Generate the install target." ${CIRCUITOUS_MASTER_PROJECT})

if (CIRCUITOUS_INSTALL)
  set(CIRCUITOUS_CMAKE_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

  set(CIRCUITOUS_INSTALL_TARGETS
    # executables
    circuitous-lift
    circuitous-run
    # libraries
    cicuitous_dependencies
    circuitous_eqsat
    circuitous_ir
    circuitous_lifter
    circuitous_printers
    circuitous_settings
    circuitous_transforms
  )

  set(CIRCUITOUS_EXPORT_NAME circuitousTargets)

  install(TARGETS ${CIRCUITOUS_INSTALL_TARGETS}
    EXPORT ${CIRCUITOUS_EXPORT_NAME}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      NAMELINK_SKIP
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gap/core
  )

  install(EXPORT ${CIRCUITOUS_EXPORT_NAME}
    FILE ${CIRCUITOUS_EXPORT_NAME}.cmake
    NAMESPACE gap::
    DESTINATION ${CIRCUITOUS_CMAKE_INSTALL_DIR}
  )

  install(TARGETS ${CIRCUITOUS_INSTALL_TARGETS}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      NAMELINK_ONLY
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gap/core
  )


  #
  # packaging support
  #

  set(CPACK_PACKAGE_VENDOR "Trail of Bits")
  set(CPACK_PACKAGE_DESCRIPTION_SUMMARY
    "A utility library to bridge llvm and mlir gaps."
  )
  set(CPACK_PACKAGE_VERSION_MAJOR ${CMAKE_PROJECT_VERSION_MAJOR})
  set(CPACK_PACKAGE_VERSION_MINOR ${CMAKE_PROJECT_VERSION_MINOR})
  set(CPACK_PACKAGE_VERSION_PATCH ${CMAKE_PROJECT_VERSION_PATCH})
  set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
  set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")

  include(CPack)

  set(CIRCUITOUS_CONFIG_NAME "${PROJECT_NAME}Config")
  set(CIRCUITOUS_PACKAGE_CONFIG_FILE "${CIRCUITOUS_CONFIG_NAME}.cmake")
  set(CIRCUITOUS_PACKAGE_CONFIG_VERSION_FILE "${CIRCUITOUS_CONFIG_NAME}Version.cmake")

  include(CMakePackageConfigHelpers)

  configure_package_config_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/${CIRCUITOUS_CONFIG_NAME}.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/${CIRCUITOUS_PACKAGE_CONFIG_FILE}"
    INSTALL_DESTINATION ${CIRCUITOUS_CMAKE_INSTALL_DIR}
  )

  write_basic_package_version_file(
    ${CIRCUITOUS_PACKAGE_CONFIG_VERSION_FILE}
    VERSION ${PACKAGE_VERSION}
    COMPATIBILITY SameMajorVersion
  )

  install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/${CIRCUITOUS_PACKAGE_CONFIG_FILE}"
    "${CMAKE_CURRENT_BINARY_DIR}/${CIRCUITOUS_PACKAGE_CONFIG_VERSION_FILE}"
    DESTINATION ${CIRCUITOUS_CMAKE_INSTALL_DIR}
  )

endif()
