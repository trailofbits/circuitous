# Copyright (c) 2020-2021 Trail of Bits, Inc.
cmake_minimum_required( VERSION 3.23 )

list( APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" )
include( vcpkg_setup )

set_property( GLOBAL PROPERTY USE_FOLDERS ON )


# check if circuitous is being used directly or via add_subdirectory,
# but allow overriding
if( NOT DEFINED CIRCUITOUS_MASTER_PROJECT )
    if( CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR )
        set( CIRCUITOUS_MASTER_PROJECT ON )
    else()
        set( CIRCUITOUS_MASTER_PROJECT OFF )
    endif()
endif()


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

#
# Use gap library
#
find_package( gap CONFIG REQUIRED )

# prevent in source builds check
include( prevent_in_source_builds )

# general project options
include( project_settings )

include( find_clang )
FindAndSelectClangCompiler()

#
# CCACHE
#
find_program( CCACHE_PROGRAM ccache )
if ( CCACHE_PROGRAM )
  set_property( GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}" )
endif ()

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

# adds header paths of library to list named var
function(add_headers lib var)
  set(headers ${${var}})
  foreach (header ${ARGN})
    set(headers ${headers} ${PROJECT_SOURCE_DIR}/include/circuitous/${lib}/${header})
  endforeach()
  set(${var} ${headers} PARENT_SCOPE)
endfunction()


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

set_property( GLOBAL PROPERTY CIRCUITOUS_INSTALL_TARGETS
  "circuitous_settings;cicuitous_dependencies"
)

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

#
# instalation support
#

add_library( ${PROJECT_NAME} INTERFACE )

set_property(
  TARGET ${PROJECT_NAME} PROPERTY POSITION_INDEPENDENT_CODE ON
)

set_target_properties( ${PROJECT_NAME}
  PROPERTIES VERSION ${PROJECT_VERSION} SOVERSION ${PROJECT_VERSION_MAJOR}
)


if (CIRCUITOUS_INSTALL)
  set(CIRCUITOUS_CMAKE_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

  get_property( CIRCUITOUS_INSTALL_TARGETS GLOBAL PROPERTY CIRCUITOUS_INSTALL_TARGETS )

  set(CIRCUITOUS_EXPORT_NAME ${PROJECT_NAME}Targets)

  install(TARGETS ${CIRCUITOUS_INSTALL_TARGETS}
    EXPORT ${CIRCUITOUS_EXPORT_NAME}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      NAMELINK_SKIP
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/circuitous
    FILE_SET HEADERS
  )

  install(EXPORT ${CIRCUITOUS_EXPORT_NAME}
    FILE ${CIRCUITOUS_EXPORT_NAME}.cmake
    NAMESPACE circuitous::
    DESTINATION ${CIRCUITOUS_CMAKE_INSTALL_DIR}
  )

  install(TARGETS ${CIRCUITOUS_INSTALL_TARGETS}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      NAMELINK_ONLY
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/circuitous
  )

  #
  # packaging support
  #

  set(CPACK_PACKAGE_VENDOR "Trail of Bits")
  set(CPACK_PACKAGE_DESCRIPTION_SUMMARY
    "Binary to circuit lifter tool"
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
