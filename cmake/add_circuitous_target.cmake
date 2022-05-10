# Copyright (c) 2022 Trail of Bits, Inc.

include(CMakeParseArguments)

function( add_circuitous_header_library name )
  cmake_parse_arguments( PARSED_ARGS "" "" "LINK_LIBS;HEADERS" ${ARGN} )

  message( STATUS "Adding circuitous library circuitous::${name}" )
  set( library_name circuitous_${name} )

  add_library( ${library_name} INTERFACE )

  target_link_libraries( ${library_name}
    INTERFACE
      circuitous::settings
      circuitous::support
      circuitous::util
      ${PARSED_ARGS_LINK_LIBS}
  )

  target_sources( ${library_name} PUBLIC FILE_SET HEADERS
    BASE_DIRS ${PROJECT_SOURCE_DIR}/include
    FILES ${PARSED_ARGS_HEADERS}
  )

  # setup namespaced alais
  add_library( circuitous::${name} ALIAS ${library_name} )

  # install target
  if ( CIRCUITOUS_INSTALL )
    get_property( targets GLOBAL PROPERTY CIRCUITOUS_INSTALL_TARGETS )
    set( targets "${library_name};${targets}")
    set_property( GLOBAL PROPERTY CIRCUITOUS_INSTALL_TARGETS ${targets} )
  endif()
endfunction()

function( add_circuitous_library name )
  cmake_parse_arguments( PARSED_ARGS "" "" "SOURCES;LINK_LIBS;HEADERS" ${ARGN} )

  message( STATUS "Adding circuitous library circuitous::${name}" )
  set( library_name circuitous_${name} )

  add_library( ${library_name} STATIC ${PARSED_ARGS_SOURCES} )

  target_link_libraries( ${library_name}
    PUBLIC
      circuitous::settings
      circuitous::support
      circuitous::util
      ${PARSED_ARGS_LINK_LIBS}
  )

  target_sources( ${library_name} PUBLIC FILE_SET HEADERS
    BASE_DIRS ${PROJECT_SOURCE_DIR}/include
    FILES ${PARSED_ARGS_HEADERS}
  )

  # setup namespaced alais
  add_library( circuitous::${name} ALIAS ${library_name} )

  # install target
  if ( CIRCUITOUS_INSTALL )
    get_property( targets GLOBAL PROPERTY CIRCUITOUS_INSTALL_TARGETS )
    set( targets "${library_name};${targets}")
    set_property( GLOBAL PROPERTY CIRCUITOUS_INSTALL_TARGETS ${targets} )
  endif()
endfunction()

function( add_circuitous_executable name )
  cmake_parse_arguments( PARSED_ARGS "" "" "SOURCES;LINK_LIBS;HEADERS" ${ARGN} )

  set( executable_name circuitous-${name} )
  message( STATUS "Adding circuitous executable ${executable_name}" )

  add_executable( ${executable_name} ${PARSED_ARGS_SOURCES} )

  target_link_libraries( ${executable_name}
    PUBLIC
      circuitous::settings
      circuitous::support
      circuitous::util
      ${PARSED_ARGS_LINK_LIBS}
  )

  target_sources( ${executable_name} PUBLIC FILE_SET HEADERS
    BASE_DIRS ${PROJECT_SOURCE_DIR}/include
    FILES ${PARSED_ARGS_HEADERS}
  )

  # install target
  if ( CIRCUITOUS_INSTALL )
    get_property( targets GLOBAL PROPERTY CIRCUITOUS_INSTALL_TARGETS )
    set( targets "${executable_name};${targets}")
    set_property( GLOBAL PROPERTY CIRCUITOUS_INSTALL_TARGETS ${targets} )
  endif()
endfunction()

