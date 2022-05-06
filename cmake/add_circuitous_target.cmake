# Copyright (c) 2022 Trail of Bits, Inc.

include(CMakeParseArguments)

function( add_circuitous_library name )
  cmake_parse_arguments( PARSED_ARGS "" "" "SOURCES;LINK_LIBS" ${ARGN} )

  message( STATUS "Adding circuitous library circuitous::${name}" )
  set( library_name circuitous_${name} )

  add_library( ${library_name} STATIC ${PARSED_ARGS_SOURCES} )

  target_link_libraries( ${library_name}
    PUBLIC
      circuitous::settings
      ${PARSED_ARGS_LINK_LIBS}
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
  cmake_parse_arguments( PARSED_ARGS "" "" "SOURCES;LINK_LIBS" ${ARGN} )

  set( executable_name circuitous-${name} )
  message( STATUS "Adding circuitous executable ${executable_name}" )

  add_executable( ${executable_name} ${PARSED_ARGS_SOURCES} )

  target_link_libraries( ${executable_name}
    PUBLIC
      circuitous::settings
      ${PARSED_ARGS_LINK_LIBS}
  )

  # install target
  if ( CIRCUITOUS_INSTALL )
    get_property( targets GLOBAL PROPERTY CIRCUITOUS_INSTALL_TARGETS )
    set( targets "${executable_name};${targets}")
    set_property( GLOBAL PROPERTY CIRCUITOUS_INSTALL_TARGETS ${targets} )
  endif()
endfunction()

