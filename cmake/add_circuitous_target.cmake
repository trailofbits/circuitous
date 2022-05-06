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

  add_library( circuitous::${name} ALIAS ${library_name} )
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

endfunction()

