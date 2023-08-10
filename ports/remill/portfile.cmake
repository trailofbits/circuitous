vcpkg_from_github(
  OUT_SOURCE_PATH SOURCE_PATH
  REPO lifting-bits/remill
  REF 589c80ef91dd8a2cd002a2762d8d4e22a1fff7cb
  SHA512 0
  HEAD_REF vcpkg-manifest
)

vcpkg_cmake_configure(
  SOURCE_PATH "${SOURCE_PATH}"
  OPTIONS
    -DUSE_SYSTEM_DEPENDENCIES=ON
)

vcpkg_cmake_install()
vcpkg_cmake_config_fixup(
  PACKAGE_NAME "remill"
  CONFIG_PATH lib/cmake/remill
)

file( REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug/include" )
file( REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug/share" )

if ( VCPKG_LIBRARY_LINKAGE STREQUAL "static" )
  file( REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/bin" "${CURRENT_PACKAGES_DIR}/debug/bin" )
endif()

file(
  INSTALL "${SOURCE_PATH}/LICENSE"
  DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}"
  RENAME copyright
)

file(
  INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage"
  DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}"
)
