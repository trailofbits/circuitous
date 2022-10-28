vcpkg_from_github(
  OUT_SOURCE_PATH SOURCE_PATH
  REPO lifting-bits/gap
  REF cfb4ac427e66106019167a9040746b3d3ca9da70
  SHA512 846261aa7592c779f0d4f2fb99b88d8be88a38d32233213d6b06aae34c2663b368d9e548ecbc3fd88346677189b6da56d146cb53c4c8c63d2845c4c1042048d9
  HEAD_REF main
)

vcpkg_cmake_configure(
  SOURCE_PATH "${SOURCE_PATH}"
  OPTIONS
    -DGAP_ENABLE_COROUTINES=ON
    -DGAP_ENABLE_TESTING=OFF
    -DGAP_ENABLE_EXAMPLES=OFF
    -DGAP_INSTALL=ON
    -DUSE_SYSTEM_DEPENDENCIES=ON
)

vcpkg_cmake_install()
vcpkg_cmake_config_fixup(
  PACKAGE_NAME "gap"
  CONFIG_PATH lib/cmake/gap
)

file( REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/debug" )

# we do not populate lib folder yet
file( REMOVE_RECURSE "${CURRENT_PACKAGES_DIR}/lib" )

file(
  INSTALL "${SOURCE_PATH}/LICENSE"
  DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}"
  RENAME copyright
)

file(
  INSTALL "${CMAKE_CURRENT_LIST_DIR}/usage"
  DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}"
)
