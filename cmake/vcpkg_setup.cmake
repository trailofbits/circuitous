#
# Copyright (c) 2021-present, Trail of Bits, Inc. All rights reserved.
#
# This source code is licensed in accordance with the terms specified in the LICENSE file found in
# the root directory of this source tree.
#

option(USE_SYSTEM_DEPENDENCIES OFF "Use system dependencies instead of trying to find vcpkg")

if (NOT USE_SYSTEM_DEPENDENCIES)
  set(VCPKG_ROOT
      ""
      CACHE FILEPATH "Root directory to use for vcpkg-managed dependencies"
  )

  if (DEFINED ENV{VCPKG_ROOT} AND NOT VCPKG_ROOT)
    set(VCPKG_ROOT $ENV{VCPKG_ROOT})
  endif ()

  if (VCPKG_ROOT)
    if (NOT EXISTS "${VCPKG_ROOT}")
      message(FATAL_ERROR "VCPKG_ROOT directory does not exist: '${VCPKG_ROOT}'")
    endif ()

    set(CMAKE_TOOLCHAIN_FILE
        "${VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake"
        CACHE FILEPATH "" FORCE
    )
  else ()
    message(
      FATAL_ERROR
        "Please define a path to VCPKG_ROOT." "Or if you don't want to use vcpkg dependencies,"
        "add '-DUSE_SYSTEM_DEPENDENCIES=ON'"
    )
  endif ()

  # Set default triplet to Release VCPKG build unless we can't find it
  if (NOT DEFINED VCPKG_TARGET_TRIPLET)
    if (APPLE)
      set(_project_vcpkg_triplet "x64-osx")
    elseif (UNIX)
      set(_project_vcpkg_triplet "x64-linux")
    else ()
      message(FATAL_ERROR "Could not detect default release triplet")
    endif ()

    set(VCPKG_TARGET_TRIPLET
        "${_project_vcpkg_triplet}"
        CACHE STRING ""
    )
    message(
      STATUS "Setting default vcpkg triplet to release-only libraries: ${VCPKG_TARGET_TRIPLET}"
    )
  endif ()
endif ()
