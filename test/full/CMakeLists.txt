#
# Copyright (c) 2022 Trail of Bits, Inc.
#

find_package( Python REQUIRED )
include(FindPythonInterp)

configure_file( run.py.in run.py @ONLY )

file( COPY defs DESTINATION . )
file( COPY tools DESTINATION . )

add_test( NAME min-tests
    COMMAND ${PYTHON_EXECUTABLE} run.py --tags min
)
