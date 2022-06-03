#
# Copyright (c) 2022 Trail of Bits, Inc.
#

find_package( Python3 REQUIRED COMPONENTS Interpreter )

configure_file( run.py.in run.py @ONLY )
configure_file( decoder_test.py decoder_test.py @ONLY )

file( COPY defs DESTINATION . )
file( COPY tools DESTINATION . )
file( COPY decoder DESTINATION . )

add_test( NAME min-tests
    COMMAND ${Python3_EXECUTABLE} run.py --tags min
)

add_test( NAME decoder-tests
    COMMAND ${Python3_EXECUTABLE} -m pytest decoder_test.py
)