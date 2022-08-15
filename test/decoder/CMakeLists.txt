#
# Copyright (c) 2022 Trail of Bits, Inc.
#

find_package( Python3 REQUIRED COMPONENTS Interpreter )

configure_file( decoder_test.py.in decoder_test.py @ONLY )
configure_file( decoder_tester.cpp decoder_tester.cpp @ONLY )

file( COPY decoder_tester.cpp DESTINATION . )

add_test( NAME decoder-tests
    COMMAND ${Python3_EXECUTABLE} -m pytest decoder_test.py
)
