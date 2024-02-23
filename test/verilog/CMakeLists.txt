#
# Copyright (c) 2024 Trail of Bits, Inc.
#

configure_file( scripts/ciff-trace.sh.in scripts/ciff-trace.sh @ONLY )
configure_file( scripts/from-trace.sh.in scripts/from-trace.sh @ONLY )

file( COPY scripts DESTINATION . )
file( COPY external DESTINATION . )
file( COPY input_mttn_traces DESTINATION . )
file( COPY run.sh DESTINATION . )

add_test( NAME verilog-tests
    COMMAND sh run.sh
)
