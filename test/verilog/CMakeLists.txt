#
# Copyright (c) 2024 Trail of Bits, Inc.
#

configure_file( scripts/ciff-and-mttn-trace.sh.in scripts/ciff-and-mttn-trace.sh @ONLY )
configure_file( scripts/from-mttn-trace.sh.in scripts/from-mttn-trace.sh @ONLY )

file( COPY scripts DESTINATION . )
file( COPY external DESTINATION . )
file( COPY input_mttn_traces DESTINATION . )
file( COPY run.sh DESTINATION . )

add_test( NAME verilog-tests
    COMMAND sh run.sh
)
