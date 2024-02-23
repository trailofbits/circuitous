#!/bin/sh

failed=0

for trace in input_mttn_traces/*; do
    sh scripts/from-trace.sh $trace >&2 || failed=1
done

if [ $failed -ne 0 ]; then
    exit 1
fi

#tiny_86="inputs/tiny86.ciff"
#for trace in input_mttn_traces/*; do
#    sh scripts/ciff-and-mttn-trace.sh $tiny_86 $trace >&2 || failed=1
#done


if [ $failed -ne 0 ]; then
    exit 1
fi

exit $failed
