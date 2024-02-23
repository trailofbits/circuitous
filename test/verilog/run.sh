#!/bin/sh

circuit=""

process_traces() {
    local script="$1"
    local failed=0
    for trace in input_mttn_traces/*; do
        sh "$script" "$trace" >&2 || failed=1
    done
    return $failed
}

display_help() {
    echo "Usage: $0 [--circuit <circuit_file>] [--help]"
    echo "Options:"
    echo "  --circuit <circuit_file>    Specify the circuit for execution"
    echo "  --help                      Display this help message"
    exit 0
}

# Process command-line options
while [ "$#" -gt 0 ]; do
    case "$1" in
        --circuit)
            circuit="$2"
            shift 2
            ;;
        --help)
            display_help
            ;;
        *)
            shift
            ;;
    esac
done

# Generate circuit from trace when --circuit was not provided
if [ -z "$circuit" ]; then
    process_traces scripts/from-trace.sh
else
    process_traces "scripts/ciff-and-mttn-trace.sh $circuit"
fi

exit $?
