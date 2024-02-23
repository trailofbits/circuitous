#!/bin/sh

set -x

circuit=""
os="linux"
arch="x86"
use_cache_flag=""
quiet_flag=""

process_traces() {
    local script="$1"
    local failed=0
    for trace in input_mttn_traces/*; do
        sh "$script" --os "$os" --arch "$arch" "$use_cache_flag" "$quiet_flag" "$trace" >&2 || failed=1
    done
    return $failed
}

display_help() {
    echo "Usage: $0 [--circuit <circuit_file>] [--os <operating_system>] [--arch <architecture>] [--no-cache] [--quiet] [--help]"
    echo "Options:"
    echo "  --circuit <circuit_file>    Specify the circuit for execution"
    echo "  --os <operating_system>     Specify the operating system (default: $os)"
    echo "  --arch <architecture>       Specify the architecture (default: $arch)"
    echo "  --no-cache                  Disable caching"
    echo "  --quiet                     Run in quiet mode"
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
        --os)
            os="$2"
            shift 2
            ;;
        --arch)
            arch="$2"
            shift 2
            ;;
        --no-cache)
            use_cache_flag="--no-cache"
            shift
            ;;
        --quiet)
            quiet_flag="--quiet"
            shift
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
    process_traces "scripts/ciff-trace.sh $circuit"
fi

exit $?
