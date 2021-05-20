[![Build](https://github.com/trailofbits/circuitous/actions/workflows/build.yml/badge.svg)](https://github.com/trailofbits/circuitous/actions/workflows/build.yml)

# Installation

Install `remill` and `cxx-common`. Then use the same process here, e.g. something like:
```
mkdir build && cd build
cmake -DVCPKG_ROOT=/path/to/vckpg \
      -DVCPKG_TARGET_TRIPLET=your-triplet \
      -Dremill_DIR=/path/to/remill/install/lib/cmake/remill \
      ..
make
```

# Usage

## circuitous-run

TODO

## circuitous-lift

There is a bunch of `*_out` options - if you want something human readable your best shot is probably `--dot_out` that produces the dot graph.

The `--os` and `--arch` are defaulted to mac.

Input can be provided by either `--bytes_in` which accept bytes as hex string (e.g. `--bytes_in ba12000000`) or by `--binary_in` which expects a filename that contains the instructions in binary form (not the assembly code!).

Option `--reduce_imms` will run the (for now) experimental reduction of immediate operands.

## Tests

See `test/README.md`
