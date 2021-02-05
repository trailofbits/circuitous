# Installation

Install `remill` and `cxx-common`. Then use the same process here, e.g. something like:
```
cmake -DVCPKG_ROOT=/path/to/vckpg -DVCPKG_TARGET_TRIPLET=your-triplet -Dremill_DIR=/path/to/remill/install/lib/cmake/remill ..
```

# Usage

## circuitous-lift

TODO

## circuitous-run

There is a bunch of `*_out` options - if you want something human readable your best shot is probably `--dot_out` that produces the dot graph.

The `--os` and `--arch` are defaulted to mac.

Input can be provided by either `--bytes_in` which accept bytes as hex string (e.g. `--bytes_in ba12000000`) or by `--binary_in` which expects a filename that contains the instructions in binary form (not the assembly code!).
