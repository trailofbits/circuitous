[![Build](https://github.com/trailofbits/circuitous/actions/workflows/build.yml/badge.svg)](https://github.com/trailofbits/circuitous/actions/workflows/build.yml)

## Build
Build of circuitous follows the traditional, modern `cmake` build process.
The simplest way to build and install the project is to use premade presets that take care of downloading all dependencies.
Dependencies of circuitous are managed by `vcpkg` manifest file.
Before building one need to specify environment variables `CXX_COMMON_ROOT` and `VCPKG_ROOT` that point to folders of pre-downloaded [vcpkg](https://github.com/microsoft/vcpkg) and [cxx-common](https://github.com/lifting-bits/cxx-common/tree/port-files) or
run `scripts/build/setup.sh` to download and setup these dependencies automatically.

Depending on your system, use `linux` or `osx` preset.  To make `debug` build use `deb` presets:

```
# configure project
cmake --preset ninja-cxx-common-x64-osx-rel

# build project
cmake --build builds/ninja-cxx-common-x64-osx-rel

# install project
cmake --build builds/ninja-cxx-common-x64-osx-rel --target install
```

## Development build

If you want to use your own prebuilt dependencies, project presets allow you to configure the build.
For example, if you have prebuild `llvm`, can be from `cxx-common` package, you can specify `CMAKE_PREFIX_PATH` to point to `LLVMConfig.cmake` folder.  For example:

```
CMAKE_PREFIX_PATH=${CXX_COMMON_ROOT}/installed/x64-linux-rel/share/:${CMAKE_PREFIX_PATH}
```

Then use presets with prefix `ninja-cxx-common-system-llvm` and your desired triplet. Similarly, to build with system remill use presets prefixed `ninja-system-remill`.

# Testing

```
ctest --preset ninja-osx-cxx-common-test
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

## Distribution and Licensing

The views, opinions, and/or findings expressed are those of the author(s) and
should not be interpreted as representing the official views or policies of the
Department of Defense or the U.S. Government.

*circuitous* is licensed under the GNU AGPLv3 License. A copy of the terms can
be found in the [LICENSE](./LICENSE) file.
