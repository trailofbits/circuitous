[![Build](https://github.com/trailofbits/circuitous/actions/workflows/build.yml/badge.svg)](https://github.com/trailofbits/circuitous/actions/workflows/build.yml)

## Build
Build of circuitous follows the traditional, modern `cmake` build process.
The simplest way to build and install the project is to use premade presets that take care of downloading all dependencies.
Dependencies of circuitous are managed by `vcpkg` manifest file.
Before building one need to specify environment variables `CXX_COMMON_ROOT` and `VCPKG_ROOT` that point to folders of pre-downloaded [vcpkg](https://github.com/microsoft/vcpkg) and [cxx-common](https://github.com/lifting-bits/cxx-common/tree/port-files) or
run `scripts/build/setup.sh` to download and setup these dependencies automatically. We also suggest setting `CC` and `CXX` to clang when also building dependancies, as some require this.

Depending on your system, use `linux` or `osx` preset.  To make `debug` build use `deb` presets:

```
# configure project
cmake --preset ninja-cxx-common-x64-osx-rel

# build project
cmake --build --preset ninja-cxx-common-osx-rel

# install project
cmake --build --preset ninja-cxx-common-osx-rel --target install
```

## Development build

If you want to use your own prebuilt dependencies, project presets allow you to configure the build.
For example, if you have prebuilt `llvm`, can be from `cxx-common` package, you can specify `CMAKE_PREFIX_PATH` to point to `LLVMConfig.cmake` folder.  For example:

```
CMAKE_PREFIX_PATH=${CXX_COMMON_ROOT}/installed/x64-linux-rel/share/:${CMAKE_PREFIX_PATH}
```

Then use presets with prefix `ninja-cxx-common-system-llvm` and your desired triplet. Similarly, to build with system remill use presets prefixed `ninja-system-remill`.

## Testing

```
ctest --preset ninja-cxx-common-osx-deb-test
```

## Dependencies

| Name | Version |
| ---- | ------- |
| [Git](https://git-scm.com/) | Latest |
| [CMake](https://cmake.org/) | 3.23+ |
| [Clang](http://clang.llvm.org/) | 12+ |
| [ccache](https://ccache.dev/) | Latest |
| [cxx-common](https://github.com/lifting-bits/cxx-common) | 0.2.6 |
| [llvm](https://github.com/lifting-bits/cxx-common) | 14+ |
| [remill](https://github.com/lifting-bits/remill) | Latest |
| [gap](https://github.com/lifting-bits/gap) | Latest |
| [doctest](https://github.com/doctest/doctest) | 2.4.8+ |
| [spdlog](https://github.com/gabime/spdlog) | 1.10.0 |

For python dependencies see `requirements.txt` and for c++ `vcpkg.json`.


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

## Dev Container for Actions

Run manually __Build Dev Container__ action or execute:

```
docker build .devcontainer --no-cache \
    -t ghcr.io/trailofbits/circuitous-ubuntu-22.04-dev:latest
```

```
docker push ghcr.io/trailofbits/circuitous-ubuntu-22.04-dev:latest
```

This requires to have CLI access to github resources.

## Distribution and Licensing

The views, opinions, and/or findings expressed are those of the author(s) and
should not be interpreted as representing the official views or policies of the
Department of Defense or the U.S. Government.

*circuitous* is licensed under the GNU AGPLv3 License. A copy of the terms can
be found in the [LICENSE](./LICENSE) file.
