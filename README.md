## Overview

Circuitous is a set of tools and libraries that focus on production of ZK
circuits tailored for a specific binary. Using circuit that is tailored for a
specific binary has many upsides; there are only instructions actually used in
the binary - no need to pay for what you don't use. Since circuits are
generated rather than handwritten, they are super easy to extend (just run
tooling again if your binary changed) or the framework can be used to
experiment with different approaches to circuit design (different cost models).

While we do provide core parts of the project as a libary, there are also some
drivers ready to be used - `circuitous-seed` can provide a list of instructions
to lift from a given binary (some can be blacklisted) and the list can be
modified afterwards. `circuitous-lift` then takes this input and emits circuit
with different forms (verilog being the most "end" result). We will eventually
provide a full step-by step example but for now you can invoke `--help` on the
respective tools, see the `Usage` section or in the worst case look into
tests/open us an issue or discussion topic!

Since this is project in active development there are some caveats.
 * We currently support only subset of x86 (both 32b and 64b) and
   the API is not super stable yet.
 * Due to how internals of circuitous work it is hard to produce a circuit with
   all forms of given instruction (let's say all `add`) - as it ingests the
   instruction encoding as input.
 * Reducing circuit size is a really hard task and we are still trying to
   improve it.


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

## circuitous-lift

The main binary that produces the circuits. It has several options, which falls
into few categories.
 * `--arch` and `--os` are defaulted to mac and current machine (these are
   needed to initialise libraries circuitous uses under the hood)
 * Some for of input
  - `--bytes-in` raw bytes in human readable form (for example `1468` for some
    version of `ADC`). More encodings should be just concatenated together.
  - `--ciff-in` a config file provided by `circuitous-seed`. You can find some
    example in `scripts/seed_sets/*.ciff`
 * Run informations
  - `--lift-with` to specify the lifter to use (currently only `v2` is supported
    with `v1` being deprecated and `v3` in active development. As you probably
    guessed these are placeholder names.
 * Outputs
  - `--verilog-out` outputs the circuit in verilog
 * Random
  - `--quiet` to silence outputs

So an example invocation may look like
```bash
circuitous-lift --arch amd64 --os macos \
                --lift-with v2 \
                --bytes-in 1468 \
                --verilog-output out.v \
                --quiet
```

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
