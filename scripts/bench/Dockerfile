FROM mcr.microsoft.com/vscode/devcontainers/cpp:ubuntu-22.04 AS base

# CMake reinstall choices: none, 3.21.5, 3.22.2, or versions from https://cmake.org/download/
ARG REINSTALL_CMAKE_VERSION_FROM_SOURCE="3.23.3"

# Optionally install the cmake for vcpkg
COPY ./reinstall-cmake.sh /tmp/
RUN if [ "${REINSTALL_CMAKE_VERSION_FROM_SOURCE}" != "none" ]; then \
        chmod +x /tmp/reinstall-cmake.sh && /tmp/reinstall-cmake.sh ${REINSTALL_CMAKE_VERSION_FROM_SOURCE}; \
    fi \
    && rm -f /tmp/reinstall-cmake.sh

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
        software-properties-common \
        build-essential ninja-build ccache cmake-curses-gui libedit-dev libpfm4-dev \
        zlib1g-dev python3-pip \
        verilator yosys

# Install LLVM 16
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | sudo apt-key add -
RUN add-apt-repository 'deb http://apt.llvm.org/jammy/ llvm-toolchain-jammy-16 main'

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
        clang-16 clang-tools-16 libclang-common-16-dev libclang-16-dev \
        libllvm16 llvm-16 llvm-16-dev libclang-rt-16-dev

# Remill dependencies
RUN dpkg --add-architecture i386
RUN apt-get update
RUN apt-get -y install --no-install-recommends libc6-dev:i386 libstdc++-10-dev:i386 g++-multilib

RUN update-alternatives --install /usr/bin/c++ c++ /usr/bin/clang++-16 60
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/clang-16 60

RUN apt-get -y purge llvm-14 clang-14

FROM base as remill

# setup cxx-common
RUN git clone -b port-files https://github.com/lifting-bits/cxx-common.git /usr/src/cxx-common

# setup remill
WORKDIR /usr/src/
RUN git clone -b vcpkg-manifest https://github.com/lifting-bits/remill.git

WORKDIR /usr/src/remill
RUN cmake -B build -S . \
    -DCMAKE_TOOLCHAIN_FILE=/usr/local/vcpkg/scripts/buildsystems/vcpkg.cmake \
    -DVCPKG_OVERLAY_TRIPLETS=/usr/src/cxx-common/triplets \
    -DVCPKG_OVERLAY_PORTS=/usr/src/cxx-common/ports \
    -DVCPKG_TARGET_TRIPLET="x64-linux-rel" \
    -DVCPKG_HOST_TRIPLET="x64-linux-rel" \
    -DVCPKG_MANIFEST_NO_DEFAULT_FEATURES=ON \
    -DREMILL_ENABLE_TESTING=OFF \
    -DREMILL_ENABLE_INSTALL_TARGET=ON

RUN cmake --build build --target install

FROM remill AS circuitous

# setup circuitous
WORKDIR /usr/src/
RUN git clone https://github.com/trailofbits/circuitous.git

WORKDIR /usr/src/circuitous
RUN cmake --preset ninja-system-remill-x64-linux-rel \
    -DCMAKE_TOOLCHAIN_FILE=/usr/local/vcpkg/scripts/buildsystems/vcpkg.cmake \
    -DVCPKG_OVERLAY_TRIPLETS=/usr/src/cxx-common/triplets \
    -DVCPKG_OVERLAY_PORTS=/usr/src/cxx-common/ports \
    -DVCPKG_TARGET_TRIPLET="x64-linux-rel" \
    -DVCPKG_HOST_TRIPLET="x64-linux-rel" \
    -DVCPKG_MANIFEST_NO_DEFAULT_FEATURES=ON \
    -DENABLE_TESTING=OFF

RUN cmake --build ./builds/ninja-system-remill-x64-linux-rel -j $(nproc)

RUN cmake --build ./builds/ninja-system-remill-x64-linux-rel --target install

FROM circuitous AS benchmarks

# setup circuitous benchmarks
WORKDIR /usr/src/
RUN git clone https://github.com/trailofbits/circuitous-benchmarks.git

WORKDIR /usr/src/circuitous-benchmarks

RUN cmake --preset ninja-cxx-common-x64-linux-rel \
    -DCMAKE_TOOLCHAIN_FILE=/usr/local/vcpkg/scripts/buildsystems/vcpkg.cmake \
    -DVCPKG_OVERLAY_TRIPLETS=/usr/src/cxx-common/triplets \
    -DVCPKG_OVERLAY_PORTS=/usr/src/cxx-common/ports

RUN cmake --build --preset ninja-cxx-common-linux-rel

WORKDIR /usr/src/circuitous-benchmarks/builds/ninja-cxx-common-x64-linux-rel/
ENTRYPOINT ["./bench/circ-bench"]
