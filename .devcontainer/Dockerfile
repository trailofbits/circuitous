# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.241.1/containers/cpp/.devcontainer/base.Dockerfile

FROM mcr.microsoft.com/vscode/devcontainers/cpp:ubuntu-22.04

# [Optional] Install CMake version different from what base image has already installed.
# CMake reinstall choices: none, 3.21.5, 3.22.2, or versions from https://cmake.org/download/
ARG REINSTALL_CMAKE_VERSION_FROM_SOURCE="3.23.3"

# Optionally install the cmake for vcpkg
COPY ./reinstall-cmake.sh /tmp/
RUN if [ "${REINSTALL_CMAKE_VERSION_FROM_SOURCE}" != "none" ]; then \
        chmod +x /tmp/reinstall-cmake.sh && /tmp/reinstall-cmake.sh ${REINSTALL_CMAKE_VERSION_FROM_SOURCE}; \
    fi \
    && rm -f /tmp/reinstall-cmake.sh

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends llvm-14 llvm-14-dev libclang-14-dev \
        build-essential ccache cmake-curses-gui libedit-dev libpfm4-dev zlib1g-dev python3-pip

# Remill dependencies
RUN dpkg --add-architecture i386
RUN apt-get update
RUN apt-get -y install --no-install-recommends libc6-dev:i386 libstdc++-10-dev:i386 g++-multilib

RUN update-alternatives --install /usr/bin/c++ c++ /usr/bin/clang++-14 60
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/clang-14 60

# Work around broken llvm packaging
RUN touch /usr/lib/llvm-14/lib/libMLIRSupportIndentedOstream.a

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
    -DUSE_SYSTEM_DEPENDENCIES=ON

RUN cmake --build build --target install

# Install python dependencies
RUN python3 -m pip install microx pytest
