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
        build-essential ccache cmake-curses-gui libedit-dev libpfm4-dev python3-pip

RUN update-alternatives --install /usr/bin/c++ c++ /usr/bin/clang++-14 60
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/clang-14 60

# Work around broken llvm packaging
RUN touch /usr/lib/llvm-14/lib/libMLIRSupportIndentedOstream.a

# setup cxx-common
RUN git clone -b port-files https://github.com/lifting-bits/cxx-common.git /usr/src/cxx-common

# setup remill
WORKDIR /usr/src/
RUN wget https://github.com/lifting-bits/remill/releases/download/v5.0.4/remill_ubuntu-20.04_packages.zip \
    && unzip remill_ubuntu-20.04_packages.zip
RUN apt-get install ./ubuntu-20.04_llvm14_deb_package/remill-5.0.4.43cfa8c-x86_64.deb

RUN python3 -m pip install microx pytest
