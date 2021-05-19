ARG LLVM_VERSION=12
ARG ARCH=amd64
ARG UBUNTU_VERSION=20.04
ARG DISTRO_BASE=ubuntu${UBUNTU_VERSION}
ARG BUILD_BASE=ubuntu:${UBUNTU_VERSION}
ARG LIBRARIES=/opt/trailofbits

# Used for downloading remill and then copied to required stages
FROM ${BUILD_BASE} as store
ARG UBUNTU_VERSION
ARG LLVM_VERSION

WORKDIR /dependencies/tmp
ADD https://github.com/lifting-bits/remill/releases/latest/download/remill_ubuntu-${UBUNTU_VERSION}_packages.zip remill_packages.zip

# Saves a bit of space in the base image.
# Also better for not repeating ourselves when installing remill
RUN apt-get update && \
    apt-get install -qqy --no-install-recommends unzip && \
    rm -rf /var/lib/apt/lists/* && \
    unzip remill_packages.zip && \
    rm remill_packages.zip && \
    mv ubuntu-${UBUNTU_VERSION}_llvm${LLVM_VERSION}_deb_package/remill-*.deb ../remill.deb && \
    cd .. && rm -rf tmp

# Run-time dependencies go here
FROM ${BUILD_BASE} AS base
ARG UBUNTU_VERSION
ARG LIBRARIES
ARG LLVM_VERSION
RUN apt-get update && \
    apt-get install -qqy --no-install-recommends curl unzip python3 python3-pip python3.8 python3.8-venv python3-setuptools xz-utils && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /dependencies

COPY requirements.txt .
RUN pip3 install --no-cache-dir -r requirements.txt

#### NOTE ####
# Remill needs to be installed in the base _and_ deps stages, because they have
# different base images
COPY --from=store /dependencies/remill.deb .
RUN dpkg -i remill.deb

# Build-time dependencies go here
FROM trailofbits/cxx-common-vcpkg-builder-ubuntu:${UBUNTU_VERSION} as deps
ARG UBUNTU_VERSION
ARG ARCH
ARG LLVM_VERSION
ARG LIBRARIES

RUN apt-get update && \
    apt-get install -qqy xz-utils python3.8-venv make rpm && \
    rm -rf /var/lib/apt/lists/*

# Build dependencies
WORKDIR /dependencies

# cxx-common
ADD https://github.com/trailofbits/cxx-common/releases/latest/download/vcpkg_ubuntu-${UBUNTU_VERSION}_llvm-${LLVM_VERSION}_${ARCH}.tar.xz vcpkg_ubuntu-${UBUNTU_VERSION}_llvm-${LLVM_VERSION}_${ARCH}.tar.xz
RUN tar -xJf vcpkg_ubuntu-${UBUNTU_VERSION}_llvm-${LLVM_VERSION}_${ARCH}.tar.xz && \
    rm vcpkg_ubuntu-${UBUNTU_VERSION}_llvm-${LLVM_VERSION}_${ARCH}.tar.xz

# Remill again (see above in the base image where this is repeated)
COPY --from=store /dependencies/remill.deb .
RUN dpkg -i remill.deb

# Source code build
FROM deps AS build
WORKDIR /circuitous
ARG UBUNTU_VERSION
ARG ARCH
ARG LLVM_VERSION
ARG LIBRARIES

ENV VIRTUAL_ENV=/opt/trailofbits/venv
ENV PATH="${VIRTUAL_ENV}/bin:${PATH}"

# create a virtualenv in /opt/trailofbits/venv
RUN python3.8 -m venv ${VIRTUAL_ENV}

# Needed for sourcing venv
SHELL ["/bin/bash", "-c"]

COPY . ./

# Source venv, build circuitous, Install binaries & system packages
RUN source ${VIRTUAL_ENV}/bin/activate && \
    cmake -G Ninja -B build -S . \
        -Dremill_DIR:PATH=/usr/local/lib/cmake/remill \
        -DCMAKE_INSTALL_PREFIX:PATH="${LIBRARIES}" \
        -DCMAKE_VERBOSE_MAKEFILE=True \
        -DVCPKG_ROOT=/dependencies/vcpkg_ubuntu-${UBUNTU_VERSION}_llvm-${LLVM_VERSION}_${ARCH} \
        -DVCPKG_TARGET_TRIPLET=x64-linux-rel \
        && \
    cmake --build build --target install

FROM base AS dist
ARG LLVM_VERSION
ARG LIBRARIES
ENV PATH="/opt/trailofbits/bin:${PATH}" \
    LLVM_VERSION_NUM=${LLVM_VERSION} \
    LLVM_VERSION=llvm${LLVM_VERSION}

# Allow for mounting of local folder
WORKDIR /circuitous/local

COPY scripts/docker-decompile-json-entrypoint.sh /opt/trailofbits/docker-decompile-json-entrypoint.sh
COPY --from=build ${LIBRARIES} ${LIBRARIES}

ENTRYPOINT ["/opt/trailofbits/docker-decompile-entrypoint.sh"]