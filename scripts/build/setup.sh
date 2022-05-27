#!/bin/sh

export VCPKG_ROOT=./_external/vcpkg
export CXX_COMMON_ROOT=./_external/cxx-common

mkdir -p ${VCPKG_ROOT} ${CXX_COMMON_ROOT}

# download vcpkg
git clone git@github.com:microsoft/vcpkg.git ${VCPKG_ROOT}

${VCPKG_ROOT}/bootstrap-vcpkg.sh

# download cxx-common
git clone --branch port-files git@github.com:lifting-bits/cxx-common.git ${CXX_COMMON_ROOT}

# install python requirements
pip install --no-cache-dir -r requirements.txt
