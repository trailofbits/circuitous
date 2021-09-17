/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#define CIRCUITOUS_RELAX_WARNINGS \
  _Pragma( "clang diagnostic push" ) \
  _Pragma( "clang diagnostic ignored \"-Wsign-conversion\"" ) \
  _Pragma( "clang diagnostic ignored \"-Wconversion\"" )

#define CIRCUITOUS_UNRELAX_WARNINGS \
  _Pragma( "clang diagnostic pop" ) \

CIRCUITOUS_RELAX_WARNINGS
#include <gflags/gflags.h>
#include <glog/logging.h>
CIRCUITOUS_UNRELAX_WARNINGS
