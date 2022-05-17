/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#define CIRCUITOUS_RELAX_WARNINGS \
  _Pragma( "clang diagnostic push" ) \
  _Pragma( "clang diagnostic ignored \"-Wsign-conversion\"" ) \
  _Pragma( "clang diagnostic ignored \"-Wconversion\"" ) \
  _Pragma( "clang diagnostic ignored \"-Wold-style-cast\"" ) \
  _Pragma( "clang diagnostic ignored \"-Wnon-virtual-dtor\"" )

#define CIRCUITOUS_UNRELAX_WARNINGS \
  _Pragma( "clang diagnostic pop" ) \
