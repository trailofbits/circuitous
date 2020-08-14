// Copyright 2020, Trail of Bits. All rights reserved.

#pragma once

#include <cstdint>

namespace circuitous {

inline static uint64_t RotateRight64(uint64_t val, unsigned rot) {
#ifdef __has_builtin
#  if !__has_builtin(__builtin_rotateright64)
#    define HYDE_NEEDS_ROR64 1
#  else
#    define HYDE_NEEDS_ROR64 0
#  endif
#elif !defined(__clang__)
#  define HYDE_NEEDS_ROR64 1
#endif

#if HYDE_NEEDS_ROR64
  return (val >> rot) | (val << (64u - (rot % 64u)));
#else
  return __builtin_rotateright64(val, rot);
#endif
}

inline static unsigned CountLeadingZeroes64(uint64_t val) {
  if (!val) {
    return 64u;
  } else {
    return static_cast<unsigned>(__builtin_clzll(val));
  }
}

}  // namespace circuitous
