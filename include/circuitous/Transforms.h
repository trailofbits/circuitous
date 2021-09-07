/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Transforms/Passes.hpp>

namespace circ
{

  template< typename Logger >
  using DebugOptimizer = Passes< Defensive< Logger, WithHistory< PassesBase > > >;

  template< typename Logger >
  using DefaultOptimizer = Passes< Defensive< Logger, PassesBase > >;

}  // namespace circ
