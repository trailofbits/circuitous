/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Transforms/Passes.hpp>

namespace circ
{

  using DebugOptimizer = Passes< Defensive< WithHistory< PassesBase > > >;
  using DefaultOptimizer = Passes< Defensive< PassesBase > >;

}  // namespace circ
