/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Transforms/Passes.hpp>

#include <circuitous/Transforms/eqsat/rule_set.hpp>

#include <span>

namespace circ {

  using RuleSet = eqsat::rule_set;

  CircuitPtr EqualitySaturation(CircuitPtr &&, std::span<RuleSet> rules);


} // namespace circ
