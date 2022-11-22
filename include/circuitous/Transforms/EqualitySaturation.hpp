/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Transforms/PassBase.hpp>

#include <eqsat/pattern/rule_set.hpp>

#include <span>

namespace circ
{
    CircuitPtr run_equality_saturation(CircuitPtr &&, std::span< eqsat::rule_set > rules);

} // namespace circ
