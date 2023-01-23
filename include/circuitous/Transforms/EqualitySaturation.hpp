/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Transforms/PassBase.hpp>

#include <eqsat/pattern/rule_set.hpp>

#include <span>

namespace circ
{
    circuit_owner_t run_equality_saturation(circuit_owner_t &&, std::span< eqsat::rule_set > rules);

} // namespace circ
