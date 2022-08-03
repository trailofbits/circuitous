/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Transforms/PassBase.hpp>

#include <eqsat/pattern/rule_set.hpp>

#include <span>

namespace circ::eqsat
{
    using RuleSet = ::eqsat::rule_set;

    CircuitPtr run_equality_saturation(CircuitPtr &&, std::span< RuleSet > rules);

} // namespace circ::eqsat
