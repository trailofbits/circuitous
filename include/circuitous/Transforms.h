/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

namespace circuitous {

class Circuit;

// Look for uses of population count that operates on a zero-extended value, and
// change it to operate on the original value.
bool StrengthReducePopulationCount(Circuit *circuit);

// Look for uses of the population count instruction that look like they are
// actually computing the parity of some bits, and then replace that computation
// with a parity node.
bool ConvertPopCountToParity(Circuit *circuit);

// Look for common topological structures and extract them so that they are
// shared by multiple different expressions.
bool ExtractCommonTopologies(Circuit *circuit);

}  // namespace circuitous
