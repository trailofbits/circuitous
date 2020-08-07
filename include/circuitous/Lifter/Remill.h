/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/IR/IR.h>

#include <string>

namespace circuitous {

std::unique_ptr<Circuit> LiftInstructionsInFile(const std::string &arch_name,
                                                const std::string &os_name,
                                                const std::string &file_name);

}  // namespace circuitous
