/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include "UseDef.h"

namespace circuitous {

uint64_t User::gNextTimestamp = 0;

User::~User(void) {}
void User::Updated(uint64_t) {}

}  // namespace circuitous
