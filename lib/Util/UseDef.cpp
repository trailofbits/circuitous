/*
 * Copyright (c) 2020 Trail of Bits, Inc.
 */

#include <circuitous/Util/UseDef.h>

namespace circuitous {

uint64_t User::gNextTimestamp = 0;

User::~User(void) {}
void User::Update(uint64_t) {}

}  // namespace circuitous
