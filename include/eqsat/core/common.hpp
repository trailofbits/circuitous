/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <cstdint>

#include <gap/core/union_find.hpp>

namespace eqsat
{
    using node_id_t = gap::union_type;

    using bitwidth_t = std::uint32_t;

} // namespace eqsat
