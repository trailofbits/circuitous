/*
 * Copyright (c) 2020-2022 Trail of Bits, Inc.
 */

#pragma once

#include <filesystem>
#include <memory>

namespace circ
{
    struct Circuit;
    using circuit_ptr_t = std::unique_ptr< Circuit >;


    // Serialize using simple custom binary format
    void serialize(std::filesystem::path filename, Circuit *circuit);

    // Deserialize from the simple custom binary format.
    circuit_ptr_t deserialize(std::filesystem::path filename);

} // namespace circ
