/*
 * Copyright (c) 2022 Trail of Bits, Inc.
 */
#pragma once

#include <cstdint>
#include <deque>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <circuitous/Lifter/Context.hpp>
#include <circuitous/Util/InstructionBytes.hpp>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Util/Warnings.hpp>

#include <remill/Arch/Instruction.h>

namespace circ
{
    struct Decoder : has_ctx_ref
    {
      private:
        using maybe_inst_t = std::optional< remill::Instruction >;
        using has_ctx_ref::has_ctx_ref;
        using self_t = Decoder;

        std::vector< remill::Instruction > decode_all_(std::string_view buff);

        // Try to decode bytes in buff using arch from `ctx_ref`.
        auto decode(std::string_view buff) -> std::tuple< maybe_inst_t, std::string_view >;
      public:
        // Recursively try to decode everything present, call `process` for each decoded inst.
        std::vector< remill::Instruction > decode_all(llvm::StringRef buff);
        std::optional< remill::Instruction > decode(const InstBytes &bytes);
    };

} // namespace circ
