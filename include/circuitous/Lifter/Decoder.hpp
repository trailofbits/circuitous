/*
 * Copyright (c) 2021 Trail of Bits, Inc.
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
#include <circuitous/Util/LLVMUtil.hpp>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Util/Logging.hpp>

#include <remill/Arch/Arch.h>
#include <remill/BC/IntrinsicTable.h>
#include <remill/BC/Lifter.h>
#include <remill/BC/Optimizer.h>
#include <remill/BC/Util.h>

CIRCUITOUS_RELAX_WARNINGS
#include <llvm/IR/Module.h>
CIRCUITOUS_UNRELAX_WARNINGS

namespace circ
{
    struct Decoder : has_ctx_ref
    {
      private:
        using maybe_inst_t = std::optional< remill::Instruction >;
        using has_ctx_ref::has_ctx_ref;
        using self_t = Decoder;

        std::vector< InstructionSelection > grouped_insts;
        std::set< std::string > inst_bytes;
        std::unordered_map< std::string, size_t > isel_index;

        void decode_all_(std::string_view buff);

        // Try to decode bytes in buff using arch from `ctx_ref`.
        auto decode(std::string_view buff) -> std::tuple< maybe_inst_t, std::string_view >;

        void process(remill::Instruction inst);
      public:
        // Recursively try to decode everything present, call `process` for each decoded inst.
        self_t &decode_all(llvm::StringRef buff);

        // Take all decoded instructions.
        auto take() { return std::move( grouped_insts ); }
    };
} // namespace circ
