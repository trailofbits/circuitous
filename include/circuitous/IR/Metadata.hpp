/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <string>
#include <optional>
#include <vector>
#include <unordered_set>
#include <unordered_map>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Util/LLVMUtil.hpp>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#pragma clang diagnostic pop

namespace circ {

  template< typename Key, typename Val >
  struct HasMetadata {
    using key_t = Key;
    using value_t = Val;
    using maybe_value_t = std::optional< value_t >;

    std::unordered_map< key_t, value_t > meta;

    auto meta_size() const { return meta.size(); }
    bool has_meta(const key_t &key) const { return meta.count(key); }

    maybe_value_t get_meta(const key_t &key) const {
      return (has_meta(key)) ? std::make_optional(meta.find(key)->second) : std::nullopt;
    }

    template< bool rewrite = false >
    void set_meta(key_t key, value_t val) {
      if constexpr (!rewrite) {
        check(!has_meta(key));
      }
      meta[std::move(key)] = std::move(val);
    }

    std::string dump_meta() const {
      std::stringstream ss;
      auto format = [&](const auto &key, const auto &val) {
        ss << "[ " << key << " ] -> " << val << std::endl;
      };
      for (const auto &[key, val] : meta) {
        format(key, val);
      }
      return ss.str();
    }
  };

  struct circir_llvm_meta {
    static inline const std::string lifted_bytes = "__circir.lifted_bytes";
    static inline const std::string llvm_inst = "__circir.llvm_inst";
    static inline const std::string llvm_source_dump = "__circir.llvm_source_dump";

    // TODO(lukas): Replace with std::vector if there is only a small
    //              number of entries.
    static inline const std::unordered_set< std::string > all = {
      lifted_bytes,
      llvm_inst,
      llvm_source_dump,
    };
  };

  static inline void annotate_llvm(llvm::Instruction *inst, auto key, auto value) {
    check(circir_llvm_meta::all.count(key));
    AddMetadata(inst, std::move(key), std::move(value));
  }

  static inline auto parse_llvm(llvm::Instruction *inst) {
    std::vector< std::tuple< std::string, std::string > > out;

    for (const auto &kind : circir_llvm_meta::all) {
      if (auto node = inst->getMetadata(kind)) {
        check(node->getNumOperands() == 1);
        auto op = llvm::dyn_cast<llvm::MDString>(node->getOperand(0));
        out.emplace_back(kind, op->getString().str());
      }
    }
    return out;
  }

  template<typename Op>
  void populate_meta(llvm::Instruction *inst, Op *op) {
    for (auto [key, value] : parse_llvm(inst)) {
      op->set_meta(std::move(key), std::move(value));
    }
  }

  template<typename LLVM, typename Op>
  auto populate_meta(LLVM *val, Op *op) {
    if (auto as_inst = llvm::dyn_cast< llvm::Instruction >(val))
      populate_meta(as_inst, op);
  }

  using HasStringMeta = HasMetadata< std::string, std::string >;

} // namespace circ
