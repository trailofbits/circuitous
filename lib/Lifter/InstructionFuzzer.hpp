/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <remill/Arch/Arch.h>
#include <remill/Arch/Instruction.h>

#include <circuitous/Lifter/Shadows.hpp>

#include <bitset>
#include <optional>
#include <string>
#include <vector>
#include <map>
#include <unordered_map>

/* This header tries to implement a simple "instruction fuzzing".
 *
 * When we encounter `mov rax, rbx` and `mov rcx, rdx` we would want
 * to treat them as one generic instruction in form `mov reg1, reg2`.
 * This is useful because it allows us make the generated cirucit smaller.
 *
 * We try not to relay on any arch specific information (to make this
 * component as re-usable as we can).
 * Core pipeline can be laid out as follows:
 *  - generate some permutations (generating all is quite expensive)
 *  - decode them using remill provided decoder for given arch, which
 *    yields remill::Instruction.
 *  - compare original instruction with decoded permutations.
 *  - keep track which bits are responsible for which operand:
 *    e.g. if original instruction was `add rax, rbx` and permuation of bit
 *    on position `x` decoded as `add rbx, rbx` we can assume that `x` is
 *    responsible for first operand.
 *    On the other hand if the permutation decoded as `xor rax, rbx` we know
 *    it does not impact any operand.
 *    There are a few cases which make ^ pretty complicated, but it does
 *    demonstrate the basic idea.
 *  - once we have all bits identified, we construct a "shadow" instruction
 *    to the original `remill::Instruction` keeps track of encoding bits for
 *    each operand (if they were determined).
 *  - for registers, we permutate all bits to get an "translation map" so we
 *    can later chose a correct reg given encoding.
 * There is some other terminology that is used:
 *  - `Husk` is an operand that does not have any identified bits, but we
 *    think it is not harcoded. Usually husks pop out when one "real" operand
 *    is represented as two in `remill::Instruction` (r/w).
 */

namespace circuitous {

namespace permutate {

  using permutations_t = std::vector<std::optional<remill::Instruction>>;

  // Generate simple permutations with bit flip.
  // E.g. if `0110 1100` is passed as input then this function generates
  // `1110 1100`
  // `0010 1100`
  // `0100 1100`
  // ...
  // `0110 1110`
  // `0110 1101`
  // It is possible some encodings are therefore missed, but generating all
  // permutations on some architecture is not a reasonable way to go, therefore
  // it is expected some part of the code down the line implements heuristic
  // that can deal with these misses.
  static inline permutations_t Flip(
    const remill::Instruction &rinst, const remill::Arch::ArchPtr &arch)
  {
    permutations_t out;
    out.resize(rinst.bytes.size() * 8);
    for (std::size_t i = 0; i < rinst.bytes.size(); ++i) {
      for (int j = 7; j >= 0; --j) {
        auto index = 8 * i + 7 - static_cast<uint64_t>(j);

        std::string flipped = rinst.bytes;
        auto byte = static_cast<uint8_t>(flipped[i]);
        uint8_t mask = 1;
        flipped[i] = static_cast<char>(byte ^ (mask << j));

        remill::Instruction tmp;
        if (arch->DecodeInstruction(0, flipped, tmp)) {
          out[index] = std::move(tmp);
        }
      }
    }
    return out;
  }

  struct OperandRef {
    using value_type = remill::Operand;
    using reference = remill::Operand &;
    using pointer = remill::Operand *;

    remill::Operand *op = nullptr;
    std::size_t idx = 0;

    pointer operator->() { return op; }
  };


  // Everything equals to everything
  struct TrueBase {
    using OpType = remill::Operand::Type;
    using operands_t = std::vector<const remill::Operand *>;

    static auto identity_imm() { return [](auto &, auto &) { return true; }; }
    static auto identity_reg() { return [](auto &, auto &) { return true; }; }

    static bool full_compare(const remill::Operand &lhs, const remill::Operand &rhs) {
      return lhs.Serialize() == rhs.Serialize();
    }

    static bool full_compare(const remill::Register *lhs, const remill::Register *rhs) {
      if (!lhs && !rhs) {
        return true;
      }
      if (!lhs || !rhs) {
        return false;
      }
      return lhs->name == rhs->name;
    }

    static bool Depends(const operands_t &) { return true; }
  };

  template<typename Next>
  struct UnitCompares : Next {
    using OpType = typename Next::OpType;

    static auto identity_imm() {
      return [](auto &self, auto &flipped) {
        // TODO(lukas): Check in remill that is_signed is always properly
        //              (and consistently) set.
        // This may seem counterintuitive, but two immediates are equal
        // even if their values are not equal -- actaully that is the whole
        // point.
        return flipped.type == remill::Operand::kTypeImmediate
              && self.size == flipped.size;
      };
    }

    static auto identity_reg() {
      return [](auto &self, auto &flipped) {
        return flipped.type == OpType::kTypeRegister &&
               self.reg.size == flipped.reg.size;
      };
    }

    static auto identity_addr() {
      return [](auto &self_, auto& flipped_) {
        if (flipped_.type != OpType::kTypeAddress) {
          return false;
        }
        auto &self = self_.addr;
        auto &flipped = flipped_.addr;
        uint8_t corrections = 0;

        if (self.segment_base_reg.name != flipped.segment_base_reg.name ||
            self.segment_base_reg.size != flipped.segment_base_reg.size) {
          return false;
        }

        if (self.index_reg.size != flipped.index_reg.size ||
            self.base_reg.size != flipped.base_reg.size) {
          return false;
        }

        // Base
        if (self.base_reg.size != flipped.base_reg.size ||
           (self.base_reg.name != flipped.base_reg.name)) {
          ++corrections;
        }

        // Index * scale (they must be checked together since they can be both missing)
        uint8_t index_corrections = 0;
        if (self.index_reg.size != flipped.index_reg.size ||
            (self.index_reg.name != flipped.index_reg.name)) {
          ++index_corrections;
        }
        if (self.scale != flipped.scale) {
          ++index_corrections;
        }
        if (self.index_reg.name.empty() || flipped.index_reg.name.empty()) {
          corrections += (index_corrections) ? 1 : 0;
        } else {
          corrections += index_corrections;
        }

        // Displacement
        if (self.displacement != flipped.displacement) {
          ++corrections;
        }
        return corrections <= 1;
      };
    }
  };

  template<typename Next>
  struct EqDependency : Next {
    using OpType = typename Next::OpType;

    static bool Depends(const std::vector<const remill::Operand *> &ops) {
      for (auto op : ops) {
        CHECK(op->type == OpType::kTypeRegister)
          << "Cannot check EqDependency on non-register operands.";
      }
      auto compare = [](auto a, auto b) {
        return a.name == b.name && a.size == b.size;
      };

      auto &fst = *(ops.begin());
      for (auto op : ops) {
        if (!compare(fst->reg, op->reg)) {
          return false;
        }
      }
      return true;
    }
  };

  template<typename Next>
  struct DependencyComparator : Next {
    using Item_t = std::map<std::size_t, const remill::Operand *>;
    using cri = const remill::Instruction &;

    template<typename Fn>
    static std::tuple<bool, bool> CheckStructure(
      cri original, cri permutation, const Item_t &items, Fn &&on_self)
    {
      if (original.bytes.size() != permutation.bytes.size()) {
        return { false, false };
      }

      if (original.function != permutation.function) {
        return { false, false };
      }

      // NOTE(lukas): Unfortunately we can have the following:
      // `64 44 xyz`
      // `64 64 44 xyz`
      // and if `44` (valid REX prefix) is changed to `64` (segment override)
      // this comparison will not notice it, as the segment override is always present
      // which is really unfortunate.
      if (!Next::full_compare(original.segment_override, permutation.segment_override)) {
        return { false, false };
      }

      if (original.operands.size() != permutation.operands.size()) {
        return { false, false };
      }

      bool exact_check = true;
      bool on_self_check = true;
      for (auto [i, op] : items) {
        exact_check &= Next::full_compare(*op, permutation.operands[i]);
        on_self_check &= on_self(*op, permutation.operands[i]);
      }

      if (!on_self_check) {
        return { false, exact_check };
      }

      // Check if there is the assumed relationship.
      // Often, it can be the same as the `exact_check`, but not always.
      std::vector<const remill::Operand *> raw_items;
      for (auto [_, op] : items) raw_items.push_back(op);
      if (!Next::Depends(raw_items)) {
        return { false, exact_check };
      }

      for (std::size_t i = 0U; i < permutation.operands.size(); ++i) {
        // Was already checked
        if (items.count(i)) {
          continue;
        }

        if (!Next::full_compare(original.operands[i], permutation.operands[i])) {
          return { false, exact_check };
        }
      }
      return { true, exact_check };
    }
  };

  // `exact_mod` allows to modify the behaviour of `Check` method if the operands of interest
  // are exactly the same in both instructions.
  // NOTE(lukas): See permutation generation and register enlarging heuristics for examples
  //              of when this was flag is used.
  template<typename Next, bool exact_mod = false>
  struct Dispatch : Next {
    using OpType = typename Next::OpType;
    using Item_t = typename Next::Item_t;

    static bool Compare(const remill::Instruction &original,
                        const remill::Instruction &permutation,
                        typename Item_t::key_type key,
                        const remill::Operand * val)
    {
      return Compare(original, permutation, { { key , val } });
    }

    static bool Compare(const remill::Instruction &original,
                        const remill::Instruction &permutation,
                        const Item_t &items)
    {
      CHECK(items.size() >= 1) << "Cannot compare " << items.size() << " items.";
      OpType type = items.begin()->second->type;
      for (auto [_, op] : items) {
        CHECK (op->type == type) << "Cannot compare as group operands of different types!";
      }
      switch(type) {
        case OpType::kTypeImmediate: return Check(original, permutation, items, Next::identity_imm());
        case OpType::kTypeRegister : return Check(original, permutation, items, Next::identity_reg());
        case OpType::kTypeAddress  : return Check(original, permutation, items, Next::identity_addr());
        default                    : return false;
      }
    }

    using cri = const remill::Instruction &;
    using citem_ref = const Item_t &;

    template<typename Fn>
    static bool Check(cri original, cri permutation, citem_ref op, Fn &&on_self) {
      auto [structural, exact] = Next::CheckStructure(original, permutation, op, on_self);
      return structural && (exact_mod || !exact);
    }
  };

  // Comparator stack handles the identification of relevant bits per Operand.
  // The layers allow customizations of smaller chunks so the bigger parts can
  // be reused.
  template<bool exact_mod>
  using RComparator = Dispatch<DependencyComparator<UnitCompares<TrueBase>>, exact_mod>;
  using Comparator = RComparator<false>;
  using HuskComparator = Dispatch<DependencyComparator<EqDependency<UnitCompares<TrueBase>>>>;
} // namespace permutate


struct InstructionFuzzer {
  // Declare return type
  using imm_meta_t = std::map<uint64_t, uint64_t>;
  using imm_meta_list_t = std::map<remill::Operand *, imm_meta_t>;

  // Import some remill types we will often neeed
  using Instruction = remill::Instruction;
  using Operand = remill::Operand;
  using Kind = remill::Operand::Address::Kind;
  using OpType = remill::Operand::Type;
  using Arch_ref = remill::Arch::ArchPtr;

  const remill::Arch::ArchPtr &arch;
  const remill::Instruction &rinst;

  permutate::permutations_t permutations;

  InstructionFuzzer(const Arch_ref &arch_, const remill::Instruction &rinst_)
    : arch(arch_), rinst(rinst_), permutations(permutate::Flip(rinst, arch))
  {}

  // TODO(lukas): Move this to remill.
  std::string ToString(remill::Operand::Type type) {
    switch(type) {
      case remill::Operand::kTypeInvalid : return "kTypeInvalid";
      case remill::Operand::kTypeRegister : return "kTypeRegister";
      case remill::Operand::kTypeShiftRegister : return "kTypeShiftRegister";
      case remill::Operand::kTypeImmediate : return "kTypeImmediate";
      case remill::Operand::kTypeAddress : return "kTypeAddress";
      case remill::Operand::kTypeExpression : return "kTypeExpression";
      case remill::Operand::kTypeRegisterExpression : return "kTypeRegisterExpression";
      case remill::Operand::kTypeImmediateExpression : return "kTypeImmediateExpression";
      case remill::Operand::kTypeAddressExpression : return "kTypeAddressExpression";
    }
  }

  // TODO(lukas): Move this to remill.
  std::string ToString(remill::Operand::Address::Kind &kind) {
    using ROAK = remill::Operand::Address::Kind;
    switch(kind) {
      case ROAK::kInvalid : return "kInvalid";
      case ROAK::kMemoryWrite : return "kMemoryWrite";
      case ROAK::kMemoryRead : return "kMemoryRead";
      case ROAK::kAddressCalculation : return "kAddressCalculation";
      case ROAK::kControlFlowTarget : return "kControlFlowTarget";
    }
  }

  void DistributeAddr(const std::vector<bool> &bits,
                      std::size_t idx,
                      shadowinst::Address &s_addr)
  {
    std::vector<std::vector<bool>> distributed_bits = {
      4, std::vector<bool>(rinst.bytes.size() * 8, false)
    };

    for (std::size_t i = 0; i < bits.size(); ++i) {
      if (!bits[i] || !permutations[i]
          || permutations[i]->operands.size() != rinst.operands.size()) {
        continue;
      }

      auto &self = rinst.operands[idx].addr;
      CHECK(permutations[i]->operands[idx].type == OpType::kTypeAddress);
      auto &flipped = permutations[i]->operands[idx].addr;

      if (self.base_reg.name != flipped.base_reg.name) {
        distributed_bits[0][i] = true;
      }

      if (self.index_reg.name != flipped.index_reg.name) {
        distributed_bits[1][i] = true;
      }

      if (self.scale != flipped.scale && !flipped.index_reg.name.empty()) {
        distributed_bits[2][i] = true;
      }

      if (self.displacement != flipped.displacement) {
        distributed_bits[3][i] = true;
      }
    }

    for (auto &bits : distributed_bits) {
      std::reverse(bits.begin(), bits.end());
    }

    s_addr.base_reg = std::make_optional<shadowinst::Reg>(std::move(distributed_bits[0]));
    s_addr.index_reg = std::make_optional<shadowinst::Reg>(std::move(distributed_bits[1]));
    s_addr.scale = std::make_optional<shadowinst::Immediate>(std::move(distributed_bits[2]));
    s_addr.displacement = std::make_optional<shadowinst::Immediate>(std::move(distributed_bits[3]));
  }

  auto FuzzOps() {
    shadowinst::Instruction shadow_inst;
    std::vector<std::vector<bool>> op_bits =
      { rinst.operands.size(), std::vector<bool>(rinst.bytes.size() * 8, false) };

    for (std::size_t i = 0; i < permutations.size(); ++i) {
      for (std::size_t op_i = 0; op_i < rinst.operands.size(); ++op_i) {
        if (!permutations[i]) {
          continue;
        }
        using C = permutate::Comparator;
        op_bits[op_i][i] = C::Compare(rinst,
                                      *permutations[i],
                                      { { op_i, &rinst.operands[op_i] } });
      }
    }

    for (std::size_t i = 0; i < rinst.operands.size(); ++i) {
      auto &r_op = rinst.operands[i];
      switch(r_op.type) {
        case OpType::kTypeRegister : {
          std::reverse(op_bits[i].begin(), op_bits[i].end());
          shadow_inst.Add<shadowinst::Reg>(op_bits[i]);
          break;
        }
        case OpType::kTypeImmediate : {
          std::reverse(op_bits[i].begin(), op_bits[i].end());
          shadow_inst.Add<shadowinst::Immediate>(op_bits[i]);
          break;
        }
        case OpType::kTypeAddress: {
          auto &s_addr = shadow_inst.Add<shadowinst::Address>();
          DistributeAddr(op_bits[i], i, *s_addr.address);
        }
        default:
          shadow_inst.operands.emplace_back();
          break;
      }
    }

    ResolveHusks(shadow_inst);
    PopulateTranslationTables(shadow_inst);
    LOG(INFO) << shadow_inst.to_string();
    return shadow_inst;
  }

  void PopulateTranslationTables(shadowinst::Instruction &s_inst) {
    std::size_t idx = 0;
    for (auto &op : s_inst.operands) {

      using maybe_reg_t = std::optional<std::tuple<remill::Operand::Register *, std::size_t>>;
      auto get_reg = [&](auto &from) -> maybe_reg_t {
        CHECK(from.operands[idx].type == OpType::kTypeRegister);
        return { { &from.operands[idx].reg, idx } };
      };
      auto get_base = [&](auto &from) -> maybe_reg_t {
        CHECK(from.operands[idx].type == OpType::kTypeAddress);
        return { { &from.operands[idx].addr.base_reg, idx } };
      };
      auto get_index = [&](auto &from) -> maybe_reg_t {
        CHECK(from.operands[idx].type == OpType::kTypeAddress);
        return { { &from.operands[idx].addr.index_reg, idx } };
      };

      if (op.reg && rinst.operands[idx].type == OpType::kTypeRegister) {
        PopulateTranslationTable(*op.reg, get_reg);
      }

      if (op.address && rinst.operands[idx].type == OpType::kTypeAddress) {
        if (op.address->base_reg) {
          PopulateTranslationTable(*op.address->base_reg, get_base);
        }
        if (op.address->index_reg) {
          PopulateTranslationTable(*op.address->index_reg, get_index);
        }
      }
      ++idx;
    }
  }

  using rops_map_t = std::map<std::size_t, const remill::Operand *>;
  using rops_maps_t = std::vector<rops_map_t>;

  // TODO(lukas): This is heurisitc. Later we are most likely going to need
  //              support multiple strategies. Also the selected heuristic
  //              implies the permutate::Comparator stack configuration.
  rops_maps_t GroupHusks(const rops_map_t &ops) {
    // TODO(lukas): First we need to sort by type
    using entry_t = std::pair<std::size_t, const remill::Operand *>;
    using OPT = remill::Operand::Type;

    //Sorts<remill::Operand::Type, entry_t> by_type;
    std::unordered_map<OPT, rops_map_t> by_type;
    for (auto &x : ops) {
      by_type[x.second->type].emplace(x.first, x.second);
    }

    auto str_hash = [](auto op) -> std::string {
      switch(op->type) {
        case OPT::kTypeImmediate: return std::to_string(op->imm.val);
        case OPT::kTypeRegister: return op->reg.name + std::to_string(op->size);
        default: LOG(FATAL) << "Cannot hash operand";
      }
    };

    std::unordered_map<std::string, rops_map_t> by_hash;
    for (auto &[_, g_by_type] : by_type) {
      for (auto &[idx, op] : g_by_type) {
        by_hash[str_hash(op)].emplace(idx, op);
      }
    }

    rops_maps_t out;
    for (auto &[_, maps] : by_hash) {
      out.push_back(std::move(maps));
    }
    return out;
  }

  void ResolveHusks(shadowinst::Instruction &s_inst) {
    std::map<std::size_t, shadowinst::Operand *> husks;
    std::map<std::size_t, const remill::Operand *> r_husks;

    // TODO(lukas): Filter husks
    for (std::size_t i = 0; i < rinst.operands.size(); ++i) {
      if (s_inst[i].IsHusk()) {
        husks[i] = &s_inst[i];
        r_husks[i] = &rinst.operands[i];
      }
    }

    // Cannot work with only one husks nor with none
    if (husks.size() <= 1) {
      return;
    }

    for (auto &group : GroupHusks(r_husks)) {
      // There is no point working with smaller than 2 group
      if (group.size() <= 1) {
        continue;
      }
      std::vector<bool> husk_bits(rinst.bytes.size() * 8, false);
      for (std::size_t i = 0; i < permutations.size(); ++i) {
        if (!permutations[i]) {
          continue;
        }
        husk_bits[i] = permutate::HuskComparator::Compare(rinst, *permutations[i], group);
      }
      std::reverse(husk_bits.begin(), husk_bits.end());
      for (auto &[i, op] : group) {
        s_inst.Replace(i, op->type, husk_bits);
      }
    }
  }

  template<typename Getter>
  bool RegEnlargementHeuristic(shadowinst::Reg &s_reg, Getter get_reg) {
    auto [from, size] = s_reg.biggest_chunk();
    // 3 bits are okay since we do not always need REX prefix.
    // 4 and more bits are most likely an error in decoding and we definitely
    // do not want to add even more.
    if (size == 3 || s_reg.region_bitsize() >= 4) {
      return false;
    }

    std::vector<shadowinst::Reg> candidates;

    // We have two defined bits (D) and need to add third
    if (size == 2) {
      // _DD_ -> DDD_
      if (from != 0 && !s_reg.regions.count(from - 1)) {
        shadowinst::Reg copy{s_reg};
        copy.regions.erase(from);
        copy.regions[from - 1] = size + 1;
        candidates.push_back(std::move(copy));
      }
      // _DD_ -> _DDD
      if (from + size < rinst.bytes.size() * 8 && !s_reg.regions.count(from + size)) {
        shadowinst::Reg copy{s_reg};
        copy.regions[from] = size + 1;
        candidates.push_back(std::move(copy));
      }
    }
    // Everything has size 1, therefore we search for "a hole": __D_D__ -> __DDD__
    // TODO(lukas): Right now we do not try all cases in D_D_D as we expect there to
    //              be only one reasonable candidate.
    if (size == 1) {
      auto hole = s_reg.get_hole();
      if (!hole) {
        return false;
      }
      shadowinst::Reg copy{s_reg};
      copy.regions.erase(*hole + 1);
      copy.regions[*hole - 1] = 3ul;
      candidates.push_back(std::move(copy));
    }
    if (candidates.empty()) {
      return false;
    }

    // We also need to check that the newly added combination satisfies the original
    // selection criterium.
    // NOTE(lukas): While this works (and overall is a good idea), the implementation
    //              is a bit spaghetti like and would probably deserve some refactor.
    auto wget_reg = [&](auto &from) {
      auto raw = get_reg(from);
      if (!raw || !std::get<0>(*raw)) {
        return raw;
      }
      auto [reg, idx] = *raw;
      if (permutate::RComparator<true>::Compare(rinst, from, idx, &from.operands[idx])) {
        return raw;
      }
      // Return empty optional of correct type
      return decltype(get_reg(from)){};
    };

    // Checks if the newly populated `shadowinst::Reg` has correct operands.
    auto is_valid = [](auto &what) {
      for (auto &[_, mats] : what.translation_map) {
        if (mats.size() > 1) {
          return false;
        }
      }
      return true;
    };

    PopulateTranslationTable_(s_reg, wget_reg);
    std::optional<shadowinst::Reg> chosen;
    for (auto &c : candidates) {
      PopulateTranslationTable_(c, wget_reg);
      // We want to select more bits only if they led to more decoded reg operands
      // and their values satisfy our conditions.
      if (c.translation_map.size() > s_reg.translation_map.size() && is_valid(c)) {
        if (chosen) {
          LOG(FATAL) << "Reg enlargement heuristic has chosen multiple candidates!";
        }
        chosen = std::move(c);
      }
    }
    if (!chosen) {
      LOG(FATAL) << "Reg enlargement heuristic did not choose aby candidate!";
    }
    s_reg = std::move(*chosen);
    return true;
  }

  template<typename Getter>
  void PopulateTranslationTable(shadowinst::Reg &shadow_reg, Getter &&get_reg)
  {
    if (RegEnlargementHeuristic(shadow_reg, get_reg)) {
      return;
    }
    return PopulateTranslationTable_(shadow_reg, std::forward<Getter>(get_reg));
  }

  // `Getter` takes an remill::Operand and returns the register we to compare against.
  // This is so we can get nested registers as well (for example in `Address`)
  template<typename Getter>
  void PopulateTranslationTable_(shadowinst::Reg &shadow_reg, Getter &&get_reg)
  {
    std::vector<uint64_t> idxs;
    for (auto &[from, size] : shadow_reg.regions) {
      auto afrom = rinst.bytes.size() * 8 - from - size;
      for (uint64_t i = 0; i < size; ++i) {
        idxs.push_back(afrom);
        ++afrom;
      }
    }

    // The callback that is called on permutation
    auto yield = [&](auto bytes) {
      // First decode permutation and check if it is valid
      remill::Instruction tmp;
      if (!arch->DecodeInstruction(0, bytes, tmp)) {
        LOG(WARNING) << "Was not able to decode guaranteed permutation\n"
                     << shadowinst::to_binary(bytes);
        return;
      }
      if (tmp.function != rinst.function) {
        LOG(WARNING) << "Guaranteed permutation generated different instruction.";
        return;
      }
      if (tmp.bytes.size() != rinst.bytes.size()) {
        LOG(WARNING) << "Guarantedd permutation resulted in inst with less used bytes.";
        return;
      }

      if (tmp.operands.size() != rinst.operands.size()) {
        LOG(WARNING) << "Permutation has different number of operands.";
        LOG(WARNING) << tmp.function;
        LOG(WARNING) << rinst.function;
        return;
      }
      // `tmp` hosts a valid instruction, therefore we now want to retrieve
      // the register operand we are fuzzing.
      // `get_reg` is allowed to perform more logic than simple retrieval, that's
      // why the return type is wrapped in `std::optional` -> if the value is not
      // present we do not want to work with this operand.
      auto maybe_reg = get_reg(tmp);
      if (!maybe_reg) {
        LOG(INFO) << "get_reg returned no value";
        return;
      }
      CHECK(std::get<0>(*maybe_reg));
      const auto &reg = *(std::get<0>(*maybe_reg));

      std::vector<bool> out;
      std::stringstream ss;

      // Generate the value
      for (auto idx : idxs) {
        auto byte = static_cast<uint8_t>(bytes[idx / 8]);
        out.push_back((byte >> (7 - (idx % 8))) & 1u);
        ss << out.back();
      }

      // Register name is empty -- therefore it is most likely not a decoded register
      if (reg.name.empty()) {
        return;
      }
      shadow_reg.translation_map[reg.name].insert(std::move(out));
    };

    std::string copied_bytes = rinst.bytes;
    _Permutate(copied_bytes, idxs, 0, yield);
  }

  template<typename Yield>
  void _Permutate(std::string &bytes, const std::vector<uint64_t> &idxs,
                  std::size_t current, Yield &yield) {
    // I am done flipping, we look at the value now
    if (current == idxs.size()) {
      yield(bytes);
      return;
    }

    // We chose not to flip first
    _Permutate(bytes, idxs, current + 1, yield);
    auto byte = static_cast<uint8_t>(bytes[idxs[current] / 8 ]);
    uint8_t mask = 1;
    bytes[idxs[current] / 8] = static_cast< char >(byte ^ (mask << (7 - idxs[current] % 8)));
    _Permutate(bytes, idxs, current + 1, yield);
    // We are taking bytes as reference we need to flip back as we backtrack!
    byte = static_cast<uint8_t>(bytes[idxs[current] / 8 ]);
    bytes[idxs[current] / 8] = static_cast< char >(byte ^ (mask << (7 - idxs[current] % 8)));
  }

  void DecodeRegions(Instruction *inst) {
    LOG(FATAL) << "InstBitParser cannot use decoder yet";
  }
};


} // namespace circuitous