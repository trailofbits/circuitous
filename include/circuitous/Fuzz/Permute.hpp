/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <remill/Arch/Arch.h>
#include <remill/Arch/Instruction.h>

namespace circ::ifuzz::permutate {

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
  static inline permutations_t flip(
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
          LOG(INFO) << tmp.Serialize();
          out[index] = std::move(tmp);
        }
      }
    }
    return out;
  }

  struct Verbose {
    std::stringstream ss;

    template<typename S>
    void _raw(S &&s) { ss << std::forward<S>(s); }

    void _raw(const remill::Instruction &inst) {
      ss << inst.Serialize();
    }

    template<typename H, typename ...Args>
    void _log(const std::string &delim, H &&h, Args &&...args) {
      _raw(std::forward<H>(h));
      if constexpr (sizeof...(Args) != 0) {
        ss << delim;
        return _log(delim, std::forward<Args>(args)...);
      } else {
        ss << std::endl;
      }
    }

    template<typename ...Args>
    void dbg(Args &&...args) {
      return _log(" ", std::forward<Args>(args)...);
    }

    template<typename ...Args>
    void dbg_neq(uint8_t indent, Args &&...args) {
      return _log(std::string(indent * 2, ' '), std::forward<Args>(args)...);
    }
  };

  // TODO(lukas): Actually this is a few separate components in one template stack.
  //              It would be best if components could be identified and refactored.
  // Everything equals to everything
  struct TrueBase : Verbose {
    using OpType = remill::Operand::Type;
    using operands_t = std::vector<const remill::Operand *>;

    const remill::Arch *arch;

    TrueBase(const remill::Arch *arch_) : arch(arch_) {}

    auto identity_imm() { return [](auto &, auto &) { return true; }; }
    auto identity_reg() { return [](auto &, auto &) { return true; }; }

    bool full_compare(const remill::Operand &lhs, const remill::Operand &rhs) {
      return lhs.Serialize() == rhs.Serialize();
    }

    bool full_compare(const remill::Register *lhs, const remill::Register *rhs) {
      if (!lhs && !rhs) {
        return true;
      }
      if (!lhs || !rhs) {
        return false;
      }
      return lhs->name == rhs->name;
    }

    bool Depends(const operands_t &) { return true; }
  };

  template<typename Next>
  struct UnitCompares : Next {
    using OpType = typename Next::OpType;
    using Next::Next;

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

    auto identity_reg() {
      return [&](auto &self, auto &flipped) {
        return flipped.type == OpType::kTypeRegister &&
               self.reg.size == flipped.reg.size;
      };
    }

    auto identity_addr() {
      return [&](auto &self_, auto& flipped_) {
        if (flipped_.type != OpType::kTypeAddress) {
          return false;
        }
        auto &self = self_.addr;
        auto &flipped = flipped_.addr;
        uint8_t corrections = 0;

        if (self.segment_base_reg.name != flipped.segment_base_reg.name ||
            self.segment_base_reg.size != flipped.segment_base_reg.size) {
          this->dbg_neq(2, "Segment base failure.");
          return false;
        }

        if (self.index_reg.size != flipped.index_reg.size ||
            self.base_reg.size != flipped.base_reg.size) {
          return false;
        }

        // RSP in base reg requires some bits set elsewhere.
        if ((self.base_reg.name != flipped.base_reg.name) &&
            (flipped.base_reg.name == "NEXT_PC")) {
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
        this->dbg("Corrections needed:", static_cast<uint32_t>(corrections));
        return corrections <= 1;
      };
    }
  };

  template<typename Next>
  struct EqDep_ : Next {
    using Next::Next;

    template<typename R>
    bool reg_dependency(const R &a, const R &b) {
      return a.name == b.name && a.size == b.size;
    }
    template<typename A>
    bool addr_dependency(const A &a, const A &b) {
      return reg_dependency(a.base_reg, b.base_reg)
             && reg_dependency(a.index_reg, b.index_reg)
             && a.scale == b.scale
             && a.displacement == b.displacement;
    }
  };

  template< typename Next >
  struct EqDepAmd64Hack_ : Next {

    using Next::Next;

    template<typename R>
    bool reg_dependency(const R &a, const R &b) {
      // Since this class represents a special case, general condition is still
      // enough.
      auto a_reg = try_enclosing_reg(this->arch, a.name);
      auto b_reg = try_enclosing_reg(this->arch, b.name);
      if (!a_reg || !b_reg)
        return this->Next::reg_dependency(a, b);
      return this->Next::reg_dependency(a, b) ||
             ((a.size + b.size == 64 + 32) && ((*a_reg)->name == (*b_reg)->name));
    }
  };

  template< typename Next >
  struct EqDepFront : Next {
    using OpType = typename Next::OpType;
    using Next::Next;

    bool Depends(const std::vector<const remill::Operand *> &ops) {
      for (auto op : ops) {
        CHECK(op->type == OpType::kTypeRegister || op->type == OpType::kTypeAddress)
          << "Can check eq dependency only on reg and addr operands.";
      }
      auto compare = [=](auto a, auto b) {
        switch (a->type) {
          case OpType::kTypeRegister: return this->Next::reg_dependency(a->reg, b->reg);
          case OpType::kTypeAddress: return this->Next::addr_dependency(a->addr, b->addr);
          default: LOG(FATAL) << "Unreachable, should be caught be ealier assert";
        }
      };

      auto &fst = *(ops.begin());
      for (auto op : ops) {
        if (!compare(fst, op)) {
          return false;
        }
      }
      return true;
    }
  };

  template< typename N >
  using EqDep = EqDepFront< EqDep_< N > >;
  template< typename N >
  using EqDepAmd64Hack = EqDepFront< EqDepAmd64Hack_< EqDep_< N > > >;

  template<typename Next>
  struct DependencyComparator : Next {
    using Item_t = std::map<std::size_t, const remill::Operand *>;
    using cri = const remill::Instruction &;

    using Next::Next;

    template<typename Fn>
    std::tuple<bool, bool> CheckStructure(
      cri original, cri permutation, const Item_t &items, Fn &&on_self)
    {
      this->dbg("Checking structure of:", permutation);
      if (original.bytes.size() != permutation.bytes.size()) {
        this->dbg_neq(1, "Sizes did not match.");
        return { false, false };
      }

      if (original.function != permutation.function) {
        this->dbg_neq(1, "Function did not match.");
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
        this->dbg_neq(1, "Operands size did not match.");
        return { false, false };
      }

      bool exact_check = true;
      bool on_self_check = true;
      for (auto [i, op] : items) {
        exact_check &= Next::full_compare(*op, permutation.operands[i]);
        on_self_check &= on_self(*op, permutation.operands[i]);
      }

      if (!on_self_check) {
        this->dbg_neq(1, "on self_check failed.");
        return { false, exact_check };
      }

      // Check if there is the assumed relationship.
      // Often, it can be the same as the `exact_check`, but not always.
      std::vector<const remill::Operand *> raw_items;
      for (auto [_, op] : items) raw_items.push_back(op);
      if (!Next::Depends(raw_items)) {
        this->dbg_neq(1, "Next::Depends failed.");
        return { false, exact_check };
      }

      for (std::size_t i = 0U; i < permutation.operands.size(); ++i) {
        // Was already checked
        if (items.count(i)) {
          continue;
        }

        if (!Next::full_compare(original.operands[i], permutation.operands[i])) {
          this->dbg_neq(1, "full_compare for operand", i, "failed.");
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
  template< typename Next, bool exact_mod = false >
  struct Dispatch : Next {
    using OpType = typename Next::OpType;
    using Item_t = typename Next::Item_t;

    using cri = const remill::Instruction;

    Dispatch(const remill::Arch *arch_) : Next(arch_) {}

    bool compare(cri &original, cri &permuation, std::size_t idx) {
      return compare(original, permuation, {{ idx, &original.operands[idx] }});
    }

    bool compare(cri &original, cri &permutation, const Item_t &items)
    {
      CHECK(items.size() >= 1) << "Cannot compare " << items.size() << " items.";
      OpType type = items.begin()->second->type;
      for (auto [_, op] : items) {
        CHECK (op->type == type) << "Cannot compare as group operands of different types!";
      }
      auto check = [&](auto identity) { return Check(original, permutation, items, identity); };

      switch(type) {
        case OpType::kTypeImmediate: return check(Next::identity_imm());
        case OpType::kTypeRegister : return check(Next::identity_reg());
        case OpType::kTypeAddress  : return check(Next::identity_addr());
        default                    : return false;
      }
    }

  template< typename ... Args >
   bool verbose_compare( Args &&...args ) {
      auto x = compare(std::forward< Args >(args) ...);
      LOG(INFO) << this->ss.str();
      return x;
    }

    using citem = const Item_t;

    template<typename Fn>
    bool Check(cri &original, cri &permutation, citem &op, Fn &&on_self) {
      auto [structural, exact] = Next::CheckStructure(original, permutation, op, on_self);
      return structural && (exact_mod || !exact);
    }
  };

  // Comparator stack handles the identification of relevant bits per Operand.
  // The layers allow customizations of smaller chunks so the bigger parts can
  // be reused.
  using base_t = UnitCompares< TrueBase >;
  template< typename N >
  using front_t = Dispatch< DependencyComparator< N > >;

  template<bool exact_mod>
  using RComparator = Dispatch< DependencyComparator< base_t >, exact_mod>;

  using Comparator = RComparator<false>;
  using HuskComparator = front_t< EqDep< base_t > >;
  using HuskComparatorAmd64Hack = front_t< EqDepAmd64Hack< base_t > >;
  using HuskEnlargerComparator = Dispatch< DependencyComparator< EqDep< base_t > >, true >;

} // circ::ifuzz::permutate
