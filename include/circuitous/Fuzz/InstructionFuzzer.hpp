/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <remill/Arch/Arch.h>
#include <remill/Arch/Instruction.h>

#include <circuitous/Lifter/Shadows.hpp>
#include <circuitous/Fuzz/InstNavigation.hpp>
#include <circuitous/Fuzz/Permute.hpp>
#include <circuitous/Fuzz/Husks.hpp>

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

namespace circ {

  struct InstructionFuzzer {
    // Import some remill types we will often neeed
    using Instruction = remill::Instruction;
    using Operand = remill::Operand;
    using Kind = remill::Operand::Address::Kind;
    using OpType = remill::Operand::Type;
    using Arch_ref = remill::Arch::ArchPtr;

    using bits_t = std::vector< bool >;

    const remill::Arch::ArchPtr &arch;
    const remill::Instruction &rinst;

    ifuzz::permutate::permutations_t permutations;

    InstructionFuzzer(const Arch_ref &arch_, const remill::Instruction &rinst_)
      : arch(arch_), rinst(rinst_), permutations(ifuzz::permutate::flip(rinst, arch))
    {}

    // We know that `bits` are used to encode `s_addr`. We need to
    // determine the relationship between these bits and attributes
    // of Address operand.
    void distribute_addr(const std::vector<bool> &bits,
                        std::size_t idx,
                        shadowinst::Address &s_addr)
    {
      std::vector<bits_t> distributed_bits( 4, bits_t(rinst.bytes.size() * 8, false) );

      for (std::size_t i = 0; i < bits.size(); ++i) {
        if (!bits[i]
            || !permutations[i]
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

      auto make = [](auto &where, const auto& bits_) {
        using raw_t = std::decay_t< std::remove_reference_t< decltype(where) > >;
        using T = typename raw_t::value_type;
        where = std::make_optional< T >(shadowinst::ordered_bits_t(bits_));
      };

      make(s_addr.base_reg, distributed_bits[0]);
      make(s_addr.index_reg, distributed_bits[1]);
      make(s_addr.scale, distributed_bits[2]);
      make(s_addr.displacement, distributed_bits[3]);

    }

    auto empty_bits() const { return bits_t(rinst.bytes.size() * 8, false); }
    auto are_empty(const bits_t &bits) {
      for (auto b : bits)
        if (b)
          return false;
      return true;
    }

    auto generate_bits() {
      std::vector< bits_t > op_bits( rinst.operands.size(), empty_bits() );

      for (std::size_t i = 0; i < permutations.size(); ++i) {
        for (std::size_t op_i = 0; op_i < rinst.operands.size(); ++op_i) {
          if (!permutations[i]) {
            continue;
          }
          using C = ifuzz::permutate::Comparator;
          op_bits[op_i][i] = C(arch.get()).compare(rinst, *permutations[i], op_i);
        }
      }
      return op_bits;
    }

    auto FuzzOps(bool generate_all=true) {
      shadowinst::Instruction shadow_inst;
      auto op_bits = generate_bits();

      for (std::size_t i = 0; i < rinst.operands.size(); ++i) {
        switch(rinst.operands[i].type) {
          case OpType::kTypeRegister : {
            shadow_inst.Add<shadowinst::Reg>(op_bits[i]);
            break;
          }
          case OpType::kTypeImmediate : {
            shadow_inst.Add<shadowinst::Immediate>(op_bits[i]);
            break;
          }
          case OpType::kTypeAddress: {
            auto &s_addr = shadow_inst.Add<shadowinst::Address>();
            distribute_addr(op_bits[i], i, *s_addr.address);
            break;
          }
          default:
            shadow_inst.operands.emplace_back();
            break;
        }
      }
      // Check sanity
      CHECK(shadow_inst.operands.size() == rinst.operands.size());

      for (std::size_t i = 0; i < shadow_inst.operands.size(); ++i) {
        if (!shadow_inst[i].IsHusk()) {
          shadow_inst.deps.push_back( {{i, &shadow_inst[i]}} );
        }
      }


      // It is possible some operands were not properly populated
      // (due to being read+write operands), so they need special attention.
      //ResolveHusks(shadow_inst);
      HuskResolver(*this).resolve_husks(shadow_inst);
      if (generate_all) {
        PopulateTranslationTables(shadow_inst);
      }

      return shadow_inst;
    }


    void PopulateTranslationTables(shadowinst::Instruction &s_inst) {

      // First we define navigation helpers.
      auto collect_navigations = [](auto &group, auto suffix) {
        std::vector<ifuzz::reg_navigation_t> out;
        for (auto &[idx, op] : group) {
          out.push_back(std::tuple_cat(std::make_tuple(idx), suffix));
        }
        return out;
      };

      auto has_reg = [](auto &g) {
        return std::all_of(g.begin(), g.end(), [](auto x) { return std::get<1>(x)->reg; } );
      };
      auto has_base_reg = [](auto &g) {
        return std::all_of(g.begin(), g.end(), [](auto x) {
            return std::get<1>(x)->address->base_reg;
        });
      };
      auto has_index_reg = [](auto &g) {
        return std::all_of(g.begin(), g.end(), [](auto x) {
            return std::get<1>(x)->address->index_reg;
        });
      };

      auto getter = [](remill::Instruction &, ifuzz::reg_navigation_t) { return true; };


      for (auto &group : s_inst.deps) {
        CHECK(group.size() > 0);
        auto &[idx_, fst_] = *group.begin();
        auto fst = rinst.operands[idx_];

        auto handle = [&](auto handler, auto suffix) {
          if (handler(group)) {
            auto idxs = collect_navigations(group, suffix);
            PopulateTranslationTable(s_inst, getter, idxs);
          }
        };

        if (fst.type == OpType::kTypeRegister) {
          handle(has_reg, std::make_tuple(0, 0));
        }
        if (fst.type == OpType::kTypeAddress) {
          handle(has_base_reg, std::make_tuple(1, 0));
          handle(has_index_reg, std::make_tuple(1, 1));
        }
      }
    }

    template<typename Getter, typename Idxs>
    bool RegEnlargementHeuristic(shadowinst::Instruction &s_inst, Getter get_reg, Idxs idxs) {
      CHECK(idxs.size() > 0);
      const auto &s_reg = **ifuzz::fetch_reg(s_inst, idxs[0]);

      auto [from, size_] = s_reg.biggest_chunk();
      auto size = size_;
      // 3 bits are okay since we do not always need REX prefix.
      // 4 and more bits are most likely an error in decoding and we definitely
      // do not want to add even more.
      if (size == 3 || s_reg.region_bitsize() >= 4) {
        return false;
      }

      auto _dd_left = [&](shadowinst::Reg out, uint64_t from) -> shadowinst::Reg {
        out.regions.erase(from);
        out.regions[from - 1] = size + 1;
        return out;
      };

      auto _dd_right = [&](shadowinst::Reg out, uint64_t from) -> shadowinst::Reg {
        out.regions[from] = size + 1;
        return out;
      };

      auto _d_hole = [&](shadowinst::Reg out, auto hole) -> shadowinst::Reg {
        out.regions.erase(hole + 1);
        out.regions[hole - 1] = 3ul;
        return out;
      };

      auto make_vessel = [&](auto maker, uint64_t fw) {
        ifuzz::s_reg_vessel out;
        for (auto &idx : idxs) {
          out[std::get<0>(idx)] = maker(**ifuzz::fetch_reg(s_inst, idx), fw);
        }
        return out;
      };

      using group_t = std::vector< ifuzz::s_reg_vessel >;
      group_t candidates;
      if (size == 2) {
        // _DD_ -> DDD_
        if (from != 0 && !s_reg.regions.count(from - 1)) {
          candidates.push_back(make_vessel(_dd_left, from));
        }
        // _DD_ -> _DDD
        if (from + size < rinst.bytes.size() * 8 && !s_reg.regions.count(from + size)) {
          candidates.push_back(make_vessel(_dd_right, from));
        }
      }

      if (size == 1) {
        auto hole = s_reg.get_hole();
        if (!hole) {
          return false;
        }
        candidates.push_back(make_vessel(_d_hole, *hole));
      }

      if (candidates.empty()) {
        return false;
      }

      // We also need to check that the newly added combination satisfies the original
      // selection criterium.
      // NOTE(lukas): While this works (and overall is a good idea), the implementation
      //              is a bit spaghetti like and would probably deserve some refactor.
      auto wget_reg = [&](auto inst, auto idxs_) {
        if (!get_reg(inst, idxs_)) {
          return false;
        }

        std::map<std::size_t, const remill::Operand *> items;
        for (auto &navigation : idxs) {
          auto idx = std::get<0>(navigation);
          items.emplace(idx, &inst.operands[idx]);
        }

        return ifuzz::permutate::HuskEnlargerComparator(arch.get()).verbose_compare(rinst, inst, items);
      };

      auto for_valid = [](auto fn, auto &group) {
        bool out = true;
        for (auto &what : group) {
          if (!what) {
            continue;
          }
          out &= fn(*what);
        }
        return out;
      };

      auto exactly_one_entry = [](auto &what) {
        for (auto &[_, mats] : what.translation_map) {
          if (mats.size() > 1) {
            return false;
          }
        }
        return true;
      };
      auto grew_ = [&](auto &what) {
        return !(what.translation_map.size() <= s_reg.translation_map.size());
      };

      auto is_valid = [&](auto &group) { return for_valid(exactly_one_entry, group); };
      auto grew = [&](auto &group) { return for_valid(grew_, group); };

      PopulateTranslationTable_(s_inst, wget_reg, idxs);
      std::optional<ifuzz::s_reg_vessel> chosen;
      for (auto &c : candidates) {
        PopulateTranslationTable_(c, wget_reg, idxs);

        // We want to select more bits only if they led to more decoded reg operands
        // and their values satisfy our conditions.
        if (grew(c) && is_valid(c)) {
          if (chosen) {
            LOG(FATAL) << "Reg enlargement heuristic has chosen multiple candidates!";
          }
          chosen = std::move(c);
        }
      }
      // NOTE(lukas): On amd64 this always signals that the translation map will
      //              not be formed properly.
      if (!chosen) {
        LOG(FATAL) << "Reg enlargement heuristic did not choose any candidate!";
      }
      ifuzz::apply_vessel(s_inst, *chosen, idxs);
      return true;
    }

    template<typename Getter, typename Idxs>
    void PopulateTranslationTable(shadowinst::Instruction &s_inst, Getter &&get_reg, Idxs idxs)
    {
      if (RegEnlargementHeuristic(s_inst, get_reg, idxs)) {
        return;
      }
      return PopulateTranslationTable_(s_inst, std::forward<Getter>(get_reg), idxs);
    }

    // `Getter` takes an remill::Operand and returns the register we to compare against.
    // This is so we can get nested registers as well (for example in `Address`)
    template<typename S, typename Getter, typename Idxs>
    void PopulateTranslationTable_(S &s_inst, Getter &&get_reg, Idxs idxs_)
    {
      auto collect_idxs = [&](const auto &shadow_reg) {
        CHECK(shadow_reg);
        std::vector<uint64_t> idxs;
        for (auto &[from, size] : (*shadow_reg)->regions) {
          auto afrom = rinst.bytes.size() * 8 - from - size;
          for (uint64_t i = 0; i < size; ++i) {
            idxs.push_back(afrom);
            ++afrom;
          }
        }
        return idxs;
      };

      CHECK(idxs_.size() > 0);
      auto idxs = collect_idxs(ifuzz::fetch_reg(s_inst, idxs_[0]));
      for (std::size_t i = 1; i < idxs_.size(); ++i) {
        CHECK(idxs == collect_idxs(ifuzz::fetch_reg(s_inst, idxs_[i])));
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

        for (auto &navigation : idxs_) {
          if (!get_reg(tmp, navigation)) {
            return;
          }
        }

        auto generate_bmap = [&]() {
          std::vector<bool> out;
          std::stringstream ss;

          // Generate the value
          for (auto idx : idxs) {
            auto byte = static_cast<uint8_t>(bytes[idx / 8]);
            out.push_back((byte >> (7 - (idx % 8))) & 1u);
            ss << out.back();
          }

          return out;
        };

        // `tmp` hosts a valid instruction, therefore we now want to retrieve
        // the register operand we are fuzzing.
        // `get_reg` is allowed to perform more logic than simple retrieval, that's
        // why the return type is wrapped in `std::optional` -> if the value is not
        // present we do not want to work with this operand.
        for (auto &navigation : idxs_) {
          auto maybe_reg = ifuzz::fetch_reg(tmp, navigation);
          if (!maybe_reg) {
            continue;
          }
          const auto &reg = **maybe_reg;

          // Register name is empty -- therefore it is most likely not a decoded register
          if (reg.name.empty()) {
            return;
          }
          (*ifuzz::fetch_reg(s_inst, navigation))->
              translation_map[reg.name].insert(generate_bmap());
        }
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
  };


} // namespace circ
