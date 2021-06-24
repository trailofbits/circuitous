/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <remill/Arch/Arch.h>
#include <remill/Arch/Instruction.h>

#include <circuitous/Lifter/Shadows.hpp>

namespace circ::ifuzz {

  using s_reg_vessel = std::array< std::optional< shadowinst::Reg >, 8 >;
  using maybe_sreg_vessel = std::optional< s_reg_vessel >;

  using reg_navigation_t = std::tuple< uint32_t, uint32_t, uint32_t >;

  static auto fetch_reg(remill::Instruction &inst, reg_navigation_t idxs)
  -> std::optional<remill::Operand::Register *>
  {
    auto [inst_idx, op_idx, snd] = idxs;

    auto &op = inst.operands[inst_idx];
    if (op_idx == 0) {
      return std::make_optional(&op.reg);
    }
    if (op_idx == 1) {
      return std::make_optional( (snd == 0) ? &op.addr.base_reg : &op.addr.index_reg );
    }
    if (op_idx == 0xff) {
      return std::nullopt;
    }
    LOG(FATAL) << "Unknown fetch config";
  }

  static auto fetch_reg(shadowinst::Instruction &s_inst, reg_navigation_t idxs)
  -> std::optional<shadowinst::Reg *>
  {
    auto [inst_idx, op_idx, snd] = idxs;

    auto &op = s_inst.operands[inst_idx];
    if (op_idx == 0) {
      return std::make_optional(&*op.reg);
    }
    if (op_idx == 1) {
      return std::make_optional( (snd == 0) ? &*op.address->base_reg
                                            : &*op.address->index_reg );
    }
    if (op_idx == 0xff) {
      return std::nullopt;
    }
    LOG(FATAL) << "Unknown fetch config";
  }

  static auto fetch_reg(s_reg_vessel &vessel, reg_navigation_t idxs)
  -> std::optional<shadowinst::Reg *>
  {
    auto [inst_idx, _, _2] = idxs;
    CHECK(inst_idx < vessel.size());

    if (vessel[inst_idx]) {
      return { &*vessel[inst_idx] };
    }
    return std::nullopt;
  }

  template<typename S>
  void assign_reg(S &vessel, reg_navigation_t navigation, shadowinst::Reg s_reg) {
    auto where = fetch_reg(vessel, navigation);
    CHECK(where && *where) << "Cannot assign into register that is not present!";
    **where = std::move(s_reg);
  }

  template<typename S>
  void assign_reg(S &vessel, reg_navigation_t nav, std::optional<shadowinst::Reg *> s_reg)
  {
    CHECK(s_reg && *s_reg) << "Cannot assign register that is not present!";
    return assign_reg(vessel, nav, **s_reg);
  }

  template<typename S, typename Idxs>
  void apply_vessel(S &onto, s_reg_vessel &vessel, const Idxs &navigations) {
    for (auto &navigation : navigations) {
      assign_reg(onto, navigation, fetch_reg(vessel, navigation));
    }
  }


} // circ::ifuzz