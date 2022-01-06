/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <remill/Arch/Arch.h>
#include <remill/Arch/Instruction.h>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Lifter/Shadows.hpp>

namespace circ::ifuzz {

  using s_reg_vessel = std::array< std::optional< shadowinst::Reg >, 8 >;
  using maybe_sreg_vessel = std::optional< s_reg_vessel >;

  using reg_navigation_t = std::tuple< uint32_t, uint32_t, uint32_t >;

  static inline auto fetch_reg(remill::Operand &r_op, std::tuple<uint32_t, uint32_t> idxs)
  -> std::optional<remill::Operand::Register *>
  {
    auto [op_idx, snd] = idxs;
    if (op_idx == 0) {
      return std::make_optional(&r_op.reg);
    }
    if (op_idx == 1) {
      return std::make_optional( (snd == 0) ? &r_op.addr.base_reg : &r_op.addr.index_reg );
    }
    if (op_idx == 0xff) {
      return std::nullopt;
    }
    UNREACHABLE() << "Unknown fetch config";
  }

  static inline auto fetch_reg(shadowinst::Operand &s_op, std::tuple<uint32_t, uint32_t> idxs)
  -> std::optional<shadowinst::Reg *>
  {
    auto [op_idx, snd] = idxs;
    if (op_idx == 0) {
      return std::make_optional(&*s_op.reg);
    }
    if (op_idx == 1) {
      return std::make_optional( (snd == 0) ? &*s_op.address->base_reg
                                            : &*s_op.address->index_reg );
    }
    if (op_idx == 0xff) {
      return std::nullopt;
    }
    UNREACHABLE() << "Unknown fetch config";
  }

  template<typename I>
  static auto fetch_reg(I &inst, reg_navigation_t idxs)
  {
    auto inst_idx = std::get<0>(idxs);

    auto &op = inst.operands[inst_idx];
    return fetch_reg(op, {std::get<1>(idxs), std::get<2>(idxs)});
  }

  static inline auto fetch_reg(s_reg_vessel &vessel, reg_navigation_t idxs)
  -> std::optional<shadowinst::Reg *>
  {
    auto [inst_idx, _, _2] = idxs;
    CHECK(inst_idx < vessel.size());

    if (vessel[inst_idx]) {
      return { &*vessel[inst_idx] };
    }
    return std::nullopt;
  }

  static inline auto reg_reg() { return std::make_tuple(0u, 0u); }
  static inline auto addr_base_reg() { return std::make_tuple(1u, 0u); }
  static inline auto addr_index_reg() { return std::make_tuple(1u, 1u); }

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

  struct sel {
    static constexpr inline uint32_t reg = 0;
    static constexpr inline uint32_t base = 1;
    static constexpr inline uint32_t index = 2;
    static constexpr inline uint32_t segment = 3;
  };

  template< uint32_t I >
  bool has_reg(const shadowinst::Operand &op) {
    if constexpr ( I == sel::reg )
      return op.reg.has_value();
    else if constexpr ( I == sel::base )
      return op.address && op.address->base_reg.has_value();
    else if constexpr ( I == sel::index )
      return op.address && op.address->index_reg.has_value();
    else if constexpr ( I == sel::segment )
      return op.address && op.address->segment.has_value();
    else
      UNREACHABLE() << "Unreachable";
  }
  template< uint32_t I >
  bool is_consistent(const shadowinst::Instruction &s_inst,
                     const std::vector< std::size_t > &todo)
  {
    bool present = true;
    bool missing = true;
    for (auto idx : todo)
    {
      present &= has_reg< I >(s_inst[idx]);
      missing &= !has_reg< I >(s_inst[idx]);
    }
    return present != missing;
  }

  template< uint32_t I >
  bool all_have_reg(const shadowinst::Instruction &s_inst,
                    const std::vector< std::size_t > &todo)
  {
    for (auto idx : todo)
      if (!has_reg< I >(s_inst[idx]))
        return false;
    return true;
  }


  template< uint32_t I >
  std::optional< shadowinst::Reg > &get_reg(shadowinst::Operand &op) {
    if constexpr ( I == sel::reg )          return op.reg;
    else if constexpr ( I == sel::base )    return op.address->base_reg;
    else if constexpr ( I == sel::index )   return op.address->index_reg;
    else if constexpr ( I == sel::segment ) return op.address->segment;
    else UNREACHABLE() << "Unreachable";
  }

  template< uint32_t I >
  const std::optional< shadowinst::Reg > &get_reg(const shadowinst::Operand &op) {
    if constexpr ( I == sel::reg )          return op.reg;
    else if constexpr ( I == sel::base )    return op.address->base_reg;
    else if constexpr ( I == sel::index )   return op.address->index_reg;
    else if constexpr ( I == sel::segment ) return op.address->segment;
    else UNREACHABLE() << "Unreachable";
  }
  // Returns a copy!
  template< uint32_t I >
  std::optional< shadowinst::Reg > get_reg_safely(shadowinst::Operand &op) {
    if constexpr ( I == sel::reg )
      return op.reg;
    else {
      if (!op.address) return {};
      return get_reg< I >(op);
    }
  }
  template< uint32_t I >
  remill::Operand::Register &get_reg(remill::Operand &op) {
    if constexpr ( I == sel::reg )          return op.reg;
    else if constexpr ( I == sel::base )    return op.addr.base_reg;
    else if constexpr ( I == sel::index )   return op.addr.index_reg;
    else if constexpr ( I == sel::segment ) return op.addr.segment_base_reg;
    else UNREACHABLE() << "Unreachable";
  }

  template< uint32_t I >
  const remill::Operand::Register &get_reg(const remill::Operand &op) {
    if constexpr ( I == sel::reg )          return op.reg;
    else if constexpr ( I == sel::base )    return op.addr.base_reg;
    else if constexpr ( I == sel::index )   return op.addr.index_reg;
    else if constexpr ( I == sel::segment ) return op.addr.segment_base_reg;
    else UNREACHABLE() << "Unreachable";
  }

  // TODO(lukas): tiny86 specific (possibly x86 specific as well).
  static inline bool is_reg_octet(uint64_t from, uint64_t size) {
    return (from % 8 == 0 || from % 8 == 3) && size == 3;
  }


} // circ::ifuzz
