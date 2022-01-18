/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <remill/Arch/Arch.h>
#include <remill/Arch/Instruction.h>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Lifter/Shadows.hpp>

namespace circ::ifuzz
{

    using s_reg_vessel = std::array< std::optional< shadowinst::Reg >, 8 >;
    using maybe_sreg_vessel = std::optional< s_reg_vessel >;

    using reg_navigation_t = std::tuple< uint32_t, uint32_t, uint32_t >;

    struct sel
    {
        static constexpr inline uint32_t reg = 0;
        static constexpr inline uint32_t base = 1;
        static constexpr inline uint32_t index = 2;
        static constexpr inline uint32_t segment = 3;
    };

    template< uint32_t I >
    bool has_reg(const shadowinst::Operand &op)
    {
        if constexpr ( I == sel::reg )
            return op.reg.has_value();
        else if constexpr ( I == sel::base )
            return op.address && op.address->base_reg.has_value();
        else if constexpr ( I == sel::index )
            return op.address && op.address->index_reg.has_value();
        else if constexpr ( I == sel::segment )
            return op.address && op.address->segment.has_value();
        else
            unreachable() << "Unreachable";
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
    std::optional< shadowinst::Reg > &get_reg(shadowinst::Operand &op)
    {
        if constexpr ( I == sel::reg )          return op.reg;
        else if constexpr ( I == sel::base )    return op.address->base_reg;
        else if constexpr ( I == sel::index )   return op.address->index_reg;
        else if constexpr ( I == sel::segment ) return op.address->segment;
        else unreachable() << "Unreachable";
    }

    template< uint32_t I >
    const std::optional< shadowinst::Reg > &get_reg(const shadowinst::Operand &op)
    {
        if constexpr ( I == sel::reg )          return op.reg;
        else if constexpr ( I == sel::base )    return op.address->base_reg;
        else if constexpr ( I == sel::index )   return op.address->index_reg;
        else if constexpr ( I == sel::segment ) return op.address->segment;
        else unreachable() << "Unreachable";
    }
    // Returns a copy!
    template< uint32_t I >
    std::optional< shadowinst::Reg > get_reg_safely(shadowinst::Operand &op)
    {
        if constexpr ( I == sel::reg )
            return op.reg;
        else
        {
            if (!op.address)
                return {};
            return get_reg< I >(op);
        }
    }

    template< uint32_t I >
    remill::Operand::Register &get_reg(remill::Operand &op)
    {
        if constexpr ( I == sel::reg )          return op.reg;
        else if constexpr ( I == sel::base )    return op.addr.base_reg;
        else if constexpr ( I == sel::index )   return op.addr.index_reg;
        else if constexpr ( I == sel::segment ) return op.addr.segment_base_reg;
        else unreachable() << "Unreachable";
    }

    template< uint32_t I >
    const remill::Operand::Register &get_reg(const remill::Operand &op)
    {
        if constexpr ( I == sel::reg )          return op.reg;
        else if constexpr ( I == sel::base )    return op.addr.base_reg;
        else if constexpr ( I == sel::index )   return op.addr.index_reg;
        else if constexpr ( I == sel::segment ) return op.addr.segment_base_reg;
        else unreachable() << "Unreachable";
    }

    // TODO(lukas): tiny86 specific (possibly x86 specific as well).
    static inline bool is_reg_octet(uint64_t from, uint64_t size)
    {
        return (from % 8 == 0 || from % 8 == 3) && size == 3;
    }


} // circ::ifuzz
