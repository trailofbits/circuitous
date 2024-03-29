/*
 * Copyright (c) 2021 - present Trail of Bits, Inc.
 */
#pragma once

#include <remill/Arch/Arch.h>
#include <remill/Arch/Instruction.h>

#include <circuitous/Lifter/Shadows.hpp>
#include <circuitous/Fuzz/InstNavigation.hpp>
#include <circuitous/Fuzz/Permute.hpp>
#include <circuitous/Support/Check.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/LLVMUtil.hpp>

#include <bitset>
#include <optional>
#include <string>
#include <vector>
#include <map>
#include <unordered_map>

namespace circ
{

    template< typename U >
    struct HuskResolver
    {
        using rops_map_t = std::map< std::size_t, const remill::Operand * >;
        using rops_maps_t = std::vector< rops_map_t >;
        using OPT = remill::Operand::Type;

        using bits_t = typename U::bits_t;

        U &fuzzer;

        HuskResolver(U &fuzzer_) : fuzzer(fuzzer_) {}

        std::size_t get_op_idx(const remill::Operand *op)
        {
            for (std::size_t i = 0; i < fuzzer.rinst.operands.size(); ++i)
                if (&fuzzer.rinst.operands[i] == op)
                    return i;
            unreachable() << "Not found";
        }

        // TODO(lukas): This is heurisitc. Later we are most likely going to need
        //              support multiple strategies. Also the selected heuristic
        //              implies the permutate::Comparator stack configuration.
        rops_maps_t group(const rops_map_t &ops, auto &&str_hash)
        {
            // TODO(lukas): First we need to sort by type
            std::unordered_map< OPT, rops_map_t > by_type;
            for (auto &x : ops)
                by_type[x.second->type].emplace(x.first, x.second);

            std::unordered_map<std::string, rops_map_t> by_hash;
            for (auto &[_, g_by_type] : by_type)
                for (auto &[idx, op] : g_by_type)
                    by_hash[str_hash(op)].emplace(idx, op);

            rops_maps_t out;

            auto handle_suspicious_group = [&](auto maps) {
                log_error() << "handle_suspicious_group invoked on "
                            << fuzzer.rinst.Serialize();
                std::map< std::size_t, const remill::Operand * > w_to_idx;
                std::map< std::size_t, const remill::Operand * > r_to_idx;
                for (auto &[u, x] : maps)
                {
                    auto i = get_op_idx(x);
                    if (x->action == remill::Operand::Action::kActionWrite)
                        w_to_idx[i] = x;
                    else if (x->action == remill::Operand::Action::kActionRead)
                        r_to_idx[i] = x;
                    else
                        unreachable() << "Unreachable";
                }
                check(w_to_idx.size() == r_to_idx.size());
                for (auto [i, op] : w_to_idx)
                {
                    check(r_to_idx.count(i + 1));
                    out.push_back({{i, op}, {i + 1, r_to_idx[i + 1]}});
                }
            };

            for (auto &[_, maps] : by_hash)
                if (maps.size() > 2)
                  handle_suspicious_group(maps);
                else
                  out.push_back(std::move(maps));
            return out;
        }

        // TODO(lukas): Decoder sometimes in case of amd64 changes write register
        //              to be 64b even though it is 32. In case this register
        //              is linked with other read, they are no longer aligned, as the
        //              read will have original size.
        rops_maps_t group_amd64hack(const rops_map_t &ops)
        {
            auto str_hash = [=](auto op) -> std::string {
                switch(op->type) {
                    case OPT::kTypeImmediate:
                        return std::to_string(op->imm.val);
                    case OPT::kTypeRegister:
                        if (auto big_reg = try_enclosing_reg(fuzzer.arch, op->reg.name))
                            return (*big_reg)->name;
                        return op->reg.name;
                    case OPT::kTypeAddress:
                        return op->addr.base_reg.name + op->addr.index_reg.name
                              + std::to_string(op->addr.scale)
                              + std::to_string(op->addr.displacement);
                    default: unreachable() << "Cannot hash operand";
                }
            };
            return group(ops, str_hash);
        }


        rops_maps_t group(const rops_map_t &ops)
        {
            auto str_hash = [](auto op) -> std::string {
                switch(op->type) {
                    case OPT::kTypeImmediate:
                        return std::to_string(op->imm.val);
                    case OPT::kTypeRegister:
                        return op->reg.name + std::to_string(op->size);
                    case OPT::kTypeAddress:
                        return op->addr.base_reg.name + op->addr.index_reg.name
                              + std::to_string(op->addr.scale)
                              + std::to_string(op->addr.displacement);
                    default: unreachable() << "Cannot hash operand";
                }
            };
            return group(ops, str_hash);
        }

        using idx_to_husk_t = std::map< std::size_t, const remill::Operand * >;
        idx_to_husk_t retrieve_husks(shadowinst::Instruction &s_inst)
        {
            idx_to_husk_t out;
            for (std::size_t i = 0; i < fuzzer.rinst.operands.size(); ++i)
                if (s_inst[i].is_husk())
                    out[i] = &fuzzer.rinst.operands[i];
            return out;
        }

        // Returns std::optional< bits_t > which contains value in case
        // operands got resolved on at least one bit - bit mapping
        // is present in returned value;
        template< typename Resolver >
        auto try_resolve_group(const rops_map_t &group)
        ->std::optional< std::tuple< typename U::bits_t, std::set< uint32_t > > >
        {
            // There is no point working with smaller than 2 group
            if (group.size() <= 1)
                return {};

            auto husk_bits = fuzzer.empty_bits();
            std::set< uint32_t > dirt_bits;
            for (std::size_t i = 0; i < fuzzer.permutations.size(); ++i)
            {
                if (!fuzzer.permutations[i])
                    continue;

                //using CMP = ifuzz::permutate::HuskComparator;
                using CMP = Resolver;
                auto res =  CMP(fuzzer.arch)
                    .compare(fuzzer.rinst, *fuzzer.permutations[i], group);
                husk_bits[i] = ifuzz::permutate::DiffResult< CMP::exact_mod >()
                    .get_result(res, group);
                if (ifuzz::permutate::DiffResult< CMP::exact_mod >().are_dirty(res, group))
                    dirt_bits.insert(static_cast< uint32_t >(fuzzer.permutations.size() - i));
            }
            if (fuzzer.are_empty(husk_bits))
                return {};
            return {{ std::move(husk_bits), std::move(dirt_bits) }};
        }

        // Update shadowinst with newly discovered information and add appropriate entry
        // to `deps`.
        void update(shadowinst::Instruction &s_inst,
                    const rops_map_t &group, const bits_t &husk_bits)
        {
            s_inst.deps.push_back({});
            shadowinst::ordered_bits_t obits(husk_bits, fuzzer.rinst_bitsize());
            for (auto &[i, op] : group)
            {
                s_inst.deps.back().emplace_back(i, &s_inst.Replace(i, op->type, obits));

                if (op->type == remill::Operand::Type::kTypeAddress)
                    fuzzer.distribute_addr(husk_bits, i, *s_inst[i].address());
            }
        }

        // TODO(lukas): Implement some sort of configurable dispatch.
        void resolve_husks(shadowinst::Instruction &s_inst)
        {
            // First heuristic worked
            auto def_grouping = [&](auto &r_husks) { return this->group(r_husks); };
            if (try_resolve_husks< ifuzz::permutate::HuskComparator >(
                  s_inst, retrieve_husks(s_inst), def_grouping))
            {
                return;
            }
            // TODO(lukas): Out of place.
            auto remill_sizes = [&](auto &r_husks) { return this->group_amd64hack(r_husks); };
            if (try_resolve_husks< ifuzz::permutate::HuskComparatorAmd64Hack >(
                  s_inst, retrieve_husks(s_inst), remill_sizes))
            {
                return;
            }
        }

        template< typename Resolver >
        bool try_resolve_husks(shadowinst::Instruction &s_inst,
                               const auto &r_husks, auto &&make_group)
        {
            // Need at least 2 husks to work with
            if (r_husks.size() <= 1)
                return true;

            bool out = true;
            for (auto &group : make_group(r_husks))
            {
                if (auto bits = try_resolve_group< Resolver >(group))
                {
                    auto [husk_bits, dirt_bits] = *bits;
                    update(s_inst, group, std::move(husk_bits));
                    s_inst.dirt.push_back(std::move(dirt_bits));
                } else
                    out &= false;
            }
            return out;
        }
    };
} // namespace circ
