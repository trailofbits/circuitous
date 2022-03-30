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

#include <circuitous/Support/Check.hpp>
#include <circuitous/Support/Log.hpp>

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
 * This is useful because it allows us make the generated circuit smaller.
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

namespace circ
{

    struct InstructionFuzzer
    {
        // Import some remill types we will often neeed
        using Instruction = remill::Instruction;
        using Operand = remill::Operand;
        using Kind = remill::Operand::Address::Kind;
        using OpType = remill::Operand::Type;
        using Arch_ptr = const remill::Arch *;

        using bits_t = std::vector< bool >;

        Arch_ptr arch;
        const remill::Instruction &rinst;

        ifuzz::permutate::permutations_t permutations;

        InstructionFuzzer(Arch_ptr arch_, const remill::Instruction &rinst_)
          : arch(arch_), rinst(rinst_), permutations(ifuzz::permutate::flip(rinst, arch))
        {}

        // We know that `bits` are used to encode `s_addr`. We need to
        // determine the relationship between these bits and attributes
        // of Address operand.
        void distribute_addr(const std::vector<bool> &bits,
                            std::size_t idx,
                            shadowinst::Address &s_addr)
        {
            std::vector<bits_t> distributed_bits( 5, bits_t(rinst.bytes.size() * 8, false) );

            for (std::size_t i = 0; i < bits.size(); ++i)
            {
                if (!bits[i]
                    || !permutations[i]
                    || permutations[i]->operands.size() != rinst.operands.size())
                {
                    continue;
                }

                auto &self = rinst.operands[idx].addr;
                check(permutations[i]->operands[idx].type == OpType::kTypeAddress);
                auto &flipped = permutations[i]->operands[idx].addr;

                if (self.base_reg.name != flipped.base_reg.name)
                    distributed_bits[0][i] = true;

                if (self.index_reg.name != flipped.index_reg.name)
                    distributed_bits[1][i] = true;

                if (self.scale != flipped.scale && !flipped.index_reg.name.empty())
                    distributed_bits[2][i] = true;

                if (self.displacement != flipped.displacement)
                    distributed_bits[3][i] = true;
            }

            auto make = [](auto &where, const auto& bits_) {
                using raw_t = std::decay_t< std::remove_reference_t< decltype(where) > >;
                using T = typename raw_t::value_type;
                // These are inner operands and therefore do not have a position.
                where = std::make_optional< T >(shadowinst::ordered_bits_t(bits_));
            };

            make(s_addr.base_reg, distributed_bits[0]);
            make(s_addr.index_reg, distributed_bits[1]);
            make(s_addr.scale, distributed_bits[2]);
            make(s_addr.displacement, distributed_bits[3]);
        }

        auto empty_bits() const { return bits_t(rinst.bytes.size() * 8, false); }
        auto are_empty(const bits_t &bits)
        {
            for (auto b : bits)
                if (b)
                    return false;
            return true;
        }

        auto generate_bits()
        {
            std::vector< bits_t > op_bits( rinst.operands.size(), empty_bits() );
            std::vector< std::set< uint32_t > > dirts( rinst.operands.size() );

            for (std::size_t i = 0; i < permutations.size(); ++i)
            {
                if (!permutations[i])
                  continue;

                log_dbg() << permutations.size() - 1 - i
                          << ": checking structure of:\n" << permutations[i]->Serialize();
                for (std::size_t op_i = 0; op_i < rinst.operands.size(); ++op_i)
                {
                    log_dbg() << "Operand: " << op_i;

                    using namespace ifuzz::permutate;
                    auto item = to_item(rinst, op_i);
                    auto res = Comparator(arch).compare(rinst, *permutations[i], item);
                    op_bits[op_i][i] = DiffResult().get_result(res, item);
                    if (DiffResult().are_dirty(res,item))
                        dirts[op_i].insert(static_cast< uint32_t >(permutations.size() - 1 - i));
                }
                log_dbg() << "Done.";
            }
            return std::make_tuple(op_bits, dirts);
        }

        auto FuzzOps(bool generate_all=true)
        {
            shadowinst::Instruction shadow_inst;
            auto [op_bits, dirts] = generate_bits();

            for (std::size_t i = 0; i < rinst.operands.size(); ++i)
            {
                switch(rinst.operands[i].type)
                {
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
            check(shadow_inst.operands.size() == rinst.operands.size());

            for (std::size_t i = 0; i < shadow_inst.operands.size(); ++i)
            {
                if (!shadow_inst[i].IsHusk()) {
                    shadow_inst.deps.push_back( {{i, &shadow_inst[i]}} );
                    shadow_inst.dirt.push_back( dirts[i] );
                }
            }


            // It is possible some operands were not properly populated
            // (due to being read+write operands), so they need special attention.
            //ResolveHusks(shadow_inst);
            HuskResolver(*this).resolve_husks(shadow_inst);
            if (generate_all)
                PopulateTranslationTables(shadow_inst);
            return shadow_inst;
        }

        void PopulateTranslationTables(shadowinst::Instruction &s_inst)
        {
            check(s_inst.deps.size() == s_inst.dirt.size());

            std::size_t i = 0;
            for (auto &group : s_inst.deps)
            {
                std::vector< std::size_t > todo;
                for (const auto &[idx, _] : group)
                    todo.push_back(idx);
                consolidate_addr(s_inst, todo, s_inst.dirt[i]);
                reg_enlarge(s_inst, todo);
                populate(s_inst, todo);
                ++i;
            }
        }

        using idx_vector_t = std::vector< std::size_t >;
        using maybe_rinst_t = std::optional< remill::Instruction >;
        using idxs_t = std::vector< uint64_t >;

        void derive_segment(shadowinst::Instruction &s_inst, const idx_vector_t &todo)
        {
            using namespace ifuzz;
            for (auto idx : todo)
                if (!s_inst[idx].address.has_value() || s_inst[idx].address->segment.has_value()
                    || !has_reg< sel::base >(s_inst[idx]))
                {
                    return;
                }

            for (auto idx : todo)
            {
                auto &segment = get_reg< sel::segment >(s_inst[idx]);
                segment = shadowinst::Reg(get_reg< sel::base >(s_inst[idx])->regions);
            }
        }

        template< uint32_t I >
        bool check_entry(const auto &s_op, const auto &r_op, const auto &nrinst)
        {
            using namespace ifuzz;
            auto r_reg = get_reg< I >(r_op);
            if (!has_reg< I >(s_op))
                return true;

            const auto &s_reg = *get_reg< I >(s_op);
            auto it = s_reg.translation_map.find(r_reg.name);
            if (it == s_reg.translation_map.end())
                return false;
            return it->second.count(get_bmap(s_reg, nrinst));
        }

        bool valid_segment(shadowinst::Instruction &s_inst,
                           remill::Instruction &nrinst,
                           const idx_vector_t &todo)
        {
            for (std::size_t i = 0; i < s_inst.operands.size(); ++i)
            {
                auto s_op = s_inst[i];
                auto r_op = nrinst.operands[i];

                using namespace ifuzz;
                if (!check_entry< sel::base >(s_op, r_op, nrinst) ||
                    !check_entry< sel::index >(s_op, r_op, nrinst) ||
                    !check_entry< sel::reg >(s_op, r_op, nrinst))
                {
                    return false;
                }
            }
            return true;
        }

        template< uint32_t I >
        void default_reg(shadowinst::Instruction &s_inst, const idx_vector_t &todo)
        {
            using namespace ifuzz;
            check(is_consistent< I >(s_inst, todo));
            if (!all_have_reg< I >(s_inst, todo))
                return;
            for (auto idx : todo)
            {
                auto &reg = get_reg< I >(s_inst[idx]);
                if (!reg->empty() && reg->translation_map.empty())
                    reg->regions.clear();
            }
        }

        void coerce_by_segment_override(shadowinst::Instruction &s_inst,
                                        const idx_vector_t &todo)
        {
            for (auto idx : todo)
                check(s_inst[idx].address->segment == s_inst[todo[0]].address->segment);

            if (!rinst.segment_override || !s_inst[todo[0]].address)
                return;

            auto &front = s_inst[todo[0]].address->segment;
            if (!front)
                return;

            // TODO(lukas): Fix on remill side.
            auto seg_base = rinst.segment_override->name + "BASE";
            check(front->translation_map.size() == 1 &&
                  front->translation_map.begin()->first == seg_base)
              << front->to_string();
            for (auto idx : todo)
            {
                s_inst[idx].address->segment->regions.clear();
                s_inst[idx].address->segment->translation_map.clear();
                s_inst[idx].address->segment->translation_map[seg_base] = {{}};
            }
        }

        void populate(shadowinst::Instruction &s_inst, const idx_vector_t &todo)
        {
            using namespace ifuzz;
            try_populate_reg< sel::reg >(s_inst, todo);
            default_reg< sel::reg >(s_inst, todo);
            try_populate_addr< sel::base >(s_inst, todo);
            default_reg< sel::base >(s_inst, todo);
            try_populate_addr< sel::index >(s_inst, todo);
            default_reg< sel::index >(s_inst, todo);

            derive_segment(s_inst, todo);
            try_populate_addr< ifuzz::sel::segment >(s_inst, todo);
            default_reg< sel::segment >(s_inst, todo);
            //coerce_by_segment_override(s_inst, todo);
        }


        bool in_todo(std::size_t x, const idx_vector_t &todo)
        {
            for (auto y : todo) if (x == y) return true;
            return false;
        }

        bool eq(const remill::Instruction &a, const remill::Instruction &b)
        {
            return a.Serialize() == b.Serialize();
        }

        bool eq(const remill::Operand &a, const remill::Operand &b)
        {
            if (a.type != b.type) return false;

            if (a.type == remill::Operand::Type::kTypeRegister) {
                return eq(a.reg, b.reg);
            }

            if (a.type == remill::Operand::Type::kTypeAddress) {
                return    eq(a.addr.base_reg, b.addr.base_reg)
                    && eq(a.addr.index_reg, b.addr.index_reg)
                    && eq(a.addr.segment_base_reg, b.addr.segment_base_reg)
                    && a.addr.scale == b.addr.scale
                    && a.addr.displacement == b.addr.displacement
                    && a.addr.address_size == b.addr.address_size
                    && a.addr.kind == b.addr.kind;
            }

            if (a.type == remill::Operand::Type::kTypeImmediate) {
                return a.imm.val == b.imm.val && a.imm.is_signed == b.imm.is_signed;
            }

            unreachable() << "Unexpected operands to custom eq hook.";
        }

        bool eq_segment_change(const remill::Operand &a, const remill::Operand &b)
        {
            if (a.type == remill::Operand::Type::kTypeAddress) {
                auto x =  eq(a.addr.index_reg, b.addr.index_reg)
                    && eq(a.addr.segment_base_reg, b.addr.segment_base_reg)
                    && a.addr.scale == b.addr.scale
                    && a.addr.displacement == b.addr.displacement
                    && a.addr.address_size == b.addr.address_size
                    && a.addr.kind == b.addr.kind;
                return x &&
                    (std::string_view(a.addr.base_reg.name).ends_with("BP") ||
                     std::string_view(a.addr.base_reg.name).ends_with("SP"))
                    ==
                    (std::string_view(a.addr.base_reg.name).ends_with("BP") ||
                     std::string_view(a.addr.base_reg.name).ends_with("SP"));
            }
            log_error() << "Unexpected operands to custom eq hook: eq_segment_change.";
            log_error() << a.Serialize();
            log_error() << b.Serialize();
            log_kill() << "Program end.";
        }

        bool eq(const remill::Operand::Register &a, const remill::Operand::Register &b)
        {
            return a.size == b.size && a.name == b.name;
        }

        template< uint32_t I >
        auto get_populate_proccess(auto get_self, auto get_reg, const idx_vector_t &todo)
        {
            return [=, &todo](maybe_rinst_t nrinst, const idxs_t idxs, std::string errs)
            {
                if (!nrinst) {
                    // TODO(lukas): Should report some error?
                    return;
                }

                for (std::size_t i = 0; i < nrinst->operands.size(); ++i)
                {
                    if (!in_todo(i, todo) && !eq(nrinst->operands[i], rinst.operands[i]))
                        return;

                    // Extra explicit copy - we want to check for example that
                    // extra `scale` was not conjured by accident.
                    remill::Operand copy = rinst.operands[i];
                    ifuzz::get_reg< I >(copy) = ifuzz::get_reg< I >(nrinst->operands[i]);
                    if constexpr (I != ifuzz::sel::segment) {
                        if (!eq(copy, nrinst->operands[i]))
                            return;
                    }
                }

                for (auto idx : todo) {
                    auto reg_name = get_reg(idx, *nrinst);
                    if (reg_name.empty())
                        return;
                    get_self(idx).translation_map[reg_name].insert(get_bmap(idxs, *nrinst));
                }
            };
        }

        template< uint32_t I >
        bool try_populate_reg_(shadowinst::Instruction &s_inst, const idx_vector_t &todo)
        {
            auto idxs = collect_idxs(*ifuzz::get_reg< I >(s_inst[todo[0]]));

            auto get_self = [&](uint64_t i) -> shadowinst::Reg & {
                return *ifuzz::get_reg< I >( s_inst[i] );
            };
            auto get_reg = [&](uint64_t i, const remill::Instruction &nrinst) {
                return ifuzz::get_reg< I >(nrinst.operands[i]).name;
            };
            auto process = get_populate_proccess< I >(get_self, get_reg, todo);

            populate_by_permuting(rinst.bytes, idxs, process);
            return true;
        }

        template< uint32_t I >
        bool try_populate_reg(shadowinst::Instruction &s_inst, const idx_vector_t &todo)
        {
            for (auto idx : todo)
                if (!ifuzz::get_reg< I >(s_inst[idx]).has_value())
                    return false;
            return try_populate_reg_< I >(s_inst, todo);
        }

        template< uint32_t I >
        bool try_populate_addr(shadowinst::Instruction &s_inst, const idx_vector_t &todo)
        {
            for (auto idx : todo)
            {
                auto addr = s_inst[idx].address;
                if (!addr || !ifuzz::get_reg< I >(s_inst[idx]).has_value()) {
                    return false;
                }
            }
            return try_populate_reg_< I >(s_inst, todo);
        }

        std::vector< bool > get_bmap(const shadowinst::has_regions &regions,
                                     const remill::Instruction &nrinst)
        {
            return get_bmap(collect_idxs(regions), nrinst);
        }

        std::vector< bool > get_bmap(const idxs_t idxs, const remill::Instruction &nrinst)
        {
            std::vector< bool > out;
            for (auto idx : idxs) {
                auto byte = static_cast< uint8_t >(nrinst.bytes[idx / 8]);
                out.push_back((byte >> (7 - (idx % 8))) & 1u);
            }
            return out;
        }

        std::vector< uint64_t > collect_idxs(const shadowinst::has_regions &regions)
        {
            std::vector< uint64_t > idxs;
            for (const auto &[from, size] : regions) {
                auto afrom = rinst.bytes.size() * 8 - from - size;
                for (uint64_t i = 0; i < size; ++i) {
                    idxs.push_back(afrom);
                    ++afrom;
                }
            }
            return idxs;
        }

        void populate_by_permuting(std::string nbytes, const idxs_t idxs, auto yield)
        {
            auto preprocess = [&](const std::string &bytes) {
                std::stringstream ss;

                remill::Instruction tmp;
                if (!arch->DecodeInstruction(0, bytes, tmp)) {
                    ss << "Decode failed!\n";
                    return yield({}, idxs, ss.str());
                }

                if (tmp.function != rinst.function) {
                    ss << "Guaranteed permuation generated different instruction!\n";
                    return yield({}, idxs, ss.str());
                }

                if (tmp.bytes.size() != rinst.bytes.size()) {
                    ss << "Guaranteed permutation uses less bytes!\n";
                    return yield({}, idxs, ss.str());
                }

                if (tmp.operands.size() != rinst.operands.size()) {
                    ss << "Guaranteed permuation has different amount of operands!\n";
                    return yield({}, idxs, ss.str());
                }

                yield( std::make_optional(std::move(tmp)), idxs, ss.str());
            };

            _Permutate(nbytes, idxs, 0, preprocess);
        }

        std::vector< shadowinst::has_regions > get_candidates(
            const shadowinst::has_regions &original)
        {
            auto [from, size_] = original.biggest_chunk();
            auto size = size_;
            // 3 bits are okay since we do not always need REX prefix.
            // 4 and more bits are most likely an error in decoding and we definitely
            // do not want to add even more.
            if (size == 3 || original.region_bitsize() >= 4)
                return {};

            using maybe_region_t = std::optional< shadowinst::has_regions >;
            auto _dd_left = [&](shadowinst::has_regions out, uint64_t from) -> maybe_region_t {
                if (!ifuzz::is_reg_octet(from - 1, size + 1))
                    return {};
                out.regions.erase(from);
                out.regions[from - 1] = size + 1;
                return { std::move(out) };
            };

            auto _dd_right = [&](shadowinst::has_regions out, uint64_t from) -> maybe_region_t {
                if (!ifuzz::is_reg_octet(from, size + 1))
                    return {};
                out.regions[from] = size + 1;
                return { std::move(out) };
            };

            auto _d_hole = [&](shadowinst::has_regions out, auto hole) -> maybe_region_t {
                if (!ifuzz::is_reg_octet(hole - 1, 3ul))
                    return {};
                out.regions.erase(hole + 1);
                out.regions[hole - 1] = 3ul;
                return { std::move(out) };
            };

            std::vector< shadowinst::has_regions > candidates;

            auto try_emplace = [&](auto fn, auto arg) {
                if (auto r = fn(original, arg))
                    candidates.push_back(std::move(*r));
            };

            if (size == 2) {
                // _DD_ -> DDD_
                if (from != 0 && !original.regions.count(from - 1))
                    //candidates.push_back(_dd_left(original, from));
                    try_emplace(_dd_left, from);
                // _DD_ -> _DDD
                if (from + size < rinst.bytes.size() * 8 && !original.regions.count(from + size))
                    try_emplace(_dd_right, from);
                //candidates.push_back(_dd_right(original, from));
            }
            if (size == 1)
                if (auto hole = original.get_hole())
                    try_emplace(_d_hole, *hole);
            //candidates.push_back(_d_hole(original, *hole));
            return candidates;
        }

        template< uint32_t I >
        std::string get_name_or(const remill::Operand &from, const std::string &def)
        {
            const auto &reg = ifuzz::get_reg< I >( from );
            if (reg.name.empty())
                return def;
            return reg.name;
        }

        template< uint32_t I >
        void add_translation_entry(shadowinst::Instruction &s_inst,
                                   uint64_t idx,
                                   const std::string &name,
                                   auto value)
        {
            check(ifuzz::has_reg< I >(s_inst[idx]));
            auto &self = *ifuzz::get_reg< I >(s_inst[idx]);
            self.translation_map[name].insert(value);
        }

        void populate_whole_addr(shadowinst::Instruction &s_inst,
                                 const idx_vector_t &todo,
                                 const shadowinst::has_regions &regions)
        {
            auto idxs = collect_idxs(regions);

            auto process = [&](maybe_rinst_t nrinst, const idxs_t idxs, std::string errs)
            {
                if (!nrinst) {
                    // TODO(lukas): This is not a hard error, figure out a way to log this.
                    return;
                }

                for (auto idx : todo) {
                    auto size = std::to_string(nrinst->operands[idx].addr.address_size);
                    auto zero = "__remill_zero_i" + size;

                    auto base = get_name_or< 1 >(nrinst->operands[idx], zero);
                    add_translation_entry< 1 >(s_inst, idx, base, get_bmap(idxs, *nrinst));
                    auto index = get_name_or< 2 >(nrinst->operands[idx], zero);
                    add_translation_entry< 2 >(s_inst, idx, index, get_bmap(idxs, *nrinst));
                }
            };
            populate_by_permuting(rinst.bytes, idxs, process);
        }

        void consolidate_addr(shadowinst::Instruction &s_inst, const idx_vector_t &todo,
                              std::set< uint32_t > dirt)
        {
            if (!s_inst[todo[0]].address)
                return;
            auto flattened = s_inst[todo[0]].address->flatten_significant_regs();
            for (auto from : dirt)
                flattened.add(from, 1);

            auto raw_candidates = get_candidates(flattened);
            if (flattened.size() == 1)
            {
                auto [from, size] = *flattened.begin();
                if (ifuzz::is_reg_octet(from, size))
                    raw_candidates.push_back(flattened);
            }
            if (raw_candidates.empty())
                return;

            check(raw_candidates.size() == 1);
            auto &raw_candidate = raw_candidates[0];

            // Explicit copy!
            auto n_s_inst = s_inst;
            for (auto idx : todo)
            {
                n_s_inst[idx].address->base_reg = shadowinst::Reg(raw_candidate.regions);
                n_s_inst[idx].address->index_reg = shadowinst::Reg(raw_candidate.regions);
            }

            populate_whole_addr(n_s_inst, todo, raw_candidate);

            for (auto idx : todo) {
                using namespace ifuzz;
                check(n_s_inst[idx].address == n_s_inst[todo[0]].address);
            }

            for (auto idx : todo) {
                auto &index = ifuzz::get_reg< ifuzz::sel::index >(n_s_inst[idx]);
                if (index->translation_map.size() != 1)
                    break;
                const auto &[key, _] = *index->translation_map.begin();
                if (std::string_view(key).starts_with("__remill_zero_"))
                {
                    index->regions.clear();
                    index->translation_map.clear();
                }
            }

            for (auto idx : todo) {
                if (!n_s_inst[idx].address->base_reg->translation_map.empty())
                    s_inst[idx].address->base_reg = n_s_inst[idx].address->base_reg;
                if (!n_s_inst[idx].address->index_reg->translation_map.empty())
                    s_inst[idx].address->index_reg = n_s_inst[idx].address->index_reg;
            }

        }

        template< uint32_t I >
        std::vector< shadowinst::Reg > get_candidates(const shadowinst::Reg &s_reg)
        {
            std::vector< shadowinst::Reg > out;
            for (auto x : get_candidates(s_reg))
                out.emplace_back(std::move(x.regions));
            return out;
        }

        void reg_enlarge(shadowinst::Instruction &s_inst, const idx_vector_t &todo)
        {
            log_dbg() << "Going to enlarge regs.";
            reg_enlarge_< 0 >(s_inst, todo);
            reg_enlarge_< 1 >(s_inst, todo);
            reg_enlarge_< 2 >(s_inst, todo);
        }

        template< uint32_t I >
        bool reg_enlarge_(shadowinst::Instruction &s_inst, const idx_vector_t &todo)
        {
            if (!ifuzz::has_reg< I >(s_inst[todo[0]]))
                return true;

            auto candidates = get_candidates< I >(*ifuzz::get_reg< I >(s_inst[todo[0]]));
            if (candidates.empty())
                return true;

            auto s_reg = ifuzz::get_reg< I >(s_inst[todo[0]]);

            auto has_only_singleton_mappings = [](auto &what) {
                for (auto &[_, mats] : what.translation_map)
                    if (mats.size() > 1)
                        return false;
                return true;
            };

            std::optional< shadowinst::Reg > chosen = s_reg;
            auto grew = [&](auto &fst) {
                return chosen->translation_map.size() < fst.translation_map.size();
            };


            auto get_repr = [&](auto &from) -> shadowinst::Reg &
            {
                return *ifuzz::get_reg< I >(from[todo[0]]);
            };

            for (const auto &c : candidates)
            {
                // Make copy, so populate family can be easily called.
                auto n_s_inst = s_inst;
                for (auto idx : todo)
                    ifuzz::get_reg< I >(n_s_inst[idx]) = c;
                populate(n_s_inst, todo);
                auto &repr = get_repr(n_s_inst);
                if (has_only_singleton_mappings(repr) && grew(repr))
                    chosen = repr;
            }

            if (chosen == s_reg) {
                if (arch->address_size == 64)
                    log_kill() << "Reg enlargement heuristic did not choose any candidate!\n"
                        << rinst.Serialize() << "\n"
                        << s_inst.to_string();
                else {
                    log_error() << "Reg enlargement heuristic did not choose any candidate!\n"
                        << rinst.Serialize();
                    return true;
                }
            }

            for (auto idx : todo) {
                ifuzz::get_reg< I >(s_inst[idx]) = *chosen;
            }
            return true;
        }

        template<typename Yield>
        void _Permutate(std::string &bytes, const std::vector<uint64_t> &idxs,
                        std::size_t current, Yield &yield)
        {
            // I am done flipping, we look at the value now
            if (current == idxs.size()) {
                yield(bytes);
                return;
            }

            // We chose not to flip first
            _Permutate(bytes, idxs, current + 1, yield);
            auto byte = static_cast<uint8_t>(bytes[idxs[current] / 8 ]);
            uint8_t mask = 1;
            bytes[idxs[current] / 8]
                = static_cast< char >(byte ^ (mask << (7 - idxs[current] % 8)));
            _Permutate(bytes, idxs, current + 1, yield);
            // We are taking bytes as reference we need to flip back as we backtrack!
            byte = static_cast<uint8_t>(bytes[idxs[current] / 8 ]);
            bytes[idxs[current] / 8]
                = static_cast< char >(byte ^ (mask << (7 - idxs[current] % 8)));
        }
    };


    static inline shadowinst::Instruction fuzz_operands(const remill::Arch &arch,
                                                        const remill::Instruction &rinst)
    {
        return InstructionFuzzer{&arch, rinst}.FuzzOps();
    }

} // namespace circ
