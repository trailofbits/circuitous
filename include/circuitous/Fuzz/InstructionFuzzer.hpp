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

        std::size_t rinst_bitsize() const { return rinst.bytes.size() * 8; }

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

            using I = shadowinst::Address::idx;
            auto ob = [&](std::size_t i)
            {
                return shadowinst::ordered_bits_t(distributed_bits[i], rinst_bitsize());
            };

            s_addr.do_make< shadowinst::Reg >(I::base_reg, ob(0));
            s_addr.do_make< shadowinst::Reg >(I::index_reg, ob(1));
            s_addr.do_make< shadowinst::Immediate >(I::scale, ob(2));
            s_addr.do_make< shadowinst::Immediate >(I::displacement, ob(3));
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

                for (std::size_t op_i = 0; op_i < rinst.operands.size(); ++op_i)
                {
                    using namespace ifuzz::permutate;
                    auto item = to_item(rinst, op_i);
                    auto res = Comparator(arch).compare(rinst, *permutations[i], item);
                    op_bits[op_i][i] = DiffResult().get_result(res, item);
                    if (DiffResult().are_dirty(res,item))
                        dirts[op_i].insert(static_cast< uint32_t >(permutations.size() - 1 - i));
                }
            }
            return std::make_tuple(op_bits, dirts);
        }

        auto fuzz_ops(bool generate_all=true)
        {
            shadowinst::Instruction shadow_inst(rinst_bitsize(), rinst.bytes);
            auto [op_bits, dirts] = generate_bits();

            for (std::size_t i = 0; i < rinst.operands.size(); ++i)
            {
                shadowinst::ordered_bits_t obits(op_bits[i], rinst_bitsize());
                switch(rinst.operands[i].type)
                {
                    case OpType::kTypeRegister : {
                        shadow_inst.Add<shadowinst::Reg>(obits);
                        break;
                    }
                    case OpType::kTypeImmediate : {
                        shadow_inst.Add<shadowinst::Immediate>(obits);
                        break;
                    }
                    case OpType::kTypeAddress: {
                        auto &s_addr = shadow_inst.Add<shadowinst::Address>();
                        distribute_addr(op_bits[i], i, *s_addr.address());
                        break;
                    }
                    default:
                        unreachable() << "REFACTOR!";
                        break;
                }
            }

            // Check sanity
            check(shadow_inst.operands.size() == rinst.operands.size());

            for (std::size_t i = 0; i < shadow_inst.operands.size(); ++i)
            {
                if (!shadow_inst[i].is_husk()) {
                    shadow_inst.deps.push_back( {{i, &shadow_inst[i]}} );
                    shadow_inst.dirt.push_back( dirts[i] );
                }
            }


            // It is possible some operands were not properly populated
            // (due to being read+write operands), so they need special attention.
            HuskResolver(*this).resolve_husks(shadow_inst);
            if (generate_all)
                populate_translation_tables(shadow_inst);
            return shadow_inst;
        }

        void populate_translation_tables(shadowinst::Instruction &s_inst)
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
                if (!s_inst[idx].address() || s_inst[idx].address()->segment_reg()
                    || !has_reg< sel::base >(s_inst[idx]))
                {
                    return;
                }

            for (auto idx : todo)
            {
                auto segment = get_reg< sel::segment >(s_inst[idx]);
                dcheck(s_inst[idx].address(), [&](){ return "Expected addr to be present"; });
                if (!segment)
                    s_inst[idx].address()->do_make< shadowinst::Reg >(
                            shadowinst::Address::idx::segment_reg,
                            get_reg< sel::base >(s_inst[idx])->regions);
                else
                    *segment = shadowinst::Reg(get_reg< sel::base >(s_inst[idx])->regions);
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
                // REFACTOR(lukas): No longer make copies.
                auto &s_op = s_inst[i];
                auto &r_op = nrinst.operands[i];

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
                auto *reg = get_reg< I >(s_inst[idx]);
                check(reg) << "REFACTOR";
                if (!reg->empty() && reg->translation_map.empty())
                    reg->regions.clear();
            }
        }

        void coerce_by_segment_override(shadowinst::Instruction &s_inst,
                                        const idx_vector_t &todo)
        {
            for (auto idx : todo)
                check(shadowinst::compare(s_inst[idx].address()->segment_reg(),
                                          s_inst[todo[0]].address()->segment_reg()));
            if (!rinst.segment_override || !s_inst[todo[0]].address())
                return;

            auto front = s_inst[todo[0]].address()->segment_reg();
            if (!front)
                return;

            // TODO(lukas): Fix on remill side.
            auto seg_base = rinst.segment_override->name + "BASE";
            check(front->translation_map.has_only(seg_base)) << front->to_string();
            for (auto idx : todo)
            {
                auto segment_reg = s_inst[idx].address()->segment_reg();
                segment_reg->clear();
                segment_reg->tm()[seg_base] = {{}};
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
                if (!ifuzz::get_reg< I >(s_inst[idx]))
                    return false;
            return try_populate_reg_< I >(s_inst, todo);
        }

        template< uint32_t I >
        bool try_populate_addr(shadowinst::Instruction &s_inst, const idx_vector_t &todo)
        {
            for (auto idx : todo)
            {
                auto addr = s_inst[idx].address();
                if (!addr || !ifuzz::get_reg< I >(s_inst[idx])) {
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
                out.push_back(byte & (1u << (idx % 8)));
            }
            return out;
        }

        std::vector< uint64_t > collect_idxs(const shadowinst::has_regions &hr)
        {
            std::vector< uint64_t > idxs;
            for (const auto &[from_, size_] : hr.regions.areas) {
                auto size = static_cast< int64_t >(size_);
                auto from = static_cast< int64_t >(from_);
                for (int64_t afrom = from + size - 1; afrom >= from; --afrom)
                {
                    idxs.push_back(static_cast< uint64_t >(afrom));
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

            permutate(nbytes, idxs, 0, preprocess);
        }

        std::vector< shadowinst::has_regions > get_candidates(
            const shadowinst::has_regions &original)
        {
            auto [from_, size_] = original.regions.biggest_chunk();
            auto size = size_;
            // 3 bits are okay since we do not always need REX prefix.
            // 4 and more bits are most likely an error in decoding and we definitely
            // do not want to add even more.
            if (size == 3 || original.regions.marked_size() >= 4)
                return {};

            using maybe_region_t = std::optional< shadowinst::has_regions >;

            auto from = from_;

            std::vector< shadowinst::has_regions > candidates;

            // REFACTOR(lukas): Remove copy & paste.
            if (size == 2)
            {
                // copy
                auto n1 = original.regions;
                if (n1.extend_left(from) && ifuzz::is_reg_octet(from - 1, n1.areas[from - 1]))
                    candidates.emplace_back(std::move(n1));
                // copy
                auto n2 = original.regions;
                if (n2.extend_right(from) && ifuzz::is_reg_octet(from, n2.areas[from]))
                    candidates.emplace_back(std::move(n2));
            }

            if (size == 1)
            {
                // copy
                auto n3 = original.regions;
                for (auto hole : original.regions.get_holes())
                {
                    if (n3.merge_areas(hole - 1, hole + 1, 1) &&
                        ifuzz::is_reg_octet(hole - 1, n3.areas[hole - 1]))
                    {
                        candidates.emplace_back(std::move(n3));
                    }
                }
            }
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
            check(ifuzz::has_reg< I >(s_inst[idx])) << idx << " " << I;
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
            if (!s_inst[todo[0]].address())
                return;
            auto flattened = s_inst[todo[0]].address()->flatten_significant_regs();
            for (auto from : dirt)
                flattened.add({from, 1});

            auto raw_candidates = get_candidates(shadowinst::has_regions(flattened));
            if (flattened.marked_size() == 1)
            {
                auto [from, size] = *flattened.areas.begin();
                if (ifuzz::is_reg_octet(from, size))
                    raw_candidates.push_back(shadowinst::has_regions(flattened));
            }
            if (raw_candidates.empty())
                return;

            check(raw_candidates.size() == 1);
            auto &raw_candidate = raw_candidates[0];

            // Explicit copy!
            auto n_s_inst = s_inst;
            for (auto idx : todo)
            {
                check(n_s_inst[idx].address()->base_reg() &&
                      n_s_inst[idx].address()->index_reg());
                *n_s_inst[idx].address()->base_reg() = shadowinst::Reg(raw_candidate.regions);
                *n_s_inst[idx].address()->index_reg() = shadowinst::Reg(raw_candidate.regions);
            }

            populate_whole_addr(n_s_inst, todo, raw_candidate);

            for (auto idx : todo) {
                using namespace ifuzz;
                check(shadowinst::compare(n_s_inst[idx].address(),
                                          n_s_inst[todo[0]].address()));
            }

            for (auto idx : todo) {
                auto index = ifuzz::get_reg< ifuzz::sel::index >(n_s_inst[idx]);
                check(index) << "REFACTOR!";
                if (index->translation_map.mappings_count() != 1)
                    break;
                const auto &[key, _] = *index->translation_map.begin();
                if (std::string_view(key).starts_with("__remill_zero_"))
                {
                    index->regions.clear();
                    index->translation_map.clear();
                }
            }

            for (auto idx : todo) {
                if (!n_s_inst[idx].address()->base_reg()->translation_map.empty())
                    *s_inst[idx].address()->base_reg() = *n_s_inst[idx].address()->base_reg();
                if (!n_s_inst[idx].address()->index_reg()->translation_map.empty())
                    *s_inst[idx].address()->index_reg() = *n_s_inst[idx].address()->index_reg();
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

            check(s_reg) << "REFACTOR";
            shadowinst::Reg chosen = *s_reg;
            auto grew = [&](auto &fst) {
                return chosen.translation_map.mappings_count() < fst.translation_map.mappings_count();
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
                {
                    check( ifuzz::get_reg< I >(n_s_inst[idx]) ) << "REFACTOR";
                    *ifuzz::get_reg< I >(n_s_inst[idx]) = c;
                }
                populate(n_s_inst, todo);
                auto &repr = get_repr(n_s_inst);
                if (has_only_singleton_mappings(repr) && grew(repr))
                    chosen = repr;
            }

            if (chosen == *s_reg) {
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
                check(ifuzz::get_reg< I >(s_inst[idx])) << "REFACTOR";
                *ifuzz::get_reg< I >(s_inst[idx]) = chosen;
            }
            return true;
        }

        template<typename Yield>
        void permutate(std::string &bytes, const std::vector<uint64_t> &idxs,
                        std::size_t current, Yield &yield)
        {
            // I am done flipping, we look at the value now
            if (current == idxs.size()) {
                yield(bytes);
                return;
            }

            // We chose not to flip first
            permutate(bytes, idxs, current + 1, yield);
            auto byte = static_cast<uint8_t>(bytes[idxs[current] / 8 ]);
            uint8_t mask = 1;
            bytes[idxs[current] / 8]
                = static_cast< char >(byte ^ (mask << (idxs[current] % 8)));
            permutate(bytes, idxs, current + 1, yield);
            // We are taking bytes as reference we need to flip back as we backtrack!
            byte = static_cast<uint8_t>(bytes[idxs[current] / 8 ]);
            bytes[idxs[current] / 8]
                = static_cast< char >(byte ^ (mask << (idxs[current] % 8)));
        }
    };


    static inline shadowinst::Instruction fuzz_operands(const remill::Arch &arch,
                                                        const remill::Instruction &rinst)
    {
        return InstructionFuzzer{&arch, rinst}.fuzz_ops();
    }

} // namespace circ
