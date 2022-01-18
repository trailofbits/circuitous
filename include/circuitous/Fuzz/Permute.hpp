/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */

#pragma once

#include <circuitous/Support/Check.hpp>
#include <circuitous/Support/Log.hpp>
#include <circuitous/Util/LLVMUtil.hpp>
#include <circuitous/Fuzz/DiffResult.hpp>


#include <remill/Arch/Arch.h>
#include <remill/Arch/Instruction.h>

namespace circ::ifuzz::permutate
{

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
        const remill::Instruction &rinst, const remill::Arch *arch)
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

    struct Verbose
    {
        std::stringstream ss;

        template<typename S>
        void _raw(S &&s) { ss << std::forward<S>(s); }

        void _raw(const remill::Instruction &inst)
        {
            ss << inst.Serialize();
        }

        template<typename H, typename ...Args>
        void _log(const std::string &delim, H &&h, Args &&...args)
        {
            _raw(std::forward<H>(h));
            if constexpr (sizeof...(Args) != 0) {
                ss << delim;
                return _log(delim, std::forward<Args>(args)...);
            } else {
                ss << std::endl;
            }
        }

        template<typename Out, typename ...Args>
        auto dbg_neq(Out &&out, uint8_t indent, Args &&...args)
        {
            _log(std::string(indent * 2, ' '), std::forward<Args>(args)...);
            return std::forward< Out >(out);
        }

        template<typename ...Args>
        void dbg(uint8_t indent, Args &&...args)
        {
            _log(std::string(indent * 2, ' '), std::forward<Args>(args)...);
        }
    };

    // TODO(lukas): Actually this is a few separate components in one template stack.
    //              It would be best if components could be identified and refactored.
    // Everything equals to everything
    struct TrueBase : Verbose
    {
        using OpType = remill::Operand::Type;
        using operands_t = std::vector<const remill::Operand *>;

        const remill::Arch *arch;

        TrueBase(const remill::Arch *arch_) : arch(arch_) {}

        auto identity_imm() { return [](auto &, auto &) { return true; }; }
        auto identity_reg() { return [](auto &, auto &) { return true; }; }

        bool full_compare(const remill::Operand &lhs, const remill::Operand &rhs)
        {
            return lhs.Serialize() == rhs.Serialize();
        }

        bool full_compare(const remill::Register *lhs, const remill::Register *rhs)
        {
            if (!lhs && !rhs)
                return true;
            if (!lhs || !rhs)
                return false;
            return lhs->name == rhs->name;
        }

        bool Depends(const operands_t &) { return true; }
    };

    template<typename Next>
    struct UnitCompares : Next
    {
        using OpType = typename Next::OpType;
        using Next::Next;

        static auto identity_imm()
        {
            return [](auto &self, auto &flipped) -> diff_result {
                // TODO(lukas): Check in remill that is_signed is always properly
                //              (and consistently) set.
                // This may seem counterintuitive, but two immediates are equal
                // even if their values are not equal -- actaully that is the whole
                // point.
                return ( flipped.type == remill::Operand::kTypeImmediate &&
                         self.size == flipped.size )
                  ? diff_result::pure
                  : diff_result::unrelated;
            };
        }

        auto identity_reg()
        {
            return [&](auto &self, auto &flipped) -> diff_result {
                return (flipped.type == OpType::kTypeRegister &&
                        self.reg.size == flipped.reg.size)
                  ? diff_result::pure
                  : diff_result::unrelated;
            };
        }

        auto identity_addr()
        {
            return [&](auto &self_, auto& flipped_) -> diff_result
            {
                if (flipped_.type != OpType::kTypeAddress)
                    return diff_result::unrelated;

                auto &self = self_.addr;
                auto &flipped = flipped_.addr;
                uint8_t corrections = 0;


                auto changed_size = [](const auto &lhs, const auto &rhs) {
                    if (lhs.size == 0 || rhs.size == 0)
                        return false;
                    return lhs.size != rhs.size;
                };

                if (changed_size(self.index_reg, flipped.index_reg) ||
                    changed_size(self.base_reg, flipped.base_reg))
                {
                    this->dbg(0, "Different reg sizes.");
                    return diff_result::unrelated;
                }

                // RSP in base reg requires some bits set elsewhere.
                if ((self.base_reg.name != flipped.base_reg.name) &&
                    (flipped.base_reg.name == "NEXT_PC"))
                {
                    return diff_result::unrelated;
                }

                if (self.base_reg.name.empty() != flipped.base_reg.name.empty())
                {
                    this->dbg(0, "Cannot (un)conjure base_reg.");
                    return diff_result::unrelated;
                }

                if (self.index_reg.name.empty() != flipped.index_reg.name.empty())
                {
                    this->dbg(0, "Cannot (un)conjure index_reg.");
                    return diff_result::unrelated;
                }

                // Base
                if (self.base_reg.size != flipped.base_reg.size ||
                   (self.base_reg.name != flipped.base_reg.name))
                {
                    ++corrections;
                    // Since bases are equal, segments should too as they depend on it.
                } else {
                    if (self.segment_base_reg.name != flipped.segment_base_reg.name ||
                        self.segment_base_reg.size != flipped.segment_base_reg.size)
                    {
                        this->dbg(0, "Same base produced different segment");
                        return diff_result::unrelated;
                    }
                }

                // Index * scale (they must be checked together since they can be both missing)
                uint8_t index_corrections = 0;
                if (self.index_reg.size != flipped.index_reg.size ||
                    (self.index_reg.name != flipped.index_reg.name))
                {
                    ++index_corrections;
                }
                if (self.scale != flipped.scale)
                    ++index_corrections;

                if (self.index_reg.name.empty() || flipped.index_reg.name.empty())
                    corrections += (index_corrections) ? 1 : 0;
                else
                    corrections += index_corrections;

                // Displacement
                if (self.address_size != flipped.address_size)
                    return diff_result::unrelated;

                if (self.displacement != flipped.displacement)
                  ++corrections;

                this->dbg(0, "Corrections needed:", static_cast<uint32_t>(corrections));
                return (corrections <= 1) ? diff_result::pure : diff_result::dirty;
            };
        }
    };

    template<typename Next>
    struct EqDep_ : Next
    {
        using Next::Next;

        template<typename R>
        bool reg_dependency(const R &a, const R &b)
        {
            return a.name == b.name && a.size == b.size;
        }

        template<typename A>
        bool addr_dependency(const A &a, const A &b)
        {
            return reg_dependency(a.base_reg, b.base_reg)
                   && reg_dependency(a.index_reg, b.index_reg)
                   && a.scale == b.scale
                   && a.displacement == b.displacement
                   && a.address_size == b.address_size;
        }
    };

    template< typename Next >
    struct EqDepAmd64Hack_ : Next
    {
        using Next::Next;

        template< typename R >
        bool reg_dependency(const R &a, const R &b)
        {
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
    struct EqDepFront : Next
    {
        using OpType = typename Next::OpType;
        using Next::Next;

        bool Depends(const std::vector< const remill::Operand * > &ops)
        {
            for (auto op : ops) {
                check(op->type == OpType::kTypeRegister || op->type == OpType::kTypeAddress)
                    << "Can check eq dependency only on reg and addr operands.";
            }
            auto compare = [=](auto a, auto b) {
                switch (a->type) {
                  case OpType::kTypeRegister:
                      return this->Next::reg_dependency(a->reg, b->reg);
                  case OpType::kTypeAddress:
                      return this->Next::addr_dependency(a->addr, b->addr);
                  default:
                      unreachable() << "Unreachable, should be caught be ealier assert";
                }
            };

            auto &fst = *(ops.begin());
            for (auto op : ops)
                if (!compare(fst, op))
                    return false;
            return true;
        }
    };

    template< typename N >
    using EqDep = EqDepFront< EqDep_< N > >;
    template< typename N >
    using EqDepAmd64Hack = EqDepFront< EqDepAmd64Hack_< EqDep_< N > > >;

    using Item_t = std::map< std::size_t, const remill::Operand * >;

    template< typename Next >
    struct DependencyComparator : Next
    {
        using cri = const remill::Instruction &;

        using Next::Next;

        template< typename Fn >
        struct_check_result_t check_structure(cri original, cri permutation,
                                              const Item_t &items, Fn &&on_self)
        {
            if (original.bytes.size() != permutation.bytes.size())
                return this->dbg_neq(std::nullopt, 1, "Sizes did not match.");

            if (original.function != permutation.function)
                return this->dbg_neq(std::nullopt, 1, "Function did not match.");

            // NOTE(lukas): Unfortunately we can have the following:
            // `64 44 xyz`
            // `64 64 44 xyz`
            // and if `44` (valid REX prefix) is changed to `64` (segment override)
            // this comparison will not notice it, as the segment override is always present
            // which is really unfortunate.
            if (!Next::full_compare(original.segment_override, permutation.segment_override))
                return this->dbg_neq(std::nullopt, 1, "Full compare failed");

            if (original.operands.size() != permutation.operands.size())
                return this->dbg_neq(std::nullopt, 1, "Operands size did not match.");

            struct_icheck_res_t out(original.operands.size(), diff_result::unknown);
            for (std::size_t i = 0U; i < permutation.operands.size(); ++i)
            {
                // Will be handled later
                if (items.count(i))
                    continue;

                if (Next::full_compare(original.operands[i], permutation.operands[i]))
                    out[i] = diff_result::exact;
            }

            auto lift_compare_res = [](auto b) {
                return b ? diff_result::exact : diff_result::unknown;
            };

            for (auto [i, op] : items)
            {
                auto exact_check = Next::full_compare(*op, permutation.operands[i]);
                auto on_self_check = on_self(*op, permutation.operands[i]);
                out[i] = join(lift_compare_res(exact_check), on_self_check);
                this->ss << to_string(out[i]) << " = join( "
                         << to_string(lift_compare_res(exact_check))
                         << ", " << to_string(on_self_check) << " )\n";
            }

            // Check if there is the assumed relationship.
            // Often, it can be the same as the `exact_check`, but not always.
            std::vector< const remill::Operand * > raw_items;
            for (auto [_, op] : items)
                raw_items.push_back(op);
            if (!Next::Depends(raw_items))
            {
                this->dbg(1, "Next::Depends failed.");
                return std::nullopt;
            }
            return std::make_optional(std::move(out));
        }
    };

    // `exact_mod` allows to modify the behaviour of `Check` method if the operands of interest
    // are exactly the same in both instructions.
    // NOTE(lukas): See permutation generation and register enlarging heuristics for examples
    //              of when this was flag is used.
    template< typename Next, bool exact_mod_ = false >
    struct Dispatch : Next
    {
        using OpType = typename Next::OpType;
        using cri = const remill::Instruction;
        using result_t = struct_check_result_t;

        static inline constexpr bool exact_mod = exact_mod_;

        Dispatch(const remill::Arch *arch_) : Next(arch_) {}

        OpType get_op_type(const Item_t &items)
        {
            check(items.size() >= 1) << "Cannot compare " << items.size() << " items.";
            OpType type = items.begin()->second->type;
            for (auto [_, op] : items)
            {
                check(op->type == type)
                    << "Cannot compare as group operands of different types!";
            }
            return type;
        }

        result_t compare(cri &original, cri &permuation, std::size_t idx)
        {
            return compare(original, permuation, {{ idx, &original.operands[idx] }});
        }

        result_t compare(cri &original, cri &permutation, const Item_t &items)
        {
            auto type = get_op_type(items);
            auto exec = [&](auto identity) {
                return investigate(original, permutation, items, identity);
            };

            switch(type)
            {
                case OpType::kTypeImmediate: return exec(Next::identity_imm());
                case OpType::kTypeRegister : return exec(Next::identity_reg());
                case OpType::kTypeAddress  : return exec(Next::identity_addr());
                default                    : return {};
            }
        }

        template< typename ... Args >
        result_t verbose_compare( Args && ... args )
        {
            auto x = compare(std::forward< Args >(args) ...);
            log_info() << this->ss.str();
            return x;
        }

        template< typename Fn >
        result_t investigate(cri &original, cri &permutation, const Item_t &op, Fn &&on_self)
        {
            return Next::check_structure(original, permutation, op, on_self);
        }
    };

    static inline Item_t to_item(const remill::Instruction &from, std::size_t idx)
    {
        return {{ idx, &from.operands[idx] }};
    }

    template< bool exact_mod = false >
    struct DiffResult
    {
        bool are_exact(const struct_check_result_t &res, const Item_t &items)
        {
            if (!res) return false;

            for (const auto &[i, _] : items)
                if ((*res)[i] != diff_result::exact)
                    return false;
            return true;
        }

        bool are_preserved(const struct_check_result_t &res, const Item_t &items)
        {
            if (!res) return true;

            for (std::size_t i = 0; i < res->size(); ++i)
                if (!items.count(i) && (*res)[i] != diff_result::exact)
                    return false;
            return true;
        }

        bool are_struct(const struct_check_result_t &res, const Item_t &items)
        {
            if (!res) return false;

            for (const auto &[i, _] : items)
            {
                auto r = (*res)[i];
                if (r != diff_result::pure && r != diff_result::exact)
                    return false;
            }
            return true;
        }

        bool are_dirty(const struct_check_result_t &res, const Item_t &item)
        {
            if (!res || !are_preserved(res, item))
                return false;
            for (const auto &[i, _] : item) {
                auto r = (*res)[i];
                if (r != diff_result::dirty)
                    return false;
            }
            return true;
        }

        bool get_result(const struct_check_result_t &res, const Item_t &items)
        {
            return are_preserved(res, items) && are_struct(res, items)
                && (exact_mod || !are_exact(res, items));
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
