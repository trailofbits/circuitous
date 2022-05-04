/*
 * Copyright (c) 2021 Trail of Bits, Inc.
 */
#pragma once

#include <cstdint>
#include <cmath>
#include <deque>
#include <iostream>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <remill/Arch/Instruction.h>

#include <circuitous/Support/Check.hpp>
#include <circuitous/Support/Log.hpp>

#include <circuitous/Util/InstructionBytes.hpp>
#include <circuitous/Util/LLVMUtil.hpp>

namespace circ::shadowinst
{

    static inline std::string to_binary(const std::string &bytes)
    {
        std::stringstream ss;
        for (char byte_ : bytes)
        {
            auto byte = static_cast< uint8_t >(byte_);
            for (int b = 7; b >= 0; --b)
                ss << static_cast< uint16_t >(byte >> b & 1u);
        }
        return ss.str();
    }

    static inline std::string to_hex(const std::string &bytes)
    {
        std::stringstream ss;
        for (auto byte : bytes) {
            ss << std::hex;
            if (static_cast<uint8_t>(byte) < 16)
                ss << "0";

            ss << static_cast<unsigned>(static_cast<uint8_t>(byte));
        }
        return ss.str();
    }

    // This describes a region of decoded bytes where
    // it makes sense to talk about an "entity" -- register, immediate operand,
    // etc. While it will usually be one contignous entry, we don't need to constraint
    // ourselves yet.
    // {from, size}
    using region_t = std::map<uint64_t, uint64_t>;
    using maybe_region_t = std::optional<region_t>;

    static inline region_t Invert(region_t what, uint64_t lenght)
    {
        region_t out;
        uint64_t current = 0;
        for (auto &[from, size] : what) {
            if (current != from)
                out[current] = from - current;

            current = from + size;
        }

        if (current != lenght)
            out[current] = lenght - current;
        return out;
    }

    static inline region_t FromToFormat(const region_t &region)
    {
        region_t out;
        for (auto &[from, size] : region)
            out[from] = from + size;
        return out;
    }

    using bits_t = std::vector< bool >;

    struct ordered_bits_t
    {
        bits_t data;

        explicit ordered_bits_t(const bits_t &data_)
        {
            data.reserve(data_.size());
            data.insert(data.end(), data_.begin(), data_.end());
        }
    };

    struct has_regions
    {
        region_t regions;

        has_regions() = default;

        has_regions(const ordered_bits_t &bits_)
        {
            auto &bits = bits_.data;
            for (std::size_t i = 0; i < bits.size(); ++i)
            {
                if (!bits[i])
                    continue;

                uint64_t offset = i;
                uint32_t count = 0;
                for(; i < bits.size() && bits[i]; ++i)
                    ++count;

                regions.emplace(offset, count);
            }
        }

        has_regions(const bits_t &bits) : has_regions(ordered_bits_t(bits)) {}
        has_regions(has_regions &&) = default;
        has_regions(const has_regions &) = default;

        has_regions &operator=(has_regions &&) = default;
        has_regions &operator=(const has_regions &) = default;

        has_regions(region_t o) : regions(std::move(o)) {}

        bool operator==(const has_regions &) const = default;

        std::size_t region_bitsize() const
        {
            std::size_t acc = 0;
            for (auto &[from, size] : regions)
                acc += size;
            return acc;
        }

        std::vector< uint64_t > region_idxs() const
        {
            std::vector<uint64_t> idxs;
            for (auto &[from, size] : regions)
                for (uint64_t i = 0; i < size; ++i)
                    idxs.push_back(from + i);
            return idxs;
        }

        auto begin() const { return regions.begin(); }
        auto end() const { return regions.end(); }
        auto size() const { return regions.size(); }
        auto empty() const { return size() == 0; }

        auto present(std::size_t idx) const
        {
            for (auto &[from, size] : regions)
                if (idx >= from && idx < from + size)
                    return true;

            return false;
        }

        std::string to_string(std::size_t indent=0) const
        {
            std::stringstream ss;
            for (auto [from, size] : regions)
                ss << std::string(indent * 2, ' ') << from << " , " << size << std::endl;

            return ss.str();
        }

        auto biggest_chunk() const
        {
            std::tuple< uint64_t, uint64_t > out{ 0, 0 };
            for (auto &[from, size] : regions)
                if (size > std::get<1>(out))
                    out = {from, size};

            return out;
        }

        std::optional< uint64_t > get_hole() const
        {
            std::vector< uint64_t > out;
            for (auto &[from, size] : regions)
            {
                auto it = regions.find(from + 2);
                if (it == regions.end() || size != 1)
                    continue;

                if (it->second == 1)
                    out.push_back(from + 1);
            }
            if (out.size() == 1)
                return { out[0] };
            return {};
        }

        // REFACTOR(lukas): Not ideal.
        template< typename T >
        static auto is_empty(const std::optional< T > &x)
        -> std::enable_if_t< std::is_base_of_v< has_regions, T >, bool >
        {
            return !x || x->empty();
        }

        template< typename T >
        static auto is_empty(const T *x)
        -> std::enable_if_t< std::is_base_of_v< has_regions, T >, bool >
        {
            return !x || x->empty();
        }

        void pre(uint64_t anchor, uint64_t from, uint64_t size)
        {
            auto original_end = anchor + regions[anchor];
            regions.erase(anchor);
            regions[from] = std::max(original_end - from, size);
        }

        void post(uint64_t anchor, uint64_t from, uint64_t size)
        {
            regions[anchor] = std::max(regions[anchor], from + size - anchor);
        }

        void add(uint64_t from, uint64_t size)
        {
            for (const auto &[x, y] : regions)
            {
                if (x <= from && x + y >= from)
                    return post(x, from, size);
                if (from <= x && from + size >= x)
                    return pre(x, from, size);
            }
            regions[from] = size;
        }

        void add(const has_regions &other)
        {
            // It is expected both are rather small
            for (const auto &[from, size] : other.regions)
                add(from, size);
        }
    };

    // It is expected `Materialization_t` is iterable and one item corresponds to one bit.
    // TODO(lukas): Remove assumption that item is castable to bool and that represents
    //              '1' or '0' depending on the casted value. Provide convertor to
    //              either `llvm::APInt` or `std::string` that is the textual repr in binary?
    // TODO(lukas): Enforce that you can insert only valid materialization w.r.t to bitsize.
    template< typename Key_t, typename Materialization_t >
    struct TranslationMap : protected std::map< Key_t, std::unordered_set< Materialization_t > >
    {
        using materialized_t = Materialization_t;
        using materializations_t = std::unordered_set< materialized_t >;
        using key_t = Key_t;

        using storage_t = std::map< key_t, materializations_t >;

        std::size_t bitsize = 0;
      public:

        TranslationMap(std::size_t bitsize_) : bitsize(bitsize_) {}
        TranslationMap(const TranslationMap &) = default;
        TranslationMap(TranslationMap &&) = default;

        TranslationMap &operator=(const TranslationMap &) = default;
        TranslationMap &operator=(TranslationMap &&) = default;

        using storage_t::operator[];
        using storage_t::empty;
        using storage_t::begin;
        using storage_t::end;
        using storage_t::count;
        using storage_t::find;

        // TODO(lukas): See if really needed.
        bool has_only(const key_t &k) const
        {
            return mappings_count() >= 1 && this->begin()->first == k;
        }
        // TODO(lukas): See if really needed.
        void clear()
        {
            this->storage_t::clear();
        }

        template< typename T = std::size_t >
        T max_mats_count() const
        {
            return static_cast< T >(std::pow(2, bitsize));
        }

        bool is_saturated() const
        {
            return mats_count() == max_mats_count();
        }

        std::size_t mappings_count() const { return this->size(); }
        std::size_t mats_count() const
        {
            std::size_t acc = 0;
            for (const auto &[_, v] : *this)
                acc += v.size();
            return acc;
        }

        template< typename T, typename Convert >
        std::map< T, key_t > reverse_map(Convert &&convert) const
        {
            std::map< T, key_t > out;
            for (auto &[ k, encodings ] :*this)
                for (auto &encoding : encodings)
                    out[convert(encoding)] = k;
            return out;
        }

        static std::string make_bitstring(const Materialization_t &from)
        {
            std::string out;
            for (std::size_t i = 0; i < from.size(); ++i)
                out += (from[i]) ? '1' : '0';

            return out;
        }

        std::map< std::string, key_t > reverse_bitmap() const
        {
            return reverse_map< std::string >(make_bitstring);
        }

        bool is_saturated_by_zeroes() const
        {
            if (mappings_count() != 1)
                return false;
            const auto &[key, encodings] = *this->begin();
            if (!std::string_view(key).starts_with("__remill_zero_i"))
                return false;

            return encodings.size() == max_mats_count();
        }

        std::string to_string(std::size_t indent) const
        {
            auto make_indent = [](std::size_t i) { return std::string(i, ' '); };

            std::stringstream ss;

            for (const auto &[k, vs] : *this)
            {
                ss << make_indent(indent) << k << std::endl;

                if (vs.empty())
                {
                    ss << "( none )";
                } else {
                    for (const auto &v : vs)
                        ss << make_indent(indent + 1) << make_bitstring(v);
                }

                ss << std::endl;
            }
            return ss.str();
        }

        auto complement() const -> std::vector< llvm::APInt >
        {
            constexpr auto hash = [](const auto &x) { return llvm::hash_value(x); };
            std::unordered_set< llvm::APInt, decltype(hash) > seen;
            // TODO(lukas): We may not need this check.
            std::optional< uint32_t > size;

            for (const auto &[reg, mats] : *this)
                for (const auto &mat : mats)
                {
                    if (!size)
                        size = mat.size();
                    check(*size == mat.size());
                    auto as_apint = make_APInt(mat, 0, mat.size());
                    seen.insert(as_apint);
                }

            std::vector< llvm::APInt > out;
            auto v = llvm::APInt(*size, 0, false);
            uint32_t max_v = 1 << *size;
            check(size);
            for (uint32_t i = 0; i < max_v; ++i)
            {
                if (!seen.count(v))
                    out.push_back(v);
                ++v;
            }
            check(seen.size() + out.size() == max_v) << seen.size() << " + " << out.size()
                                                     << " != " << max_v;
            return out;
        }
    };

    template< typename K, typename V >
    std::ostream &operator<<(std::ostream &os, const TranslationMap< K, V > &tm)
    {
        return os << tm.to_string();
    }

    struct Reg : has_regions
    {
        // Actual instances for given reg
        using materialized_t = std::vector< bool >;
        using reg_t = std::string;
        using TM_t = TranslationMap< reg_t, materialized_t >;

        using has_regions::has_regions;

        TM_t translation_map;
        std::unordered_set< reg_t > dirty;

        std::optional< std::size_t > selector;

        Reg(const ordered_bits_t &bits_)
            : has_regions(bits_), translation_map(this->region_bitsize())
        {}

        Reg(const bits_t &bits) : Reg(ordered_bits_t(bits)) {}

        Reg(const region_t &o)
            : has_regions(o), translation_map(this->region_bitsize())
        {}

        auto &tm() { return translation_map; }
        const auto &tm() const { return translation_map; }

        std::string to_string(std::size_t indent=0) const
        {
            std::stringstream ss;
            std::string _indent(indent * 2, ' ');

            ss << _indent << "Regions:" << std::endl;
            ss << this->has_regions::to_string(indent + 1);
            ss << _indent << "Translation map:" << std::endl;

            ss << translation_map.to_string(indent + 1);
            return ss.str();
        }

        // REFACTOR(lukas): Remove.
        bool is_dirty(const reg_t &reg) const
        {
            return dirty.count(reg);
        }

        void mark_dirty(const reg_t &reg)
        {
            check(translation_map.count(reg));
            dirty.insert(reg);
        }
        void for_each_present(auto &cb) const { cb(*this); }
    };

    struct Immediate : has_regions
    {
        using has_regions::has_regions;

        void for_each_present(auto &cb) const { cb(*this); }
    };

    struct Address
    {
        using maybe_reg_t = std::optional< Reg >;
        using maybe_imm_t = std::optional< Immediate >;

        struct idx
        {
            static inline constexpr std::size_t base_reg = 0u;
            static inline constexpr std::size_t index_reg = 1u;
            static inline constexpr std::size_t segment_reg = 2u;

            static inline constexpr std::size_t scale = 0u;
            static inline constexpr std::size_t displacement = 1u;
        };

        std::array< maybe_reg_t, 3 > _regs;
        std::array< maybe_imm_t, 2 > _imms;

        Address() = default;

        template< typename T, typename Arr >
        static T try_fetch(Arr &arr, std::size_t idx)
        {
            auto &v = arr[idx];
            return (v) ? &*v : nullptr;
        }

        template< typename O >
        static auto to_pointer(O &from)
        {
            return (from) ? &*from : nullptr;
        }

        #define circ_shadowinst_Address_make_getter(name, type, field) \
            type name() { return try_fetch< type >(field, idx::name); } \
            const type name() const { return try_fetch< const type >(field, idx::name); }

        circ_shadowinst_Address_make_getter(base_reg, Reg *, _regs);
        circ_shadowinst_Address_make_getter(index_reg, Reg *, _regs);
        circ_shadowinst_Address_make_getter(segment_reg, Reg *, _regs);

        circ_shadowinst_Address_make_getter(scale, Immediate *, _imms);
        circ_shadowinst_Address_make_getter(displacement, Immediate *, _imms);

        #undef circ_shadowinst_Address_make_getter

        template< typename T, typename ... Args  >
        auto do_make(std::size_t idx, Args &&... args)
        -> std::enable_if_t< std::is_same_v< T, Reg >, Reg &>
        {
            _regs[idx] = std::make_optional(std::forward< Args >(args) ...);
            return *_regs[idx];
        }

        template< typename T, typename ... Args  >
        auto do_make(std::size_t idx, Args &&... args)
        -> std::enable_if_t< std::is_same_v< T, Immediate >, Immediate &>
        {
            _imms[idx] = std::make_optional(std::forward< Args >(args) ...);
            return *_imms[idx];
        }

        template< typename T, std::size_t idx, typename ... Args >
        T &make(Args && ... args)
        {
            return do_make< T >(idx, std::forward< Args >(args) ... );
        }

        bool operator==(const Address &) const = default;

        template< typename Self, typename C >
        static void _for_each_present(Self &self, C &&c)
        {
            for (auto &r : self._regs)
                if (r)
                    c(*r);
            for (auto &i : self._imms)
                if (i)
                    c(*i);

        }

        template<typename C> void for_each_present(C &&c)
        {
            _for_each_present(*this, std::forward<C>(c));
        }
        template<typename C> void for_each_present(C &&c) const
        {
            _for_each_present(*this, std::forward<C>(c));
        }

        bool empty() const
        {
            // REFACTOR(lukas): Think of api.
            return has_regions::is_empty(base_reg()) &&
                   has_regions::is_empty(index_reg()) &&
                   has_regions::is_empty(scale()) &&
                   has_regions::is_empty(displacement());
        }

        has_regions flatten_significant_regs() const
        {
            has_regions out;
            auto exec = [&](const auto &other) { out.add(other); };
            auto invoke = [&](const auto &on_what) { if (on_what) exec(*on_what); };
            invoke(base_reg());
            invoke(index_reg());
            return out;
        }

        // REFACTOR(lukas): Copy pasta.
        bool present(std::size_t idx) const
        {
            bool out = false;
            auto collect = [&](const auto &op) { out |= op.present(idx); };
            for_each_present(collect);
            return out;
        }

        std::string to_string(std::size_t indent) const
        {
            auto make_indent = [&](auto count) { return std::string(count * 2, ' '); };

            std::stringstream ss;
            auto format = [&](const auto &what, const std::string &prefix) {
                ss << make_indent(indent) << prefix << ": " << std::endl;
                if (what)
                    ss << what->to_string(indent + 1);
                else
                    ss << make_indent(indent + 1u) << "( not set )\n";
            };

            format(base_reg(), "Base");
            format(index_reg(), "Index");
            format(segment_reg(), "Segment");
            format(scale(), "Scale");
            format(displacement(), "Displacement");

            return ss.str();
        }
    };

    template< typename T >
    static inline bool compare(const T *lhs, const T *rhs)
    {
        if (!lhs && !rhs)
            return true;
        if (!lhs || !rhs)
            return false;
        return *lhs == *rhs;
    }


    struct Shift : has_regions
    {
        using has_regions::has_regions;
        void for_each_present(auto &cb) const { cb(*this); }
    };

    struct Operand
    {
        using self_t = Operand;
        using op_type = remill::Operand::Type;

      protected:
        std::variant< Immediate, Reg, Address, Shift > _data;

        // Simply forwarded to the variant ctor.
        template< typename ... Args >
        Operand(Args && ...args) : _data(std::forward< Args >(args) ...) {}

      public:

        Immediate *immediate() { return std::get_if< Immediate >(&_data); }
        Reg       *reg() { return std::get_if< Reg >(&_data); }
        Address   *address() { return std::get_if< Address >(&_data); }
        Shift     *shift() { return std::get_if< Shift >(&_data); }

        const Immediate *immediate() const { return std::get_if< Immediate >(&_data); }
        const Reg       *reg()       const { return std::get_if< Reg >(&_data); }
        const Address   *address()   const { return std::get_if< Address >(&_data); }
        const Shift     *shift()     const { return std::get_if< Shift >(&_data); }

        bool operator==(const Operand &) const = default;

        template< typename T, typename ... Args >
        static self_t make(Args && ... args) {
            if constexpr (std::is_same_v< T, Address >)
                return self_t(std::in_place_type_t< T >());
            else
                return self_t(std::in_place_type_t< T >(), std::forward< Args >(args) ...);
        }

        template< typename ...Args >
        static self_t make(op_type type, Args && ...args)
        {
            switch(type)
            {
                case remill::Operand::Type::kTypeRegister: {
                    return Operand::make< Reg >(std::forward<Args>(args)...);
                }
                case remill::Operand::Type::kTypeImmediate : {
                    return Operand::make< Immediate >(std::forward<Args>(args)...);
                }
                case remill::Operand::Type::kTypeAddress : {
                    return Operand::make<Address>(std::forward<Args>(args)...);
                }
                default :
                    unreachable()
                        << "Cannot replace shadow operand with"
                        << "type that is neither reg, addr nor imm.";
            }
        }


        template< typename CB >
        auto visit(CB &&cb) { return std::visit(std::forward< CB >(cb), _data); }

        template< typename CB >
        auto visit(CB &&cb) const { return std::visit(std::forward< CB >(cb), _data); }

        template< typename CB >
        void for_each_present(CB &cb) const
        {
            return visit([&](const auto &x) { return x.for_each_present(cb); });
        }

        bool present(std::size_t idx) const
        {
            bool out = false;
            auto collect = [&](auto &op) { out |= op.present(idx); };
            visit(collect);
            return out;
        }

        bool is_husk() const { return empty(); }

        bool empty() const
        {
            auto is_empty = [](const auto &v) { return v.empty(); };
            return visit(is_empty);
        }

        std::string to_string(std::size_t indent = 0) const
        {
            auto fmt = [&](auto &op) { return op.to_string(indent); };
            return visit(fmt);
        }
    };

    static inline std::ostream &operator<<(std::ostream &os, const Operand &op)
    {
        return os << op.to_string();
    }

    struct Instruction
    {
        // We need to fullfil that is pointers to operands are never invalidated!
        // TODO(lukas): See if ^ cannot be relaxed, as it is a propbable source of errors.
        std::deque< Operand > operands;

        // Some instructions can depend on the same parts
        // of encoding - we want to keep track of those
        using operand_ctx_t = std::tuple< std::size_t, Operand * >;
        std::vector< std::vector< operand_ctx_t > > deps;
        std::vector< std::set< uint32_t > > dirt;

        std::vector< Reg * > selectors;

        Instruction() = default;
        Instruction(const Instruction &other) : operands(other.operands)
        {
            for (const auto &o_cluster : other.deps)
            {
                std::vector< operand_ctx_t > cluster;
                for (const auto &[idx, _] : o_cluster)
                    cluster.emplace_back(idx, &operands[idx]);
                deps.push_back(std::move(cluster));
            }
        }

        Instruction(Instruction &&) = default;

        Instruction &operator=(const Instruction&) = delete;
        Instruction &operator=(Instruction &&) = default;

        auto size() const { return operands.size(); }
        const auto &operator[](std::size_t idx) const { return operands[idx]; }
        auto &operator[](std::size_t idx) { return operands[idx]; }

        // Will be passed `const &` of all present *leaves*.
        template< typename CB >
        void for_each_present(CB &cb) const
        {
            for (const auto &op : operands)
                op.for_each_present(cb);
        }

        // Cannot have templated code in function local `struct` defnitions.
        struct assign_
        {
            Instruction &self;
            void operator()(Reg &what)
            {
                check(!what.selector);
                what.selector = self.selectors.size();
                self.selectors.push_back(&what);
            }
            void operator()(Address &addr)
            {
                addr.for_each_present(*this);
            }
            // Fallthrough
            void operator()(auto &&) {}
        };

        void distribute_selectors()
        {
            assign_ assign{ *this };
            for (auto &op : operands)
                op.visit(assign);

        }

        bool operator==(const Instruction &o) const
        {
            if (o.operands != operands)
                return false;

            auto cmp_op_ctx = [](const operand_ctx_t &ctx, const operand_ctx_t &octx) {
                return std::get< 0 >(ctx) == std::get< 0 >(octx);
            };

            auto cmp_deps = [&](const auto &cluster, const auto &ocluster) {
                return std::equal(cluster.begin(), cluster.end(), ocluster.begin(), cmp_op_ctx);
            };

            return std::equal(deps.begin(), deps.end(), o.deps.begin(), cmp_deps);
        }

        bool present(std::size_t idx) const
        {
            for (auto &op : operands)
                if (op.present(idx))
                    return true;

            return false;
        }

        bool validate()
        {
            uint32_t dirty_count = 0;
            for (const auto &op : operands)
              if (op.reg() && !op.reg()->dirty.empty())
                ++dirty_count;

            return dirty_count <= 1;
        }

        template< typename T, typename ...Args >
        auto &Add(Args && ... args)
        {
            return operands.emplace_back(Operand::make<T>(std::forward<Args>(args)...));
        }

        template< typename ...Args >
        auto &Replace(std::size_t idx, remill::Operand::Type type, Args && ...args)
        {
            operands[idx] = Operand::make(type, std::forward< Args >(args)...);
            return operands[idx];
        }

        region_t IdentifiedRegions() const
        {
            region_t out;
            auto collect = [&](const has_regions &x)
            {
                for (const auto &[from, size] : x.regions)
                {
                    dcheck(!out.count(from) || out[from] == size, [](){
                        return "Inconsistent regions."; }
                    );
                    out[from] = size;
                }
            };

            for_each_present(collect);
            return out;
        }

        // We need lenght of the entire region to be able to calculate last region
        region_t UnknownRegions(uint64_t lenght) const
        {
            return Invert(IdentifiedRegions(), lenght);
        }

        std::string to_string() const
        {
            std::stringstream ss;
            ss << "Shadowinst:" << std::endl;

            for (const auto &cluster : deps) {
                ss << "Deps cluster: [ ";
                for (const auto &[idx, _] : cluster)
                    ss << idx << " ";
                ss << "]" << std::endl;
            }

            for (const auto &dirts : dirt)
            {
                ss << "dirt( ";
                for (auto d : dirts)
                    ss << d << " ";
                ss << ")" << std::endl;
            }

            for (std::size_t i = 0; i < operands.size(); ++i)
            {
                ss << "OP: " << i << ": ";
                ss << operands[i].to_string();
            }
            ss << "(print done)" << std::endl;
            return ss.str();
        }
    };

    static inline std::string remove_shadowed(const Instruction &s_inst,
                                              const std::string &bytes)
    {
        std::string out;
        for (std::size_t i = 0; i < bytes.size(); ++i)
            if (s_inst.present(i))
                out += bytes[bytes.size() - 1 - i];
        return out;
    }

} // namespace circ::shadowinst
