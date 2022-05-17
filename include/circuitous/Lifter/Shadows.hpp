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
#include <type_traits>
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

    // This describes a region of decoded bytes where
    // it makes sense to talk about an "entity" -- register, immediate operand,
    // etc. While it will usually be one contignous entry, we don't need to constraint
    // ourselves yet.
    // {from, size}
    using region_t = std::map<uint64_t, uint64_t>;
    using maybe_region_t = std::optional<region_t>;

    region_t Invert(region_t what, uint64_t lenght);
    region_t FromToFormat(const region_t &region);

    using bits_t = std::vector< bool >;

    struct ordered_bits_t
    {
        bits_t data;
        std::size_t size;

        explicit ordered_bits_t(const bits_t &data_, std::size_t size_)
            : size(size_)
        {
            data.reserve(data_.size());
            data.insert(data.end(), data_.begin(), data_.end());

            for (std::size_t i = size; i < data.size(); ++i)
                dcheck(!data[i], []() { return "Trying to cut non-zero bit."; });

            data.resize(size_);
        }
    };

    struct answers_queries
    {
        bool operator==(const answers_queries &) const = default;
    };

    template< typename B >
    struct Regions_ : answers_queries
    {
        using self_t = Regions_< B >;
        using bit_t = B;
        using storage_t = std::vector< B >;

        storage_t bits;
        std::map< std::size_t, std::size_t > areas;

        Regions_(std::size_t init) : bits(init, B{ false }) {}
        Regions_(const ordered_bits_t &bits_) : bits(bits_.data)
        {
            for (std::size_t i = 0; i < bits.size(); ++i)
            {
                if (!bits[i])
                    continue;

                uint64_t offset = i;
                uint32_t count = 0;
                for(; i < bits.size() && bits[i]; ++i)
                    ++count;

                areas.emplace(offset, count);
            }
        }

        void clear()
        {
            bits.clear();
            areas.clear();
        }

        std::size_t marked_size() const
        {
            std::size_t acc = 0;
            for (auto &[from, size] : areas)
                acc += size;
            return acc;
        }

        std::size_t total_size() const
        {
            return bits.size();
        }
        std::vector< std::size_t > get_marked_idxs()
        {
            std::vector< std::size_t > out;
            for (std::size_t i = 0; i < bits.size(); ++i)
                if (bits[i])
                    out.push_back(i);
            return out;
        }

        bool is_marked(std::size_t i) const { return bits[i]; }
        bool empty() const { return areas.empty(); }

        std::string to_string(std::size_t indent=0) const
        {
            std::stringstream ss;
            for (auto [from, size] : areas)
                ss << std::string(indent * 4, ' ') << "[ "
                   << from << " , " << size
                   << " ]" << std::endl;

            return ss.str();
        }

        auto biggest_chunk() const
        {
            std::tuple< uint64_t, uint64_t > out{ 0, 0 };
            for (auto &[from, size] : areas)
                if (size > std::get< 1 >(out))
                    out = {from, size};

            return out;
        }

        std::vector< std::size_t > get_holes() const
        {
            std::vector< std::size_t > holes;
            if (total_size() <= 2)
                return holes;
            for (std::size_t i = 1; i < bits.size() - 1; ++i)
            {
                if (!bits[i] && bits[i - 1] && bits[i + 1])
                    holes.push_back(i);
            }
            return holes;
        }

        using interval = std::tuple< std::size_t, std::size_t >;

        self_t &mark_area(const interval &vs)
        {
            auto [x, y] = vs;
            areas.emplace(x, y);
            return *this;
        }

        self_t &mark_bits(const interval &vs)
        {
            const auto &[from, size] = vs;
            for (std::size_t i = from; i - from < size; ++i)
                bits[i] = true;
            return *this;
        }

        void mark(const interval &vs)
        {
            mark_area(vs).mark_bits(vs);
        }

        bool is_applicable(const interval &vs) const
        {
            const auto &[from, size] = vs;
            for (std::size_t i = from; i - from < size; ++i)
                if (bits[i])
                    return false;
            return true;
        }

        bool add(const interval &vs)
        {
            if (!is_applicable(vs))
                return false;
            mark(vs);
            return true;
        }

        std::optional< std::size_t > get_enclosing_area(std::size_t idx) const
        {
            if (idx >= bits.size() || !bits[idx])
                return {};

            std::size_t i = idx;
            while (i > 0 && !bits[idx])
                --i;

            if (i != 0)
                return { i };
            if (bits[idx])
                return {};

            return { i };
        }

        bool empty_interval_of_size(std::size_t a, std::size_t b, std::size_t d) const
        {
            check(a != b);

            if ( a > b ) std::swap(a, b);
            if ( b - a != d )
                return false;

            ++a;
            while (a != b)
            {
                if (bits[a])
                    return false;
                ++a;
            }

            return true;
        }

        bool extend_left(std::size_t from)
        {
            // There is no space on the left
            if (from == 0 || bits[from - 1] || !bits[from])
                return false;

            bits[from - 1] = true;
            auto new_size = areas[from] + 1;
            areas.erase(from);
            areas.emplace(from - 1, new_size);
            return true;
        }

        bool extend_right(std::size_t from)
        {
            auto size = areas[from];
            auto next = from + size;
            if (next == bits.size() - 1 || bits[next] || !bits[from])
                return false;
            bits[next] = true;
            areas[from] += 1;

            return true;
        }

        bool merge_areas(std::size_t a, std::size_t b, std::size_t distance)
        {
            if ( a > b )
                std::swap(a, b);

            auto ax = get_enclosing_area(a);
            auto bx = get_enclosing_area(b);
            if (!ax || !bx)
                return false;

            if (!empty_interval_of_size(*ax + areas[*ax], *bx, distance))
                return false;

            auto end = *bx + areas[*bx];
            areas.erase(*bx);
            areas[*ax] = end - *ax - 1;
            return true;
        }

        bool operator==(const Regions_ &) const = default;
    };


    struct accepts_queries
    {
        bool operator==(const accepts_queries &) const = default;
    };

    namespace query
    {
        template< typename T >
        concept container_like = requires (T c)
        {
            c.begin();
            c.end();
        };

        template< typename Q, typename V >
        concept invocable = requires(Q &q, const V &v)
        {
            q(v);
        };

        template< typename Q, typename V >
        concept indirectly_invocable = requires(Q &q, const V &v)
        {
            q(*v);
        };

        template< typename V >
        concept accepts_queries_t = std::is_base_of_v< accepts_queries, V >;

        // TODO(lukas): Otherwise it dies in the depths of template instantiations *inside*
        //              a concept. No idea, try with later clang.
        template< typename V >
        concept answers_queries_t = std::is_base_of_v< answers_queries, V >;

        /* dispatch */

        template< typename Q, container_like C >
        auto do_apply(Q &q, const C &c) -> decltype(do_apply(q, *c.begin()))
        {
            // TODO(lukas): Return type with optional would be complicated for now not neeeded.
            check(c.begin() != c.end());
            auto it = c.begin();
            auto r = do_apply(q, *it);
            ++it;
            while (it != c.end())
            {
                r = q(do_apply(q, (*it)), r);
                ++it;
            }
            return r;
        }

        template< typename Q, answers_queries_t V > requires(invocable< Q, V >)
        auto do_apply(Q &q, const V &v) -> decltype (q(v))
        {
            return q(v);
        }

        template< typename Q, answers_queries_t V >
        auto do_apply(Q &q, const std::optional< V > &v) -> decltype(q(*v))
        {
            if (!v)
                return {};
            return q(*v);
        }

        template< typename Q, accepts_queries_t V >
        auto do_apply(Q &q, const std::optional< V > &v) -> decltype(v->query(q))
        {
            if (v)
                return v->query(q);
            return {};
        }


        template< typename Q, accepts_queries_t V >
        auto do_apply(Q &q, const V &v) -> decltype(v.query(q))
        {
            return v.query(q);
        }

        /* top-level apply */

        template< typename Q, typename Head, typename ... Args >
        auto apply(Q &&q, Head &&head, Args && ...args)
        -> decltype(do_apply(q, std::forward< Head >(head)))
        {
            auto r = do_apply(q, std::forward< Head >(head));
            if constexpr (sizeof ... (Args) == 0)
                return r;
            else
            {
                // NOTE(lukas): If there is not `query::apply` it will try to match
                //              `std::apply` and error is not going to make sense.
                auto tail = query::apply(std::forward< Q >(q), std::forward< Args >(args) ...);
                return q(std::move(r), std::move(tail));
            }
        }

        /* combinators */

        template< typename C, typename V >
        std::optional< V > combine_r(C &&c, std::optional< V > lhs, std::optional< V > rhs)
        {
            if (!rhs) return lhs;
            if (!lhs) return rhs;
            return std::make_optional( c(*lhs, *rhs) );
        }

        template< class ... Ts > struct overloaded : Ts ... { using Ts::operator() ...; };
        template< class ... Ts > overloaded(Ts ...) -> overloaded< Ts ... >;

        static inline auto and_combine()
        {
            return []< typename T >(T rhs, T lhs) {
                return combine_r(std::logical_and(), std::move(rhs), std::move(lhs));
            };
        }

        static inline auto or_combine()
        {
            return []< typename T >(T rhs, T lhs) {
                return combine_r(std::logical_or(), std::move(rhs), std::move(lhs));
            };
        }

        template< typename C >
        auto get_combinator(C &&c_)
        {
            return [c = std::forward< C >(c_)](auto r, auto l) {
                if (!r) return l;
                if (!l) return r;
                return c(std::move(r), std::move(l));
            };

        }

        static inline auto is_marked(std::size_t idx)
        {
            return overloaded
            {
                [=]< answers_queries_t V >(const V &region) {
                    return std::make_optional(region.is_marked(idx));
                },
                or_combine()
            };
        }

        static inline auto is_empty()
        {
            return overloaded
            {
                []< answers_queries_t V >(const V &region) {
                    return std::make_optional(region.empty());
                },
                and_combine()
            };
        }

    } // namespace query


    struct has_regions : accepts_queries
    {
        using Regions = Regions_< bool >;
        Regions regions;

        has_regions(std::size_t bitsize) : regions(bitsize) {}
        has_regions(const ordered_bits_t &bits_) : regions(bits_) {}
        has_regions(const Regions &regions_) : regions(regions_) {}

        template< typename Q >
        auto query(Q &&q) const
        {
            return query::apply(std::forward< Q >(std::forward< Q >(q)), regions);
        }

        bool empty() const
        {
            auto x = this->query(query::is_empty());
            dcheck(x, []() { return "Optional does not have value."; });
            return *x;
        }

        template< typename ... Ts >
        std::string to_string(Ts && ... ts) const
        {
            return regions.to_string(std::forward< Ts >(ts) ...);
        }

        bool operator==(const has_regions &) const = default;
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

            if (this->empty())
            {
                ss << make_indent(indent * 4) << "( empty )" << std::endl;
                return ss.str();
            }

            for (const auto &[k, vs] : *this)
            {
                auto subindent = make_indent(indent * 4 + k.size());
                if (vs.empty())
                {
                    ss << "( empty )";
                    break;
                }

                bool fst = true;
                for (const auto &v : vs)
                {
                    if (fst)
                    {
                        ss << make_indent(indent * 4) << k;
                        fst = false;
                    } else {
                        ss << subindent;
                    }
                    ss << " -> " << make_bitstring(v) << std::endl;
                }
            }
            ss << make_indent(indent * 4) << "( mappings: " << mappings_count()
                                          << ", materializations: " << mats_count()
                                          << " of maximum " << max_mats_count()
                                          << " => "
                                          << (is_saturated() ? "saturated" : "not saturated")
                                          << " )" << std::endl;
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

        bool operator==(const TranslationMap &) const = default;
    };

    template< typename K, typename V >
    std::ostream &operator<<(std::ostream &os, const TranslationMap< K, V > &tm)
    {
        return os << tm.to_string();
    }

    struct Reg : has_regions
    {
        using self_t = Reg;

        // Actual instances for given reg
        using materialized_t = std::vector< bool >;
        using reg_t = std::string;
        using TM_t = TranslationMap< reg_t, materialized_t >;

        using has_regions::has_regions;
        using has_regions::query;
        using has_regions::empty;

        TM_t translation_map;
        std::unordered_set< reg_t > dirty;

        std::optional< std::size_t > selector;

        Reg(const ordered_bits_t &bits_)
            : has_regions(bits_), translation_map(regions.marked_size())
        {}

        Reg(const Regions &o)
            : has_regions(o), translation_map(o.marked_size())
        {}

        auto &tm() { return translation_map; }
        const auto &tm() const { return translation_map; }

        std::string to_string(std::size_t indent=0, bool print_header=true) const;

        // REFACTOR(lukas): Remove.
        bool is_dirty(const reg_t &reg) const { return dirty.count(reg); }

        void mark_dirty(const reg_t &reg)
        {
            check(translation_map.count(reg));
            dirty.insert(reg);
        }
        void for_each_present(auto &cb) const { cb(*this); }

        self_t &clear()
        {
            // TODO(lukas): What about semantics of `selector` and `dirty`?
            translation_map.clear();
            regions.clear();
            return *this;
        }

        bool operator==(const Reg &) const = default;
    };

    struct Immediate : has_regions
    {
        using has_regions::has_regions;

        using has_regions::query;

        void for_each_present(auto &cb) const { cb(*this); }
        std::string to_string(std::size_t indent=0, bool print_header=true) const;
    };

    struct Address : accepts_queries
    {
        using Regions = typename has_regions::Regions;
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

        template< typename Q >
        auto query(Q &&q) const
        {
            return query::apply(std::forward< Q >(q), _regs, _imms);
        }

        Regions flatten_significant_regs() const;
        //bool is_marked(std::size_t idx) const;
        std::string to_string(std::size_t indent) const;
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
        using has_regions::query;
        void for_each_present(auto &cb) const { cb(*this); }
        std::string to_string(std::size_t) const { not_implemented(); }
    };

    struct Operand : accepts_queries
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

        //bool operator==(const Operand &) const = default;

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

        template< typename Q >
        auto query(Q &q) const
        {
            auto fwd = [&](const auto &x) { return x.query(q); };
            return visit(fwd);
        }

        bool is_husk() const
        {
            auto empty = [&](const auto &x) { return x.query(query::is_empty()); };
            return *visit(empty);
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
        std::size_t enc_bitsize;

        Instruction(std::size_t enc_bitsize_) : enc_bitsize(enc_bitsize_) {}
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

        void distribute_selectors();
        bool operator==(const Instruction &o) const;

        bool present(std::size_t idx) const
        {
            if (auto x = query::apply(query::is_marked(idx), operands))
                return *x;
            return false;
        }

        // REFACTOR(lukas): Remove when removing whole `dirty` mechanism.
        bool validate() const;

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

        region_t IdentifiedRegions() const;

        // We need lenght of the entire region to be able to calculate last region
        region_t UnknownRegions(uint64_t lenght) const
        {
            return Invert(IdentifiedRegions(), lenght);
        }

        std::string to_string() const;

    };

    std::string remove_shadowed(const Instruction &s_inst, const std::string &bytes);

} // namespace circ::shadowinst
